# apps/core/lib/core/recall/synonyms.ex
defmodule Core.Recall.Synonyms do
  @moduledoc """
  Synonym harvesting & expansion for the recall pipeline.

  Input:
    • `candidates` — list of `{key, tok_ids, prio}` produced by Execute.
    • `opts` — functions & limits (exists/neg cache/limit/budget/etc).

  Output:
    • `{added_refs, taken_count, budget_hit?}` where `added_refs` are recall refs:
      %{
        id: {:db_synonym, src_key, syn},
        matched_tokens: [%{tok_id: integer(), conf: nil}],
        activation_snapshot: 0.0,
        source: :db_recall,
        reason: :synonym,
        score: float(),
        ts_ms: integer()
      }
  """

  alias Core.Recall.Synonyms.{Source, DbSource}

  @type candidate :: {binary(), [non_neg_integer()], 0 | 1}

  @default_score 0.65
  @default_reason :synonym
  @default_limit 8
  @default_budget_ms 40

  @doc """
  Expand `candidates` via a synonym source under slot/time budgets.

  Options:
    • :exists_fun       — (binary -> bool), target existence check (defaults false)
    • :neg_exists_fun   — (binary -> bool), negative cache probe
    • :neg_put_fun      — (binary -> :ok), negative cache insert
    • :limit            — max refs to add (default #{@default_limit})
    • :budget_ms        — time budget from `t0` (default #{@default_budget_ms})
    • :t0               — ms clock start (default: now)
    • :already_ids      — MapSet of ids to avoid duplicates
    • :score            — score assigned to synonym refs (default #{@default_score})
    • :reason           — reason atom (default #{@default_reason})
    • :source           — module implementing #{inspect(Source)} (default #{inspect(DbSource)})
  """
  @spec run([candidate()], keyword()) :: {[map()], non_neg_integer(), boolean()}
  def run(candidates, opts \\ []) when is_list(candidates) do
    exists_fun = Keyword.get(opts, :exists_fun, fn _ -> false end)
    neg_exists = Keyword.get(opts, :neg_exists_fun, fn _ -> false end)
    neg_put    = Keyword.get(opts, :neg_put_fun, fn _ -> :ok end)

    limit      = Keyword.get(opts, :limit, @default_limit)
    budget_ms  = Keyword.get(opts, :budget_ms, @default_budget_ms)
    t0         = Keyword.get(opts, :t0, now_ms())
    already    = Keyword.get(opts, :already_ids, MapSet.new())

    base_score = Keyword.get(opts, :score, @default_score)
    reason     = Keyword.get(opts, :reason, @default_reason)
    source_mod = Keyword.get(opts, :source, DbSource)

    keys =
      candidates
      |> Enum.map(fn {k, _ids, _p} -> k end)
      |> Enum.filter(&is_binary/1)
      |> Enum.map(&normalize/1)
      |> Enum.reject(&(&1 in [nil, ""]))
      |> Enum.uniq()

    grouped_rows =
      if function_exported?(source_mod, :fetch_grouped, 1) do
        safe(fn -> source_mod.fetch_grouped(keys) end, %{})
      else
        %{}
      end

    syn_map = harvest_synonyms(grouped_rows)

    do_expand(candidates, syn_map, exists_fun, neg_exists, neg_put,
      limit, budget_ms, t0, already, base_score, reason, [])
  end

  # ---------- harvesting ----------

  defp harvest_synonyms(%{} = grouped) do
    grouped
    |> Enum.into(%{}, fn {k, rows} ->
      syns =
        rows
        |> List.wrap()
        |> Enum.flat_map(&extract_syns/1)
        |> Enum.map(&normalize/1)
        |> Enum.reject(&(&1 in [nil, ""]))
        |> MapSet.new()

      {normalize(k), syns}
    end)
  end

  defp harvest_synonyms(_), do: %{}

  defp extract_syns(%{} = row) do
    (Map.get(row, :synonyms) || Map.get(row, "synonyms") ||
       Map.get(row, :syns)    || Map.get(row, "syns") || [])
    |> List.wrap()
    |> Enum.flat_map(fn
      s when is_binary(s) -> [s]
      %{} = m ->
        # tolerate shapes like %{lemma: "..."} or %{word: "..."}
        v = Map.get(m, :lemma) || Map.get(m, "lemma") ||
            Map.get(m, :word)  || Map.get(m, "word")
        if is_binary(v), do: [v], else: []
      _ -> []
    end)
  end

  # ---------- expansion ----------

  defp do_expand([], _syn_map, _exists, _neg_exists, _neg_put,
                 _limit, _budget, _t0, _already, _score, _reason, acc),
    do: {Enum.reverse(acc), 0, false}

  defp do_expand([{_k,_ids,_p} | _]=_rest, _syn_map, _exists, _neg_exists, _neg_put,
                 limit, _budget, _t0, _already, _score, _reason, acc)
       when is_integer(limit) and limit <= 0,
    do: {Enum.reverse(acc), 0, false}

  defp do_expand([{key, tok_ids, _prio} | rest], syn_map, exists_fun, neg_exists, neg_put,
                 limit, budget_ms, t0, already, base_score, reason, acc) do
    elapsed = now_ms() - t0

    cond do
      budget_ms != :infinity and elapsed >= budget_ms ->
        {Enum.reverse(acc), 0, true}

      true ->
        src = normalize(key)
        syns = Map.get(syn_map, src, MapSet.new())

        {acc2, taken, bhit} =
          expand_for_key(src, tok_ids, syns, exists_fun, neg_exists, neg_put,
            limit, budget_ms, t0, already, base_score, reason, acc, 0)

        if bhit do
          {Enum.reverse(acc2), taken, true}
        else
          {tail_refs, tail_taken, tail_bhit} =
            do_expand(rest, syn_map, exists_fun, neg_exists, neg_put,
              dec(limit, taken), budget_ms, t0, union_ids(already, acc2), base_score, reason, acc2)

          {tail_refs, taken + tail_taken, tail_bhit}
        end
    end
  end

  defp expand_for_key(_src, _tok_ids, syns, _exists_fun, _neg_exists, _neg_put,
                      limit, _budget, _t0, _already, _score, _reason, acc, taken)
       when MapSet.size(syns) == 0 or (is_integer(limit) and limit <= 0),
    do: {acc, taken, false}

  defp expand_for_key(src, tok_ids, syns, exists_fun, neg_exists, neg_put,
                      limit, budget_ms, t0, already, base_score, reason, acc, taken) do
    Enum.reduce_while(MapSet.to_list(syns), {acc, taken, false}, fn syn, {acc0, t0count, _} ->
      elapsed = now_ms() - t0
      if budget_ms != :infinity and elapsed >= budget_ms do
        {:halt, {acc0, t0count, true}}
      else
        cond do
          syn == src ->
            {:cont, {acc0, t0count, false}}

          neg_exists.(syn) ->
            {:cont, {acc0, t0count, false}}

          exists_fun.(syn) ->
            ref = build_ref(src, syn, tok_ids, base_score, reason)
            if MapSet.member?(already, ref.id) do
              {:cont, {acc0, t0count, false}}
            else
              next_limit = dec(limit, 1)
              if is_integer(next_limit) and next_limit < 0 do
                {:halt, {acc0, t0count, false}}
              else
                {:cont, {[ref | acc0], t0count + 1, false}}
              end
            end

          true ->
            _ = safe_neg_put(neg_put, syn)
            {:cont, {acc0, t0count, false}}
        end
      end
    end)
  end

  # ---------- ref building & helpers ----------

  defp build_ref(src_key, synonym, tok_ids, score, reason) do
    %{
      id: {:db_synonym, src_key, synonym},
      matched_tokens: Enum.map(List.wrap(tok_ids), &%{tok_id: &1, conf: nil}),
      activation_snapshot: 0.0,
      source: :db_recall,
      reason: reason,
      score: score,
      ts_ms: now_ms()
    }
  end

  defp union_ids(already, refs) do
    Enum.reduce(refs, already, fn r, acc -> MapSet.put(acc, r.id) end)
  end

  defp dec(:infinity, _), do: :infinity
  defp dec(n, by) when is_integer(n), do: n - by

  defp normalize(nil), do: nil
  defp normalize(b) when is_binary(b), do: b |> String.downcase() |> String.trim()
  defp normalize(v), do: v |> to_string() |> String.downcase() |> String.trim()

  defp safe_neg_put(fun, key) when is_function(fun, 1) do
    try do
      fun.(key)
    rescue
      _ -> :ok
    catch
      _, _ -> :ok
    end
  end

  defp now_ms() do
    try do
      System.monotonic_time(:millisecond)
    rescue
      _ -> System.system_time(:millisecond)
    end
  end
end

