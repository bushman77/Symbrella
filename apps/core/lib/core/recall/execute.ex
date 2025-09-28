defmodule Core.Recall.Execute do
  @moduledoc """
  Execute a recall plan against long-term memory (LTM/DB).
  """

  alias Core.SemanticInput, as: SI
  alias Core.Recall.Plan

  @type exists_fun :: (binary() -> boolean())
  @type neg_exists_fun :: (binary() -> boolean())
  @type neg_put_fun :: (binary() -> :ok)

  @max_tok_id 9_223_372_036_854_775_807

  @spec execute(SI.t(), Plan.t(), keyword()) :: SI.t()
  def execute(%SI{} = si, %Plan{} = plan, opts \\ []) do
    t0 = now_ms()
    exists_fun = Keyword.get(opts, :exists_fun, default_exists_fun())
    neg_exists = Keyword.get(opts, :neg_exists_fun, default_neg_exists_fun())
    neg_put = Keyword.get(opts, :neg_put_fun, default_neg_put_fun())
    budget_ms = plan.budget_ms
    max_items = plan.max_items

    {candidates, covered_tok_ids} = candidate_keys(si)

    candidates2 =
      Enum.reject(candidates, fn {_key, tok_ids, _prio} ->
        Enum.any?(tok_ids, &(&1 in covered_tok_ids))
      end)

    {added_refs, counts, budget_hit?} =
      run_strategies(
        candidates2,
        plan.strategies,
        exists_fun,
        neg_exists,
        neg_put,
        max_items,
        budget_ms,
        t0,
        []
      )

    merge_and_trace(si, added_refs, counts, budget_ms, budget_hit?, t0)
  end

  # ---------- candidate key extraction ----------

  # Returns {candidates, covered_tok_ids}
  # candidates: [{key :: binary, tok_ids :: [non_neg_integer], prio :: 0 | 1}]
  defp candidate_keys(%SI{tokens: tokens} = si) do
    indexed = Enum.with_index(tokens)

    cands =
      indexed
      |> Enum.flat_map(fn {tok, idx} ->
        case tok do
          %Core.Token{phrase: phrase, mw: true} ->
            [{normalize(phrase), [idx], 0}]

          %Core.Token{phrase: phrase} ->
            [{normalize(phrase), [idx], 1}]

          %{} = m ->
            key =
              Map.get(m, :norm) ||
                normalize(Map.get(m, :text) || Map.get(m, :phrase) || "")

            prio = if Map.get(m, :is_mwe_head, false), do: 0, else: 1
            [{key, [Map.get(m, :id, idx)], prio}]
        end
      end)
      |> Enum.reject(fn {k, _, _} -> k in [nil, ""] end)

    merged =
      Enum.reduce(cands, %{}, fn {k, tok_ids, prio}, acc ->
        Map.update(acc, {k, prio}, MapSet.new(tok_ids), &MapSet.union(&1, MapSet.new(tok_ids)))
      end)

    list =
      merged
      |> Enum.map(fn {{k, prio}, set} -> {k, Enum.sort(MapSet.to_list(set)), prio} end)
      |> Enum.sort_by(fn {_k, tok_ids, prio} -> {prio, Enum.min(tok_ids)} end)

    covered = covered_tok_ids_from_active(si)
    {list, covered}
  end

  # Pull tok_ids already claimed by runtime/recency so we don't duplicate
  defp covered_tok_ids_from_active(%SI{active_cells: refs}) do
    refs
    |> Enum.flat_map(fn
      %{source: src, matched_tokens: mts} when src in [:runtime, :recency] ->
        Enum.map(mts, & &1.tok_id)

      _ ->
        []
    end)
    |> Enum.uniq()
  end

  defp covered_tok_ids_from_active(_), do: []

  # ---------- strategy runner ----------

  defp run_strategies(
         candidates,
         strategies,
         exists_fun,
         neg_exists,
         neg_put,
         max_items,
         budget_ms,
         t0,
         acc_refs
       ) do
    counts = %{exact: 0, synonym: 0, embedding: 0}

    # normalize limits to support :infinity
    limit =
      case max_items do
        :infinity -> :infinity
        n when is_integer(n) and n > 0 -> n
        _ -> 8
      end

    Enum.reduce_while(strategies, {acc_refs, counts, false}, fn strat, {refs, cnts, _bhit} ->
      elapsed = now_ms() - t0

      time_left =
        case budget_ms do
          :infinity -> :infinity
          n when is_integer(n) and n > 0 -> n - elapsed
          _ -> 40 - elapsed
        end

      cond do
        time_left != :infinity and time_left <= 0 ->
          {:halt, {refs, cnts, true}}

        limit != :infinity and length(refs) >= limit ->
          {:halt, {refs, cnts, false}}

        strat == :exact ->
          slots_left =
            case limit do
              :infinity -> :infinity
              n -> n - length(refs)
            end

          {added, new_cnts, bh} =
            exact_pass(candidates, exists_fun, neg_exists, neg_put, slots_left, time_left, t0)

          {:cont, {refs ++ added, bump(cnts, :exact, new_cnts.exact), bh}}

        strat == :synonym ->
          {:cont, {refs, cnts, false}}

        strat == :embedding ->
          {:cont, {refs, cnts, false}}

        true ->
          {:cont, {refs, cnts, false}}
      end
    end)
  end

  defp bump(cnts, key, add), do: Map.update!(cnts, key, &(&1 + add))

  defp exact_pass(candidates, exists_fun, neg_exists, neg_put, slots_left, time_left_ms, t0) do
    do_exact(candidates, exists_fun, neg_exists, neg_put, slots_left, time_left_ms, t0, [], 0)
  end

  defp do_exact(
         [],
         _exists_fun,
         _neg_exists,
         _neg_put,
         _slots_left,
         _time_left_ms,
         _t0,
         acc,
         cnt
       ),
       do: {Enum.reverse(acc), %{exact: cnt}, false}

  defp do_exact(
         [{_key, _tok_ids, _prio} | _] = _rest,
         _exists_fun,
         _neg_exists,
         _neg_put,
         slots_left,
         _tl,
         _t0,
         acc,
         cnt
       )
       when is_integer(slots_left) and slots_left <= 0,
       do: {Enum.reverse(acc), %{exact: cnt}, false}

  defp do_exact(
         [{key, tok_ids, _prio} | rest],
         exists_fun,
         neg_exists,
         neg_put,
         slots_left,
         time_left_ms,
         t0,
         acc,
         cnt
       ) do
    elapsed = now_ms() - t0

    if time_left_ms != :infinity and elapsed >= time_left_ms do
      {Enum.reverse(acc), %{exact: cnt}, true}
    else
      # Positives (DB existence) beat stale negcache
      cond do
        exists_fun.(key) ->
          ref = to_active_ref_exact(key, tok_ids)

          next_slots =
            case slots_left do
              :infinity -> :infinity
              n -> n - 1
            end

          do_exact(
            rest,
            exists_fun,
            neg_exists,
            neg_put,
            next_slots,
            time_left_ms,
            t0,
            [ref | acc],
            cnt + 1
          )

        neg_exists.(key) ->
          do_exact(rest, exists_fun, neg_exists, neg_put, slots_left, time_left_ms, t0, acc, cnt)

        true ->
          _ = safe_neg_put(neg_put, key)
          do_exact(rest, exists_fun, neg_exists, neg_put, slots_left, time_left_ms, t0, acc, cnt)
      end
    end
  end

  defp to_active_ref_exact(key, tok_ids) do
    %{
      id: {:db_word, key},
      matched_tokens: Enum.map(tok_ids, &%{tok_id: &1, conf: nil}),
      activation_snapshot: 0.0,
      source: :db_recall,
      reason: :exact,
      score: 1.0,
      ts_ms: now_ms()
    }
  end

  # ---------- merge & trace ----------

  defp merge_and_trace(%SI{} = si, added_refs, counts, budget_ms, budget_hit?, t0) do
    merged =
      (si.active_cells ++ added_refs)
      |> Enum.uniq_by(& &1.id)
      |> Enum.sort_by(fn r -> {-round(activation_of(r) * 1.0e6), min_tok_id(r)} end)

    ev = %{
      stage: :recall_exec,
      ts_ms: now_ms(),
      meta: %{
        counts: counts,
        latency_ms: max(now_ms() - t0, 0),
        budget_ms: budget_ms,
        budget_hit?: budget_hit?
      }
    }

    %SI{si | active_cells: merged, trace: si.trace ++ [ev]}
  end

  # tolerate structs, maps, strings, anything
  defp activation_of(%Db.BrainCell{activation: a}) when is_number(a), do: a
  defp activation_of(%{activation_snapshot: a}) when is_number(a), do: a
  defp activation_of(%{activation: a}) when is_number(a), do: a
  defp activation_of(_), do: 0.0

  # tolerate items with/without matched_tokens/token_id
  defp min_tok_id(%{matched_tokens: [%{tok_id: t0} | _]}), do: t0
  defp min_tok_id(%{matched_tokens: _mts}), do: 0
  defp min_tok_id(%{token_id: tid}) when is_integer(tid), do: tid
  defp min_tok_id(%Db.BrainCell{token_id: tid}) when is_integer(tid), do: tid
  defp min_tok_id(_), do: @max_tok_id

  # ---- injected defaults ----

  defp default_exists_fun() do
    if Code.ensure_loaded?(Db) and function_exported?(Db, :word_exists?, 1) do
      fn key ->
        try do
          Db.word_exists?(key)
        rescue
          _ -> false
        catch
          _, _ -> false
        end
      end
    else
      fn _ -> false end
    end
  end

  defp default_neg_exists_fun() do
    if Code.ensure_loaded?(Core.NegCache) and function_exported?(Core.NegCache, :exists?, 1) do
      &Core.NegCache.exists?/1
    else
      fn _ -> false end
    end
  end

  defp default_neg_put_fun() do
    if Code.ensure_loaded?(Core.NegCache) and function_exported?(Core.NegCache, :put, 1) do
      &Core.NegCache.put/1
    else
      fn _ -> :ok end
    end
  end

  defp safe_neg_put(fun, key) do
    try do
      fun.(key)
    rescue
      _ -> :ok
    catch
      _, _ -> :ok
    end
  end

  defp normalize(<<>>), do: <<>>
  defp normalize(bin) when is_binary(bin), do: String.downcase(String.trim(bin))

  defp now_ms() do
    try do
      System.monotonic_time(:millisecond)
    rescue
      _ -> System.system_time(:millisecond)
    end
  end
end
