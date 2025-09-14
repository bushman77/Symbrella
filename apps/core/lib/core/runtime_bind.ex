defmodule Core.RuntimeBind do
  @moduledoc """
  Step 3: bind Brain's *currently active* cells to SemanticInput.

  - DB-free, deterministic given same snapshot + tokens.
  - Accepts tokens either as `%Core.Token{}` or earlier map-shape.
  """

  alias Core.SemanticInput, as: SI

  @default_topk 16

  @spec bind(SI.t(), keyword()) :: SI.t()
  def bind(%SI{} = si, opts \\ []) do
    now = now_ms()
    topk = Keyword.get(opts, :topk, @default_topk)

    with {:ok, {state_ref, actives}} <- fetch_snapshot(opts),
         {token_keys_map, tok_count} <- build_token_keys_map(si.tokens),
         {matches, considered} <- match_actives(actives, token_keys_map),
         refs <- build_active_refs(matches, now),
         refs_sorted <- sort_and_cap(refs, topk),
         merged <- merge_active_cells(si.active_cells, refs_sorted) do
      put_trace(%SI{si | brain_state_ref: state_ref, active_cells: merged}, %{
        stage: :runtime_bind,
        meta: %{
          state_ref: state_ref,
          added: length(refs_sorted),
          considered: considered,
          token_count: tok_count,
          topk: topk,
          timeout?: false
        },
        ts_ms: now
      })
    else
      {:error, _reason} ->
        put_trace(si, %{
          stage: :runtime_bind,
          meta: %{
            state_ref: nil,
            added: 0,
            considered: 0,
            token_count: length(si.tokens),
            topk: topk,
            timeout?: true
          },
          ts_ms: now
        })
    end
  end

  # ---------- Snapshot fetch ----------

  defp fetch_snapshot(opts) do
    case Keyword.fetch(opts, :snapshot) do
      {:ok, {_, _} = snap} ->
        {:ok, snap}

      :error ->
        case Keyword.get(opts, :snapshot_fun) do
          fun when is_function(fun, 0) -> fun.()
          _ -> {:error, :no_snapshot}
        end
    end
  rescue
    _ -> {:error, :snapshot_error}
  end

  # ---------- Token key index (supports %Core.Token{} or map-shape) ----------

  # Returns {keys_map, token_count}
  # keys_map: match_key(binary) => [%{tok_id: non_neg_integer(), is_mwe_head: boolean()}]
  defp build_token_keys_map(tokens) do
    tokens
    |> Enum.with_index()
    |> Enum.reduce({%{}, 0}, fn {tok, idx}, {acc, n} ->
      case tok do
        %Core.Token{} = t ->
          # Current struct: use the normalized phrase; do NOT access non-existent :lemma
          norm = normalize(t.phrase)
          keys = [norm]
          {put_keys(acc, keys, idx, Map.get(t, :mw, false)), n + 1}

        %{} = m ->
          # Back-compat map tokens: may include :lemma and MWE metadata
          id = Map.get(m, :id, idx)
          norm = Map.get(m, :norm) || normalize(Map.get(m, :text) || Map.get(m, :phrase) || "")
          lemma = Map.get(m, :lemma)
          keys = [norm] |> maybe_cons(lemma) |> maybe_cons(mwe_phrase_from_map(m, tokens))
          is_head = m[:is_mwe_head] == true
          {put_keys(acc, keys, id, is_head), n + 1}
      end
    end)
  end

  defp normalize(<<>>), do: <<>>
  defp normalize(bin) when is_binary(bin), do: String.downcase(String.trim(bin))

  defp maybe_cons(list, nil), do: list
  defp maybe_cons(list, val), do: [val | list]

  # Try to reconstruct an MWE phrase for map-shaped tokens
  defp mwe_phrase_from_map(%{is_mwe_head: true, mwe_id: id}, all) when is_binary(id) do
    toks =
      all
      |> Enum.filter(&(is_map(&1) and Map.get(&1, :mwe_id) == id))
      |> Enum.sort_by(&Map.get(&1, :id, 0))

    case toks do
      [] -> nil
      _  -> toks |> Enum.map(&(Map.get(&1, :norm) || normalize(Map.get(&1, :text) || Map.get(&1, :phrase) || ""))) |> Enum.join(" ")
    end
  end

  defp mwe_phrase_from_map(_, _), do: nil

  defp put_keys(acc, keys, tok_id, is_head) do
    Enum.reduce(keys, acc, fn
      nil, a -> a
      "", a -> a
      key, a ->
        Map.update(a, key, [%{tok_id: tok_id, is_mwe_head: is_head}], fn lst ->
          [%{tok_id: tok_id, is_mwe_head: is_head} | lst]
        end)
    end)
  end

  # ---------- Matching ----------

  # Returns: {matches_map, considered_count}
  # matches_map: cell_id -> %{activation: float, tok_ids: MapSet.t()}
  defp match_actives(actives, token_keys_map) do
    considered = length(actives)

    matches =
      Enum.reduce(actives, %{}, fn %{id: id, keys: keys, activation: a}, acc ->
        hit_ids =
          keys
          |> Enum.flat_map(fn key -> Map.get(token_keys_map, normalize(key), []) end)
          |> Enum.map(& &1.tok_id)
          |> Enum.uniq()

        case hit_ids do
          [] -> acc
          ids ->
            Map.update(acc, id, %{activation: a, tok_ids: MapSet.new(ids)}, fn %{activation: a0, tok_ids: set} ->
              %{activation: max(a0, a), tok_ids: Enum.reduce(ids, set, &MapSet.put(&2, &1))}
            end)
        end
      end)

    {matches, considered}
  end

  # ---------- Build refs, order, cap, merge ----------

  defp build_active_refs(matches_map, now_ms) do
    Enum.map(matches_map, fn {id, %{activation: a, tok_ids: set}} ->
      %{
        id: id,
        matched_tokens:
          set
          |> Enum.to_list()
          |> Enum.sort()
          |> Enum.map(&%{tok_id: &1, conf: nil}),
        activation_snapshot: a,
        source: :runtime,
        reason: :matched_active_cell,
        score: nil,
        ts_ms: now_ms
      }
    end)
  end

  defp sort_and_cap(refs, topk) do
    refs
    |> Enum.sort_by(fn r -> {-(round(r.activation_snapshot * 1.0e6)), min_tok_id(r)} end)
    |> Enum.take(topk)
  end

  defp min_tok_id(%{matched_tokens: [%{tok_id: t0} | _]}), do: t0
  defp min_tok_id(%{matched_tokens: []}), do: 0

  defp merge_active_cells(existing, incoming) do
    by_id = Map.new(existing, &{&1.id, &1})

    merged =
      Enum.reduce(incoming, by_id, fn r, acc ->
        case Map.get(acc, r.id) do
          nil -> Map.put(acc, r.id, r)
          prev -> Map.put(acc, r.id, merge_refs(prev, r))
        end
      end)

    merged
    |> Map.values()
    |> Enum.sort_by(fn r -> {-(round(r.activation_snapshot * 1.0e6)), min_tok_id(r)} end)
  end

  defp merge_refs(prev, new) do
    merged_tokens =
      (prev.matched_tokens ++ new.matched_tokens)
      |> Enum.uniq_by(& &1.tok_id)
      |> Enum.sort_by(& &1.tok_id)

    %{
      prev
      | matched_tokens: merged_tokens,
        activation_snapshot: max(prev.activation_snapshot, new.activation_snapshot),
        # precedence: :runtime > :recency > :db_recall
        source: precedence(prev.source, new.source),
        reason: prev.reason
    }
  end

  defp precedence(:runtime, _), do: :runtime
  defp precedence(:recency, :db_recall), do: :recency
  defp precedence(_, right), do: right

  defp put_trace(%SI{} = si, ev), do: %SI{si | trace: si.trace ++ [ev]}

  defp now_ms() do
    try do
      System.monotonic_time(:millisecond)
    rescue
      _ -> System.system_time(:millisecond)
    end
  end
end

