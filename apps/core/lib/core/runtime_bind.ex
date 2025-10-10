defmodule Core.RuntimeBind do
  @moduledoc ~S"""
  Runtime binding: match current tokens (and MWEs) to a snapshot of known cells,
  producing `si.active_cells`, with tracing and soft-fail behavior.

  Merge precedence (deterministic):
    1) Source name rank (e.g., "only" > "runtime")
    2) Numeric `:source_priority` (higher wins)
    3) `activation_snapshot` (higher wins)
    4) Stable fallback to left (for determinism)
  """

  alias Core.SemanticInput

  @type snapshot_t ::
          map()
          | (binary() -> list())
          | (binary(), keyword() -> list())
          | {any(), list()}

  # -------- Configuration (ranks) ------------------------------------

  @source_name_rank %{
    "only" => 100,
    "gold" => 90,
    "user" => 85,
    "curated" => 80,
    "runtime" => 50,
    "mwe" => 45,
    "repo" => 40,
    "snapshot" => 40,
    "default" => 10
  }

  # -------- Public API -----------------------------------------------

  @spec bind(SemanticInput.t()) :: SemanticInput.t()
  def bind(%SemanticInput{} = si), do: bind(si, [])

  @spec bind(SemanticInput.t(), keyword()) :: SemanticInput.t()
  def bind(%SemanticInput{} = si, opts) do
    snapshot = Keyword.get(opts, :snapshot)
    topk = Keyword.get(opts, :topk, 16)

    # Soft-fail: if snapshot is missing, mark timeout? unless explicitly overridden
    timeout? = !!Keyword.get(opts, :timeout?, is_nil(snapshot))

    # downcased phrases (words + MWEs)
    phrases = phrases_from_tokens(si)
    # phrase -> [%{tok_id: ...}]
    hitmap = phrase_hitmap(si)

    # Collect new matches (count used for trace), but merge with existing cells before dedup.
    {matches, count, brain_ref} =
      case snapshot do
        nil ->
          {[], 0, Map.get(si, :brain_state_ref)}

        {ref, list} when is_list(list) ->
          m = collect_matches(phrases, {ref, list}, hitmap: hitmap)
          {m, length(m), ref}

        %{} = snap ->
          m = collect_matches(phrases, snap, hitmap: hitmap)
          {m, length(m), Map.get(snap, :ref, Map.get(si, :brain_state_ref))}

        fun when is_function(fun, 1) or is_function(fun, 2) ->
          m = collect_matches(phrases, fun, hitmap: hitmap)
          {m, length(m), Map.get(si, :brain_state_ref)}
      end

    # >>> IMPORTANT: merge existing active cells with new matches (existing first for stable left-bias)
    existing = List.wrap(si.active_cells || [])
    merged_input = existing ++ matches
    active_cells = dedup_and_cap(merged_input, topk)

    ev = %{
      stage: :runtime_bind,
      ts_ms: System.system_time(:millisecond),
      meta: %{count: count, timeout?: timeout?}
    }

    si2 = %SemanticInput{
      si
      | active_cells: active_cells,
        trace: [ev | si.trace || []]
    }

    case brain_ref do
      nil -> si2
      _ -> Map.put(si2, :brain_state_ref, brain_ref)
    end
  end

  # -------- Matching --------------------------------------------------

  # Snapshot as function (supports arity 1 or 2)
  defp collect_matches(phrases, snapshot_fun, opts) when is_function(snapshot_fun) do
    hm = Keyword.get(opts, :hitmap, Keyword.get(opts, :_hitmap, %{}))

    cands =
      case :erlang.fun_info(snapshot_fun, :arity) do
        {:arity, 2} -> Enum.flat_map(phrases, fn p -> List.wrap(snapshot_fun.(p, opts)) end)
        _ -> Enum.flat_map(phrases, fn p -> List.wrap(snapshot_fun.(p)) end)
      end

    cands
    |> Enum.filter(&valid_cell?/1)
    |> Enum.map(&annotate_cell(&1, hm))
  end

  # Snapshot as map (index) OR tuple {brain_state_ref, list_of_cells}
  defp collect_matches(phrases, snapshot, opts) do
    hm = Keyword.get(opts, :hitmap, Keyword.get(opts, :_hitmap, %{}))
    set = MapSet.new(phrases)

    case snapshot do
      # MAP SNAPSHOT: accept phrase-index maps AND a global list of cells
      %{} = snap ->
        idx =
          cond do
            is_map(snap[:index]) -> snap[:index]
            is_map(snap[:phrases]) -> snap[:phrases]
            is_map(snap[:cells]) -> snap[:cells]
            true -> %{}
          end

        global_list =
          cond do
            is_list(snap[:cells]) -> snap[:cells]
            is_list(snap[:all]) -> snap[:all]
            true -> []
          end

        phrase_hits =
          phrases
          |> Enum.flat_map(&List.wrap(Map.get(idx, &1, [])))

        (phrase_hits ++ global_list)
        |> Enum.filter(&valid_cell?/1)
        |> Enum.map(&annotate_cell(&1, hm))

      # TUPLE SNAPSHOT: {brain_state_ref, list_of_cells}, with sibling promotion
      {ref, list} when is_list(list) ->
        valid = Enum.filter(list, &valid_cell?/1)

        matched_by_keys =
          valid
          |> Enum.filter(fn cell ->
            keys = normalized_keys(cell)
            src = cell[:source] || cell["source"]
            only? = to_string(src) |> String.downcase() == "only"
            global = keys == []
            only? or global or Enum.any?(keys, &MapSet.member?(set, &1))
          end)

        matched_ids = matched_by_keys |> Enum.map(& &1.id) |> MapSet.new()

        siblings =
          valid
          |> Enum.filter(fn c -> MapSet.member?(matched_ids, c.id) end)

        (matched_by_keys ++ siblings)
        |> Enum.uniq_by(fn c -> {c.id, c[:source] || c["source"]} end)
        |> Enum.map(fn cell ->
          cell
          |> Map.put_new(:brain_state_ref, ref)
          |> annotate_cell(hm)
        end)

      _ ->
        []
    end
  end

  defp normalized_keys(cell) do
    cell
    |> Map.get(:keys, Map.get(cell, "keys", []))
    |> List.wrap()
    |> Enum.reject(&is_nil/1)
    |> Enum.map(&downcased/1)
  end

  defp annotate_cell(cell, hitmap) do
    keys =
      cell
      |> Map.get(:keys, Map.get(cell, "keys", []))
      |> List.wrap()
      |> Enum.map(&downcased/1)

    matches =
      keys
      |> Enum.flat_map(&Map.get(hitmap, &1, []))
      |> Enum.uniq_by(& &1.tok_id)
      # stable order for tests
      |> Enum.sort_by(& &1.tok_id)

    cell
    |> Map.put_new(:source, :runtime)
    |> Map.put_new(:matched_tokens, matches)
    |> Map.put_new(:reason, :matched_active_cell)
    |> put_activation_snapshot()
  end

  defp put_activation_snapshot(%{} = cell) do
    snap =
      cond do
        is_number(cell[:activation_snapshot]) -> cell[:activation_snapshot] * 1.0
        is_number(cell["activation_snapshot"]) -> cell["activation_snapshot"] * 1.0
        is_number(cell[:activation]) -> cell[:activation] * 1.0
        is_number(cell["activation"]) -> cell["activation"] * 1.0
        is_number(cell[:modulated_activation]) -> cell[:modulated_activation] * 1.0
        is_number(cell["modulated_activation"]) -> cell["modulated_activation"] * 1.0
        is_number(cell[:score]) -> cell[:score] * 1.0
        is_number(cell["score"]) -> cell["score"] * 1.0
        true -> nil
      end

    if is_number(snap), do: Map.put(cell, :activation_snapshot, snap), else: cell
  end

  # -------- Validity --------------------------------------------------

  defp valid_cell?(%{id: id}) when is_binary(id) or is_integer(id), do: true
  defp valid_cell?(_), do: false

  # -------- Phrase extraction (tokens + MWEs) -------------------------

  defp phrases_from_tokens(%SemanticInput{tokens: tokens}) do
    tokens = List.wrap(tokens)

    word_phrases =
      tokens
      |> Enum.map(&token_phrase/1)
      |> Enum.filter(&is_binary/1)
      |> Enum.map(&downcased/1)

    mwe_phrases =
      tokens
      |> Enum.filter(&is_mwe_head?/1)
      |> Enum.map(&mwe_phrase(&1, tokens))
      |> Enum.filter(&is_binary/1)
      |> Enum.map(&downcased/1)

    Enum.uniq(word_phrases ++ mwe_phrases)
  end

  defp phrase_hitmap(%SemanticInput{tokens: tokens}) do
    tokens = List.wrap(tokens)

    Enum.reduce(Enum.with_index(tokens), %{}, fn {t, idx}, acc ->
      # word token
      acc =
        case token_phrase(t) do
          nil ->
            acc

          p ->
            Map.update(acc, downcased(p), [%{tok_id: token_id(t, idx)}], fn lst ->
              lst ++ [%{tok_id: token_id(t, idx)}]
            end)
        end

      # mwe head -> expand to phrase + member token ids
      acc =
        if is_mwe_head?(t) do
          case mwe_phrase_and_ids(t, tokens, idx) do
            {nil, _ids} ->
              acc

            {phrase, ids} ->
              Map.update(acc, downcased(phrase), Enum.map(ids, &%{tok_id: &1}), fn lst ->
                Enum.uniq_by(lst ++ Enum.map(ids, &%{tok_id: &1}), & &1.tok_id)
              end)
          end
        else
          acc
        end

      acc
    end)
  end

  defp token_id(%_struct{id: id}, _idx) when is_integer(id), do: id

  defp token_id(%{} = t, _idx) do
    case Map.get(t, :id) || Map.get(t, "id") do
      i when is_integer(i) -> i
      _ -> nil
    end
  end

  defp token_id(_other, idx), do: idx

  defp token_phrase(%_struct{phrase: p}) when is_binary(p), do: p

  defp token_phrase(%{} = t) do
    cond do
      is_binary(Map.get(t, :phrase)) -> Map.get(t, :phrase)
      is_binary(Map.get(t, "phrase")) -> Map.get(t, "phrase")
      is_binary(Map.get(t, :norm)) -> Map.get(t, :norm)
      is_binary(Map.get(t, "norm")) -> Map.get(t, "norm")
      is_binary(Map.get(t, :text)) -> Map.get(t, :text)
      is_binary(Map.get(t, "text")) -> Map.get(t, "text")
      true -> nil
    end
  end

  defp token_phrase(_), do: nil

  defp is_mwe_head?(%_struct{is_mwe_head: true}), do: true
  defp is_mwe_head?(%{} = t), do: Map.get(t, :is_mwe_head) == true or Map.get(t, :mw) == true
  defp is_mwe_head?(_), do: false

  defp mwe_phrase(token, all_tokens) do
    case mwe_phrase_and_ids(token, all_tokens, nil) do
      {phrase, _ids} -> phrase
    end
  end

  # Returns {phrase, [tok_ids]}
  defp mwe_phrase_and_ids(token, all_tokens, idx_fallback) do
    case Map.get(token, :mwe_span) do
      %{} = mwe ->
        a = mwe[:first_id]
        b = mwe[:last_id]

        if is_integer(a) and is_integer(b) do
          phrase =
            all_tokens
            |> Enum.filter(fn t ->
              case Map.get(t, :id) || Map.get(t, "id") do
                id when is_integer(id) -> id >= a and id <= b
                _ -> false
              end
            end)
            |> Enum.map(&token_phrase/1)
            |> Enum.filter(&is_binary/1)
            |> Enum.join(" ")

          ids =
            all_tokens
            |> Enum.flat_map(fn t ->
              case Map.get(t, :id) || Map.get(t, "id") do
                id when is_integer(id) and id >= a and id <= b -> [id]
                _ -> []
              end
            end)

          {phrase, ids}
        else
          span_path(token, all_tokens, idx_fallback)
        end

      _ ->
        span_path(token, all_tokens, idx_fallback)
    end
  end

  # Handle span expressed as token index window {i, j} (j exclusive)
  defp span_path(token, all_tokens, idx_fallback) do
    case Map.get(token, :span) do
      {a, b} when is_integer(a) and is_integer(b) ->
        phrase =
          all_tokens
          |> Enum.with_index()
          |> Enum.filter(fn {_t, i} -> i >= a and i < b end)
          |> Enum.map(fn {t, _} -> token_phrase(t) end)
          |> Enum.filter(&is_binary/1)
          |> Enum.join(" ")

        ids =
          all_tokens
          |> Enum.with_index()
          |> Enum.flat_map(fn {t, i} ->
            if i >= a and i < b do
              case Map.get(t, :id) || Map.get(t, "id") do
                id when is_integer(id) -> [id]
                _ -> []
              end
            else
              []
            end
          end)

        {phrase, ids}

      _ ->
        {token_phrase(token), List.wrap(token_id(token, idx_fallback))}
    end
  end

  # -------- Dedup + TopK ---------------------------------------------

  defp dedup_and_cap(cells, topk) do
    cells
    |> Enum.reduce(%{}, fn cell, acc ->
      id = cell.id

      case acc do
        %{^id => existing} -> Map.put(acc, id, merge_cell(existing, cell))
        _ -> Map.put(acc, id, cell)
      end
    end)
    |> Map.values()
    |> Enum.sort_by(&sort_key/1, :desc)
    |> Enum.take(topk)
  end

  # Prefer :activation for ranking, then :score, then hash(id)
  defp sort_key(%{activation: a}) when is_number(a), do: a
  defp sort_key(%{"activation" => a}) when is_number(a), do: a
  defp sort_key(%{score: s}) when is_number(s), do: s
  defp sort_key(%{"score" => s}) when is_number(s), do: s
  defp sort_key(%{id: id}) when is_integer(id), do: id * 1.0
  defp sort_key(%{id: id}) when is_binary(id), do: :erlang.phash2(id) * 1.0
  defp sort_key(_), do: 0.0

  # -------- Merge precedence (refactored) -----------------------------

  # Name rank dominates; then numeric :source_priority; then snapshot; then stable lhs.
  defp merge_cell(a, b) do
    {na, pa} = source_rank(a)
    {nb, pb} = source_rank(b)

    preferred =
      cond do
        nb > na ->
          b

        na > nb ->
          a

        pb > pa ->
          b

        pa > pb ->
          a

        true ->
          sa = snapshot_of(a) || -1.0
          sb = snapshot_of(b) || -1.0
          if sb > sa, do: b, else: a
      end

    fallback = if preferred === a, do: b, else: a
    merged = Map.merge(fallback, preferred, fn _k, _v1, v2 -> v2 end)

    snap = snapshot_of(preferred) || snapshot_of(fallback)
    if is_number(snap), do: Map.put(merged, :activation_snapshot, snap), else: merged
  end

  # Rank tuple = {name_rank, numeric_priority}
  defp source_rank(cell), do: {source_name_rank(cell), source_numeric_priority(cell)}

  defp source_numeric_priority(%{source_priority: p}) when is_number(p), do: p * 1.0
  defp source_numeric_priority(%{"source_priority" => p}) when is_number(p), do: p * 1.0
  defp source_numeric_priority(_), do: 0.0

  defp source_name_rank(%{source: s}), do: source_name_rank(s)
  defp source_name_rank(%{"source" => s}), do: source_name_rank(s)

  defp source_name_rank(s) when is_atom(s),
    do: source_name_rank(Atom.to_string(s))

  defp source_name_rank(s) when is_binary(s),
    do: Map.get(@source_name_rank, String.downcase(s), @source_name_rank["default"])

  # Snapshots used as a tertiary tie-breaker
  defp snapshot_of(cell) do
    cond do
      is_number(cell[:activation_snapshot]) -> cell[:activation_snapshot] * 1.0
      is_number(cell["activation_snapshot"]) -> cell["activation_snapshot"] * 1.0
      is_number(cell[:activation]) -> cell[:activation] * 1.0
      is_number(cell["activation"]) -> cell["activation"] * 1.0
      is_number(cell[:modulated_activation]) -> cell[:modulated_activation] * 1.0
      is_number(cell["modulated_activation"]) -> cell["modulated_activation"] * 1.0
      is_number(cell[:score]) -> cell[:score] * 1.0
      is_number(cell["score"]) -> cell[:score] * 1.0
      true -> nil
    end
  end

  # -------- Small utils ----------------------------------------------

  defp downcased(nil), do: ""
  defp downcased(v), do: v |> to_string() |> String.downcase()
end
