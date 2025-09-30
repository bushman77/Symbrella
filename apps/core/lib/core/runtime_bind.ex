defmodule Core.RuntimeBind do
  @moduledoc ~S"""
  Runtime binding: match current tokens (and MWEs) to a snapshot of known cells,
  producing `si.active_cells`, with tracing and soft-fail behavior.
  """

  alias Core.SemanticInput

  @type snapshot_t ::
          map()
          | (binary() -> list())
          | (binary(), keyword() -> list())
          | {any(), list()}

  # Tests call bind/1; just forward to bind/2.
  @spec bind(SemanticInput.t()) :: SemanticInput.t()
  def bind(%SemanticInput{} = si), do: bind(si, [])

  @spec bind(SemanticInput.t(), keyword()) :: SemanticInput.t()
  def bind(%SemanticInput{} = si, opts) do
    snapshot = Keyword.get(opts, :snapshot)
    topk = Keyword.get(opts, :topk, 16)
    # Soft-fail should report timeout? when snapshot is missing, unless explicitly overridden
    timeout? = !!Keyword.get(opts, :timeout?, is_nil(snapshot))

    phrases = phrases_from_tokens(si)
    # downcased phrase => [%{tok_id: ...}]
    hitmap = phrase_hitmap(si)

    {active_cells, count, brain_ref} =
      case snapshot do
        nil ->
          {[], 0, Map.get(si, :brain_state_ref)}

        {ref, list} when is_list(list) ->
          m = collect_matches(phrases, {ref, list}, Keyword.put(opts, :_hitmap, hitmap))
          {dedup_and_cap(m, topk), length(m), ref}

        %{} = snap ->
          m = collect_matches(phrases, snap, Keyword.put(opts, :_hitmap, hitmap))
          {dedup_and_cap(m, topk), length(m), Map.get(snap, :ref, Map.get(si, :brain_state_ref))}

        fun when is_function(fun, 1) or is_function(fun, 2) ->
          m = collect_matches(phrases, fun, Keyword.put(opts, :_hitmap, hitmap))
          {dedup_and_cap(m, topk), length(m), Map.get(si, :brain_state_ref)}
      end

    ev = %{
      stage: :runtime_bind,
      ts_ms: System.system_time(:millisecond),
      meta: %{count: count, timeout?: timeout?}
    }

    si2 = %Core.SemanticInput{
      si
      | active_cells: active_cells,
        trace: [ev | si.trace || []]
    }

    case brain_ref do
      nil -> si2
      _ -> Map.put(si2, :brain_state_ref, brain_ref)
    end
  end

  # -------------------------
  # Matching
  # -------------------------

  # function(snapshot, 1-arity)
  defp collect_matches(phrases, snapshot, opts) when is_function(snapshot, 1) do
    hm = Keyword.get(opts, :_hitmap, %{})

    phrases
    |> Enum.flat_map(fn p -> List.wrap(snapshot.(p)) end)
    |> Enum.filter(&valid_cell?/1)
    |> Enum.map(&annotate_cell(&1, hm))
  end

  # function(snapshot, 2-arity)
  defp collect_matches(phrases, snapshot, opts) when is_function(snapshot, 2) do
    hm = Keyword.get(opts, :_hitmap, %{})

    phrases
    |> Enum.flat_map(fn p -> List.wrap(snapshot.(p, opts)) end)
    |> Enum.filter(&valid_cell?/1)
    |> Enum.map(&annotate_cell(&1, hm))
  end

  # map index snapshot
  defp collect_matches(phrases, %{} = snapshot, opts) do
    hm = Keyword.get(opts, :_hitmap, %{})

    idx =
      cond do
        is_map(snapshot[:index]) -> snapshot[:index]
        is_map(snapshot[:cells]) -> snapshot[:cells]
        is_map(snapshot[:phrases]) -> snapshot[:phrases]
        true -> %{}
      end

    phrases
    |> Enum.flat_map(fn p -> List.wrap(Map.get(idx, p, [])) end)
    |> Enum.filter(&valid_cell?/1)
    |> Enum.map(&annotate_cell(&1, hm))
  end

  # tuple {brain_state_ref, cells}
  defp collect_matches(phrases, {ref, list}, opts) when is_list(list) do
    hm = Keyword.get(opts, :_hitmap, %{})
    set = MapSet.new(phrases)

    list
    |> Enum.filter(&valid_cell?/1)
    |> Enum.filter(fn cell ->
      keys = Map.get(cell, :keys, Map.get(cell, "keys", [])) |> List.wrap()
      Enum.any?(keys, fn k -> MapSet.member?(set, String.downcase(to_string(k))) end)
    end)
    |> Enum.map(fn cell ->
      cell
      |> Map.put_new(:brain_state_ref, ref)
      |> annotate_cell(hm)
    end)
  end

  defp annotate_cell(cell, hitmap) do
    keys =
      cell
      |> Map.get(:keys, Map.get(cell, "keys", []))
      |> List.wrap()
      |> Enum.map(&String.downcase(to_string(&1)))

    matches =
      keys
      |> Enum.flat_map(&Map.get(hitmap, &1, []))
      |> Enum.uniq_by(& &1.tok_id)
      # stable [0,1,2,...] for tests
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
        is_number(cell[:activation]) -> cell[:activation] * 1.0
        is_number(cell[:modulated_activation]) -> cell[:modulated_activation] * 1.0
        is_number(cell[:score]) -> cell[:score] * 1.0
        true -> nil
      end

    if is_number(snap), do: Map.put(cell, :activation_snapshot, snap), else: cell
  end

  # -------------------------
  # Validity
  # -------------------------

  defp valid_cell?(%{id: id}) when is_binary(id) or is_integer(id), do: true
  defp valid_cell?(_), do: false

  # -------------------------
  # Phrase extraction (tokens + MWEs)
  # -------------------------

  defp phrases_from_tokens(%SemanticInput{tokens: tokens}) do
    tokens = List.wrap(tokens)

    word_phrases =
      tokens
      |> Enum.map(&token_phrase/1)
      |> Enum.filter(&is_binary/1)
      |> Enum.map(&String.downcase/1)

    mwe_phrases =
      tokens
      |> Enum.filter(&is_mwe_head?/1)
      |> Enum.map(&mwe_phrase(&1, tokens))
      |> Enum.filter(&is_binary/1)
      |> Enum.map(&String.downcase/1)

    Enum.uniq(word_phrases ++ mwe_phrases)
  end

  defp phrase_hitmap(%SemanticInput{tokens: tokens}) do
    tokens = List.wrap(tokens)

    # Map phrase -> [%{tok_id: ...}]
    Enum.reduce(Enum.with_index(tokens), %{}, fn {t, idx}, acc ->
      # word token (append to preserve ascending order)
      acc =
        case token_phrase(t) do
          nil ->
            acc

          p ->
            Map.update(acc, String.downcase(p), [%{tok_id: token_id(t, idx)}], fn lst ->
              lst ++ [%{tok_id: token_id(t, idx)}]
            end)
        end

      # mwe head -> expand to its phrase and all member token ids
      acc =
        if is_mwe_head?(t) do
          case mwe_phrase_and_ids(t, tokens, idx) do
            {nil, _ids} ->
              acc

            {phrase, ids} ->
              Map.update(acc, String.downcase(phrase), Enum.map(ids, &%{tok_id: &1}), fn lst ->
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
    # Prefer explicit mwe_span by token ids if present
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
      {a, b} ->
        if is_integer(a) and is_integer(b) do
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
        else
          {token_phrase(token), List.wrap(token_id(token, idx_fallback))}
        end

      _ ->
        {token_phrase(token), List.wrap(token_id(token, idx_fallback))}
    end
  end

  # -------------------------
  # Dedup + TopK
  # -------------------------

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

  # Merge with source precedence; numeric :source_priority wins, else name map
  defp merge_cell(a, b) do
    {pa, pb} = {source_pri(a), source_pri(b)}
    sa = snapshot_of(a)
    sb = snapshot_of(b)

    preferred =
      cond do
        pb > pa -> b
        pa > pb -> a
        is_number(sb) and is_number(sa) and sb > sa -> b
        true -> a
      end

    fallback = if preferred === a, do: b, else: a

    merged =
      Map.merge(fallback, preferred, fn _k, _v1, v2 -> v2 end)

    # Ensure the final cell’s snapshot reflects the winner’s source
    snap = snapshot_of(preferred) || snapshot_of(fallback)
    if is_number(snap), do: Map.put(merged, :activation_snapshot, snap), else: merged
  end

  defp snapshot_of(cell) do
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
  end

  defp source_pri(%{source_priority: p}) when is_number(p), do: p * 1.0
  defp source_pri(%{"source_priority" => p}) when is_number(p), do: p * 1.0
  defp source_pri(%{source: s}), do: source_name_pri(s)
  defp source_pri(%{"source" => s}), do: source_name_pri(s)
  defp source_pri(_), do: 0.0

  defp source_name_pri(s) when is_atom(s), do: source_name_pri(Atom.to_string(s))

  defp source_name_pri(s) when is_binary(s) do
    pri = %{
      "gold" => 5.0,
      "curated" => 4.0,
      "runtime" => 3.0,
      "mwe" => 2.5,
      "repo" => 2.0,
      "snapshot" => 2.0,
      "default" => 1.0
    }

    Map.get(pri, String.downcase(s), 1.0)
  end
end
