defmodule Core.TokenResolution do
  @moduledoc """
  Builds a semantic token cover without changing the raw tokenizer output.

  Tokenization stays conservative: `si.tokens` contains base span-backed words,
  while `si.phrase_candidates` and `si.mwe_matches` describe possible phrase
  windows. This stage chooses a compact non-overlapping cover that downstream
  stages can inspect via `:token_cover` / `:resolved_tokens`.
  """

  alias Core.SemanticInput
  alias Core.MWE.Boundaries

  @spec resolve(SemanticInput.t() | map(), keyword()) :: SemanticInput.t() | map()
  def resolve(si, opts \\ [])

  def resolve(%SemanticInput{} = si, opts) when is_list(opts) do
    si
    |> Map.from_struct()
    |> resolve(opts)
    |> then(&struct(SemanticInput, &1))
  end

  def resolve(%{} = si, opts) when is_list(opts) do
    if Keyword.get(opts, :token_resolution, :auto) in [false, :off] do
      si
    else
      cover = build_cover(si, opts)

      si
      |> Map.put(:token_cover, cover)
      |> Map.put(:resolved_tokens, cover)
      |> append_trace(:token_resolution, %{
        decision: if(Enum.any?(cover, &(&1.kind != :token)), do: :resolved, else: :base_tokens),
        meta: %{
          cover_count: length(cover),
          phrase_count: Enum.count(cover, &(&1.kind != :token))
        }
      })
    end
  end

  def resolve(other, _opts), do: other

  defp build_cover(si, opts) do
    tokens = si |> Map.get(:tokens, []) |> List.wrap()
    sentence = Map.get(si, :sentence, "")

    candidates =
      phrase_candidates(si, sentence) ++ base_candidates(tokens, sentence)

    candidates
    |> Enum.sort_by(&sort_key/1)
    |> take_nonoverlapping()
    |> Enum.sort_by(& &1.start)
    |> Enum.map(&Map.drop(&1, [:start, :stop, :score]))
    |> maybe_limit(opts)
  end

  # MWE match spans are inclusive base-token indices. Build character spans
  # from those tokens rather than depending on precomputed 2/3-grams (the
  # lexicon can recognize 4/5-grams, and a phrase may occur more than once).
  defp phrase_candidates(si, sentence) do
    tokens =
      si
      |> Map.get(:tokens, [])
      |> List.wrap()
      |> Enum.filter(&(int_get(&1, :n, 1) == 1))
      |> Enum.sort_by(&int_get(&1, :index, 0))

    si
    |> Map.get(:mwe_matches, [])
    |> List.wrap()
    |> Enum.group_by(fn match ->
      {Map.get(match, :span), norm(text(match, :phrase))}
    end)
    |> Enum.flat_map(fn {{span, phrase}, matches} ->
      with {start_index, end_index} <- span,
           true <- is_integer(start_index) and is_integer(end_index) and
                     start_index >= 0 and end_index >= start_index,
           segment <-
             Enum.filter(tokens, fn tok ->
               idx = int_get(tok, :index, -1)
               idx >= start_index and idx <= end_index
             end),
           true <- length(segment) == end_index - start_index + 1,
           true <- Boundaries.valid_window?(segment),
           true <- norm(Enum.map_join(segment, " ", &text(&1, :phrase))) == phrase do
        {first_start, _} = interval(hd(segment), sentence)
        {_, last_stop} = interval(List.last(segment), sentence)

        # Prefer DB lexical evidence, then the later MWE detector pass.
        {best, _} =
          matches
          |> Enum.with_index()
          |> Enum.max_by(fn {match, order} ->
            {if(Map.get(match, :source) == :db_lexicon, do: 1, else: 0), order}
          end)

        internal_patterns =
          matches
          |> Enum.reject(&(Map.get(&1, :source) == :db_lexicon))
          |> Enum.map(&(Map.get(&1, :pos) || []))
          |> Enum.uniq()

        internal_pos =
          matches
          |> Enum.reject(&(Map.get(&1, :source) == :db_lexicon))
          |> List.last()
          |> case do
            nil -> []
            match -> Map.get(match, :pos) || []
          end

        lexical_pos =
          matches
          |> Enum.flat_map(&(Map.get(&1, :lexical_pos) || []))
          |> Enum.uniq()

        sense_ids =
          matches
          |> Enum.flat_map(&(Map.get(&1, :sense_ids) || []))
          |> Enum.uniq()

        kind = atom_get(best, :kind, :pos)

        # Verified dictionary senses outrank competing structural patterns,
        # regardless of length. Curated phrase rules alone do not qualify.
        dictionary_backed? =
          Enum.any?(matches, fn match ->
            Map.get(match, :source) == :db_lexicon and
              List.wrap(Map.get(match, :lexical_pos)) != [] and
              List.wrap(Map.get(match, :sense_ids)) != []
          end)

        [
          %{
            kind: :phrase,
            phrase: Enum.map_join(segment, " ", &text(&1, :phrase)),
            span: {first_start, last_stop - first_start},
            token_span: {start_index, end_index},
            n: length(segment),
            source: :token_resolution,
            resolution: kind,
            pos: internal_pos,
            pos_patterns: internal_patterns,
            lexical_pos: lexical_pos,
            sense_ids: sense_ids,
            dictionary_backed?: dictionary_backed?,
            confirmed?: kind in [:lex, :both],
            start: first_start,
            stop: last_stop,
            score: phrase_score(kind, length(segment)) +
                     if(lexical_pos == [], do: 0.0, else: 0.5)
          }
        ]
      else
        _ -> []
      end
    end)
  end

  defp base_candidates(tokens, sentence) do
    Enum.map(tokens, fn tok ->
      {start, stop} = interval(tok, sentence)

      %{
        kind: :token,
        phrase: text(tok, :phrase),
        span: Map.get(tok, :span) || Map.get(tok, "span"),
        n: int_get(tok, :n, 1),
        source: Map.get(tok, :source) || Map.get(tok, "source") || :token,
        index: Map.get(tok, :index) || Map.get(tok, "index"),
        start: start,
        stop: stop,
        score: 1.0
      }
    end)
  end

  defp phrase_score(:both, size), do: 4.0 + size
  defp phrase_score(:lex, size), do: 3.5 + size
  defp phrase_score(:pos, size), do: 2.5 + size
  defp phrase_score(_kind, size), do: 2.0 + size

  # Apply lexical provenance before the length-based score so a long,
  # generic grammatical construction cannot displace a real dictionary MWE.
  defp sort_key(c) do
    provenance_rank = if Map.get(c, :dictionary_backed?, false), do: 0, else: 1
    {provenance_rank, -c.score, c.start, -(c.stop - c.start), c.phrase}
  end

  defp take_nonoverlapping(candidates) do
    candidates
    |> Enum.reduce({[], MapSet.new()}, fn cand, {acc, covered} ->
      range = MapSet.new(cand.start..max(cand.stop - 1, cand.start))

      if MapSet.disjoint?(covered, range) do
        {[cand | acc], MapSet.union(covered, range)}
      else
        {acc, covered}
      end
    end)
    |> elem(0)
  end

  defp maybe_limit(cover, opts) do
    case Keyword.get(opts, :token_resolution_limit) do
      limit when is_integer(limit) and limit > 0 -> Enum.take(cover, limit)
      _ -> cover
    end
  end

  defp interval(tok, sentence) do
    phrase = text(tok, :phrase) |> norm()
    span = Map.get(tok, :span) || Map.get(tok, "span") || {0, 0}
    sentence = sentence |> to_string() |> String.trim() |> String.replace(~r/\s+/u, " ")

    case span do
      {start, len} when is_integer(start) and is_integer(len) ->
        cond do
          slice_matches?(sentence, start, len, phrase) -> {start, start + len}
          len > start and slice_matches?(sentence, start, len - start, phrase) -> {start, len}
          true -> {start, max(start + max(len, 1), start + 1)}
        end

      [start, len] when is_integer(start) and is_integer(len) ->
        interval(%{span: {start, len}, phrase: phrase}, sentence)

      _ ->
        {0, 1}
    end
  end

  defp slice_matches?(sentence, start, len, phrase)
       when start >= 0 and len > 0 do
    sentence
    |> String.slice(start, len)
    |> norm()
    |> Kernel.==(phrase)
  end

  defp slice_matches?(_sentence, _start, _len, _phrase), do: false

  defp append_trace(si, stage, attrs) do
    event = %{
      stage: stage,
      decision: Map.get(attrs, :decision, :observed),
      meta: Map.get(attrs, :meta, %{}),
      ts_ms: System.system_time(:millisecond)
    }

    Map.update(si, :trace, [event], fn trace -> [event | List.wrap(trace)] end)
  end

  defp text(map, key) when is_map(map), do: Map.get(map, key) || Map.get(map, to_string(key)) || ""
  defp text(_map, _key), do: ""

  defp int_get(map, key, default) when is_map(map) do
    case Map.get(map, key) || Map.get(map, to_string(key)) do
      value when is_integer(value) -> value
      _ -> default
    end
  end

  defp int_get(_map, _key, default), do: default

  defp atom_get(map, key, default) when is_map(map) do
    case Map.get(map, key) || Map.get(map, to_string(key)) do
      value when is_atom(value) -> value
      value when is_binary(value) -> String.to_existing_atom(value)
      _ -> default
    end
  rescue
    ArgumentError -> default
  end

  defp atom_get(_map, _key, default), do: default

  defp norm(value) do
    value
    |> to_string()
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/^\p{P}+|\p{P}+$/u, "")
    |> String.replace(~r/\s+/u, " ")
  end
end
