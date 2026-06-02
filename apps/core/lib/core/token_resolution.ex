defmodule Core.TokenResolution do
  @moduledoc """
  Builds a semantic token cover without changing the raw tokenizer output.

  Tokenization stays conservative: `si.tokens` contains base span-backed words,
  while `si.phrase_candidates` and `si.mwe_matches` describe possible phrase
  windows. This stage chooses a compact non-overlapping cover that downstream
  stages can inspect via `:token_cover` / `:resolved_tokens`.
  """

  alias Core.SemanticInput

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

  defp phrase_candidates(si, sentence) do
    matches = si |> Map.get(:mwe_matches, []) |> List.wrap()
    phrase_candidates = si |> Map.get(:phrase_candidates, []) |> List.wrap()

    matches
    |> Enum.flat_map(fn match ->
      phrase = text(match, :phrase)
      size = int_get(match, :size, 1)

      phrase_candidates
      |> Enum.filter(fn cand -> norm(text(cand, :phrase)) == norm(phrase) end)
      |> Enum.map(fn cand ->
        {start, stop} = interval(cand, sentence)
        kind = atom_get(match, :kind, :pos)

        %{
          kind: :phrase,
          phrase: text(cand, :phrase),
          span: Map.get(cand, :span) || Map.get(cand, "span"),
          n: int_get(cand, :n, size),
          source: :token_resolution,
          resolution: kind,
          pos: Map.get(match, :pos) || Map.get(match, "pos") || [],
          confirmed?: kind in [:lex, :both],
          start: start,
          stop: stop,
          score: phrase_score(kind, size)
        }
      end)
    end)
    |> Enum.uniq_by(fn c -> {norm(c.phrase), c.start, c.stop, c.resolution} end)
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

  defp sort_key(c), do: {-c.score, c.start, -(c.stop - c.start), c.phrase}

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
    |> String.replace(~r/\s+/u, " ")
  end
end
