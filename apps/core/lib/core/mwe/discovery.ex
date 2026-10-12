defmodule Core.MWE.Discovery do
  @moduledoc """
  Non-lexical discovery of plausible multi-word expressions.

  Existing dictionary MWEs remain authoritative in :mwe_lexicon. Missing
  phrases can be proposed from curated lexical signatures or conservative
  productive patterns (verb + particle, noun + noun).

  Hypotheses are explicitly unverified and NEVER enter :sense_candidates,
  :mwe_lexicon, or LIFG as invented dictionary senses. They are observations
  that can later be verified through contextual evidence or learning.
  """

  alias Core.MWE.Boundaries

  @particles MapSet.new(~w(up down off on out in over away back around through apart along aside))
  @default_limit 24

  @spec run(map(), keyword()) :: map()
  def run(si, opts \\ [])

  def run(%{tokens: tokens} = si, opts) when is_list(tokens) and is_list(opts) do
    if Keyword.get(opts, :mwe_discovery, :on) in [:off, false] do
      si
    else
      words =
        tokens
        |> Enum.filter(&(field(&1, :n) in [nil, 1]))
        |> Enum.with_index()
        |> Enum.map(fn {token, fallback_index} ->
          %{
            index: field(token, :index) || fallback_index,
            phrase: field(token, :phrase) || "",
            word: normalize(field(token, :phrase))
          }
        end)

      known =
        si
        |> Map.get(:mwe_lexicon, [])
        |> List.wrap()
        |> Enum.filter(fn
          %{senses: senses} when is_list(senses) -> senses != []
          _ -> false
        end)
        |> MapSet.new(fn unit -> {Map.get(unit, :span), normalize(Map.get(unit, :phrase))} end)

      parts_of_speech = collect_pos(Map.get(si, :active_cells, []))

      hypotheses =
        (curated_hypotheses(si, words) ++ productive_hypotheses(words, parts_of_speech))
        |> Enum.reject(fn candidate ->
          MapSet.member?(known, {candidate.span, candidate.phrase})
        end)
        |> Enum.sort_by(fn candidate ->
          {first, last} = candidate.span
          {first, -priority(candidate.category), -(last - first + 1), candidate.phrase}
        end)
        |> Enum.uniq_by(&{&1.span, &1.phrase})
        |> Enum.take(discovery_limit(opts))

      Map.put(si, :mwe_hypotheses, hypotheses)
    end
  end

  def run(other, _opts), do: other

  defp curated_hypotheses(si, words) do
    si
    |> Map.get(:mwe_matches, [])
    |> List.wrap()
    |> Enum.flat_map(fn
      %{span: {first, last}, phrase: match_phrase, kind: kind}
      when kind in [:lex, :both] and is_integer(first) and is_integer(last) and
             first >= 0 and last >= first ->
        segment =
          Enum.filter(words, fn word ->
            word.index >= first and word.index <= last
          end)

        if length(segment) == last - first + 1 and
             Boundaries.valid_window?(segment) and
             normalize(Enum.map_join(segment, " ", & &1.phrase)) == normalize(match_phrase) do
          [hypothesis(match_phrase, {first, last}, :curated_phrase, [:lexical_signature])]
        else
          []
        end

      _ ->
        []
    end)
  end

  defp productive_hypotheses(words, pos_by_word) do
    words
    |> Enum.chunk_every(2, 1, :discard)
    |> Enum.flat_map(fn [first, second] = segment ->
      if Boundaries.valid_window?(segment) and first.word != "" and second.word != "" do
        a_pos = Map.get(pos_by_word, first.word, MapSet.new())
        b_pos = Map.get(pos_by_word, second.word, MapSet.new())

        category =
          cond do
            MapSet.member?(a_pos, "verb") and MapSet.member?(@particles, second.word) ->
              :phrasal_verb

            MapSet.member?(a_pos, "noun") and MapSet.member?(b_pos, "noun") ->
              :compound_noun

            true ->
              nil
          end

        if category do
          [hypothesis(
            first.word <> " " <> second.word,
            {first.index, second.index},
            category,
            [:word_pos_pattern]
          )]
        else
          []
        end
      else
        []
      end
    end)
  end

  defp hypothesis(phrase, span, category, evidence) do
    {first, last} = span

    %{
      phrase: normalize(phrase),
      span: span,
      size: last - first + 1,
      category: category,
      evidence: evidence,
      source: :mwe_discovery,
      status: :unverified,
      verified?: false
    }
  end

  defp collect_pos(cells) do
    cells
    |> List.wrap()
    |> Enum.reduce(%{}, fn cell, acc ->
      term = normalize(field(cell, :norm) || field(cell, :word))
      raw_pos = field(cell, :pos)

      pos =
        case raw_pos do
          p when is_binary(p) -> p |> String.trim() |> String.downcase()
          p when is_atom(p) -> Atom.to_string(p)
          _ -> ""
        end

      if term != "" and not String.contains?(term, " ") and pos != "" do
        Map.update(acc, term, MapSet.new([pos]), &MapSet.put(&1, pos))
      else
        acc
      end
    end)
  end

  defp priority(:curated_phrase), do: 3
  defp priority(:phrasal_verb), do: 2
  defp priority(:compound_noun), do: 1

  defp discovery_limit(opts) do
    case Keyword.get(opts, :mwe_discovery_limit, @default_limit) do
      n when is_integer(n) and n > 0 -> min(n, 64)
      _ -> @default_limit
    end
  end

  defp field(%{} = map, key), do: Map.get(map, key) || Map.get(map, Atom.to_string(key))
  defp field(_, _), do: nil

  defp normalize(text) when is_binary(text) do
    text
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/^\p{P}+|\p{P}+$/u, "")
    |> String.replace(~r/\s+/u, " ")
  end

  defp normalize(_), do: ""
end
