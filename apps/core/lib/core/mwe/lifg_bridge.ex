defmodule Core.MWE.LIFGBridge do
  @moduledoc """
  Presents selected lexical MWEs to LIFG as additional candidate tokens.

  The original word tokens remain available and are restored after LIFG runs.
  Phrase choices are returned separately; they are not silently substituted
  for word-level decisions or treated as a sentence-level interpretation.
  """

  @spec prepare(map(), keyword()) :: {map(), map()}
  def prepare(si, opts \\ [])

  def prepare(%{tokens: tokens} = si, opts) when is_list(tokens) and is_list(opts) do
    if Keyword.get(opts, :mwe_lifg, :on) in [:off, false] or
         not sequential_indices?(tokens) do
      {si, %{}}
    else
      units = Map.get(si, :mwe_lexicon, []) |> List.wrap()

      covers =
        si
        |> Map.get(:token_cover, [])
        |> List.wrap()
        |> Enum.filter(&(Map.get(&1, :kind) == :phrase))

      selected =
        Enum.flat_map(covers, fn cover ->
          unit =
            Enum.find(units, fn candidate ->
              candidate.span == Map.get(cover, :token_span) and
                candidate.phrase == norm(Map.get(cover, :phrase))
            end)

          if is_map(unit) and List.wrap(Map.get(unit, :senses)) != [] do
            [{cover, unit}]
          else
            []
          end
        end)

      {extra, buckets, virtual} =
        selected
        |> Enum.with_index(length(tokens))
        |> Enum.reduce({[], %{}, %{}}, fn {{cover, unit}, idx}, {acc, sc, refs} ->
          token = %{
            index: idx,
            token_index: idx,
            phrase: unit.phrase,
            span: cover.span,
            n: unit.size,
            mw: true,
            source: :mwe_lexicon
          }

          candidates =
            Enum.map(unit.senses, fn sense ->
              %{
                id: sense.id,
                lemma: sense.norm,
                norm: sense.norm,
                pos: sense.pos,
                definition: Map.get(sense, :definition),
                gram_function: Map.get(sense, :gram_function, []),
                activation: Map.get(sense, :activation) || 0.3,
                score: 0.3,
                rel_prior: 0.3,
                mw: true,
                source: :mwe_lexicon
              }
            end)

          {[token | acc], Map.put(sc, idx, candidates), Map.put(refs, idx, cover)}
        end)

      merged_buckets =
        Map.merge(Map.get(si, :sense_candidates) || %{}, buckets)

      input =
        si
        |> Map.put(:tokens, tokens ++ Enum.reverse(extra))
        |> Map.put(:sense_candidates, merged_buckets)

      {input, virtual}
    end
  end

  def prepare(other, _opts), do: {other, %{}}

  @spec split_choices(list(), map()) :: {list(), list()}
  def split_choices(choices, virtual) when is_list(choices) and is_map(virtual) do
    {words, phrases} =
      Enum.reduce(choices, {[], []}, fn choice, {word_acc, phrase_acc} ->
        case Map.get(virtual, Map.get(choice, :token_index)) do
          nil ->
            {[choice | word_acc], phrase_acc}

          cover ->
            phrase_choice =
              Map.merge(choice, %{
                phrase: cover.phrase,
                span: cover.span,
                token_span: cover.token_span
              })

            {word_acc, [phrase_choice | phrase_acc]}
        end
      end)

    {Enum.reverse(words), Enum.reverse(phrases)}
  end

  defp sequential_indices?(tokens) do
    tokens
    |> Enum.with_index()
    |> Enum.all?(fn {token, idx} -> Map.get(token, :index) == idx end)
  end

  defp norm(value) when is_binary(value) do
    value
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/^\p{P}+|\p{P}+$/u, "")
    |> String.replace(~r/\s+/u, " ")
  end

  defp norm(_), do: ""
end
