defmodule Core.Text.Passage do
  @moduledoc """
  Read-only sentence segmentation with source-aligned UTF-8 byte spans.

  Preserves original sentence text and punctuation.
  Does not select lexical senses, resolve references, or assert facts.

  Decimal points are not sentence boundaries because they are not
  followed by whitespace or the end of the input.

  Abbreviations and quoted sentence endings require further analysis.
  """

  @boundary ~r/[.!?]+(?=\s|\z)/u

  @type segment :: %{
          text: String.t(),
          span: {non_neg_integer(), non_neg_integer()},
          terminator: :period | :question_mark | :exclamation_mark | :none
        }

  @spec segment(String.t()) ::
          {:ok, [segment()]} | {:error, :invalid_input}

  def segment(text) when is_binary(text) do
    {segments, cursor} =
      @boundary
      |> Regex.scan(text, return: :index)
      |> Enum.reduce({[], 0}, fn
        [{position, length}], {acc, start} ->
          stop = position + length

          acc =
            case build_segment(text, start, stop) do
              nil -> acc
              segment -> [segment | acc]
            end

          {acc, stop}
      end)

    segments =
      case build_segment(text, cursor, byte_size(text)) do
        nil -> segments
        segment -> [segment | segments]
      end

    {:ok, Enum.reverse(segments)}
  end

  def segment(_), do: {:error, :invalid_input}

  defp build_segment(text, first, last)
       when first <= last do
    raw = binary_part(text, first, last - first)

    without_leading = String.trim_leading(raw)
    content = String.trim_trailing(without_leading)

    leading_bytes =
      byte_size(raw) - byte_size(without_leading)

    start = first + leading_bytes
    stop = start + byte_size(content)

    if content == "" do
      nil
    else
      %{
        text: content,
        span: {start, stop},
        terminator: terminator(content)
      }
    end
  end

  defp terminator(text) do
    cond do
      String.ends_with?(text, "?") ->
        :question_mark

      String.ends_with?(text, "!") ->
        :exclamation_mark

      String.ends_with?(text, ".") ->
        :period

      true ->
        :none
    end
  end

  @doc """
  Align source-preserved passage segments to canonical word tokens.

  The result is observational. It does not invoke semantic processing,
  change token indices, or update memory.
  """
  @spec align(map()) ::
          {:ok, [map()]} | {:error, :invalid_alignment}

  def align(%{
        sentence: sentence,
        original_text: original,
        tokenization: %{
          valid?: true,
          span_unit: :utf8_bytes,
          token_indices: :lexical_words
        },
        tokens: tokens
      })
      when is_binary(sentence) and is_binary(original) and
             is_list(tokens) do
    with true <- sentence == original,
         {:ok, segments} <- segment(sentence),
         true <- segments != [] and length(segments) <= 16,
         true <- tokens != [] and length(tokens) <= 256,
         true <- valid_tokens?(tokens, sentence),
         true <-
           Enum.all?(tokens, fn token ->
             Enum.count(segments, &belongs?(token, &1)) == 1
           end) do
      aligned =
        Enum.map(segments, fn segment ->
          indices =
            for token <- tokens,
                belongs?(token, segment),
                do: token.index

          range =
            case indices do
              [] -> nil
              [first | _] -> {first, List.last(indices)}
            end

          segment
          |> Map.put(:word_indices, indices)
          |> Map.put(:word_range, range)
        end)

      complete? =
        Enum.all?(aligned, &(&1.word_indices != [])) and
          Enum.flat_map(aligned, & &1.word_indices) ==
            Enum.map(tokens, & &1.index)

      if complete? do
        {:ok, aligned}
      else
        {:error, :invalid_alignment}
      end
    else
      _ -> {:error, :invalid_alignment}
    end
  end

  def align(_), do: {:error, :invalid_alignment}

  defp valid_tokens?(tokens, sentence) do
    individual? =
      tokens
      |> Enum.with_index()
      |> Enum.all?(fn {token, expected_index} ->
        case token do
          %{
            index: ^expected_index,
            n: 1,
            kind: :word,
            phrase: phrase,
            span: {first, last}
          }
          when is_binary(phrase) and
                 is_integer(first) and is_integer(last) and
                 first >= 0 and last > first and
                 last <= byte_size(sentence) ->
            binary_part(sentence, first, last - first) == phrase

          _ ->
            false
        end
      end)

    ordered? =
      tokens
      |> Enum.chunk_every(2, 1, :discard)
      |> Enum.all?(fn [left, right] ->
        elem(left.span, 1) <= elem(right.span, 0)
      end)

    individual? and ordered?
  end

  defp belongs?(
         %{span: {token_start, token_stop}},
         %{span: {sentence_start, sentence_stop}}
       ) do
    token_start >= sentence_start and
      token_stop <= sentence_stop
  end
end
