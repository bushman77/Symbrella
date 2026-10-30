defmodule Core.MWE.Boundaries do
  @moduledoc """
  Guards multiword candidates against punctuation and sentence boundaries.

  Canonical Phase 1 tokens use end-exclusive UTF-8 byte spans, while punctuation
  is stored separately. Inspect the *original source gap* when that contract
  is present; contiguous lexical indices alone cannot prove adjacency.

  Legacy callers without canonical source metadata retain their earlier
  surface-punctuation and token-index checks.
  """

  @spec valid_window?(list(), map() | nil) :: boolean()
  def valid_window?(tokens, context \\ nil)

  def valid_window?([_, _ | _] = tokens, context) do
    tokens
    |> Enum.chunk_every(2, 1, :discard)
    |> Enum.all?(fn [left, right] ->
      l = phrase(left)
      r = phrase(right)

      l != "" and r != "" and
        not String.match?(l, ~r/[\\p{P}\\p{S}]$/u) and
        not String.match?(r, ~r/^[\\p{P}\\p{S}]/u) and
        contiguous?(left, right) and
        source_gap_clear?(left, right, context)
    end)
  end

  def valid_window?(_, _), do: false

  # Never cross a punctuation token or a line break merely because Phase 1
  # assigned consecutive lexical indices to the words on either side.
  defp source_gap_clear?(left, right, %{
         sentence: sentence,
         tokenization: %{span_unit: :utf8_bytes, end_exclusive?: true, valid?: true}
       }) when is_binary(sentence) do
    with {start_left, end_left} when is_integer(start_left) and
                                     is_integer(end_left) <- field(left, :span),
         {start_right, end_right} when is_integer(start_right) and
                                       is_integer(end_right) <- field(right, :span),
         true <- start_left >= 0 and end_left > start_left,
         true <- start_right >= end_left and end_right > start_right,
         true <- end_right <= byte_size(sentence),
         true <- binary_part(sentence, start_left, end_left - start_left) == phrase(left),
         true <- binary_part(sentence, start_right, end_right - start_right) == phrase(right) do
      gap = binary_part(sentence, end_left, start_right - end_left)
      # Horizontal whitespace only: newlines break phrase continuity.
      String.match?(gap, ~r/\\A\\h+\\z/u)
    else
      _ -> false
    end
  end

  defp source_gap_clear?(_left, _right, %{tokenization: %{span_unit: :utf8_bytes}}),
    do: false

  defp source_gap_clear?(_left, _right, _legacy), do: true

  defp contiguous?(left, right) do
    case {field(left, :index), field(right, :index)} do
      {a, b} when is_integer(a) and is_integer(b) -> b == a + 1
      _ -> true
    end
  end

  defp phrase(token) do
    case field(token, :phrase) do
      text when is_binary(text) -> String.trim(text)
      _ -> ""
    end
  end

  defp field(%{} = token, key), do: Map.get(token, key) || Map.get(token, Atom.to_string(key))
  defp field(_, _), do: nil
end
