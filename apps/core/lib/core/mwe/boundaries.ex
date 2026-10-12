defmodule Core.MWE.Boundaries do
  @moduledoc """
  Guards candidate MWE windows against sentence and punctuation boundaries.

  Word tokenization preserves punctuation on the surface of boundary words.
  A punctuation/symbol character at the end of a nonfinal token, or at the
  start of a noninitial token, prevents a window from crossing that boundary.

  Internal apostrophes in contractions and punctuation *after* the final
  phrase word remain valid. Explicit token indices, when present, must be
  contiguous to prevent joining words across discarded tokens.
  """

  @spec valid_window?(list()) :: boolean()
  def valid_window?([_, _ | _] = tokens) do
    tokens
    |> Enum.chunk_every(2, 1, :discard)
    |> Enum.all?(fn [left, right] ->
      l = phrase(left)
      r = phrase(right)

      l != "" and r != "" and
        not String.match?(l, ~r/[\p{P}\p{S}]$/u) and
        not String.match?(r, ~r/^[\p{P}\p{S}]/u) and
        contiguous?(left, right)
    end)
  end

  def valid_window?(_), do: false

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
