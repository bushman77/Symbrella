defmodule Brain.LIFG.BoundaryGuard do
  @moduledoc ~S"""
  Boundary filter for LIFG: drop tokens that do not align to word boundaries
  in the original sentence (unless they are explicit MWEs via `mw: true`).
  """

  @spec sanitize([map()], String.t()) :: [map()]
  def sanitize(tokens, sentence) when is_list(tokens) and is_binary(sentence) do
    # regex first, then string
    words =
      Regex.scan(~r/\p{L}+/u, sentence)
      |> List.flatten()
      |> MapSet.new()

    Enum.filter(tokens, fn t -> keep_token?(t, words) end)
  end

  # Keep explicit MWEs
  defp keep_token?(%{mw: true}, _words), do: true

  # Single-word tokens must match a standalone word from the sentence
  defp keep_token?(%{phrase: phrase}, words) when is_binary(phrase) do
    if String.contains?(phrase, " ") do
      false
    else
      MapSet.member?(words, phrase)
    end
  end

  # Unexpected shape → permissive
  defp keep_token?(_t, _words), do: true
end
