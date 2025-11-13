defmodule Core.Recall.Synonyms.Normalize do
  @moduledoc """
  Normalization helpers for words and part-of-speech tags.
  """

  @type pos :: atom() | nil

  @spec word(String.t()) :: String.t()
  def word(w) when is_binary(w) do
    w
    |> String.trim()
    |> String.downcase()
    |> String.normalize(:nfc)
  end

  @spec pos(term()) :: pos()
  def pos(nil), do: nil
  def pos(p) when is_atom(p), do: p

  def pos(p) when is_binary(p) do
    p
    |> String.trim()
    |> String.downcase()
    |> case do
      "n" -> :noun
      "v" -> :verb
      "adj" -> :adjective
      "adv" -> :adverb
      "noun" -> :noun
      "verb" -> :verb
      "adjective" -> :adjective
      "adverb" -> :adverb
      _ -> nil
    end
  end
end
