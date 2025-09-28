defmodule Core.LexID do
  @moduledoc false
  @pos ~w(noun verb adj adv interj pron prep conj det num aux adp part punct unk)

  def build(word, pos, idx), do: "#{normalize_word(word)}|#{normalize_pos(pos)}|#{idx}"

  def cell_id?(id) when is_binary(id) do
    case String.split(id, "|") do
      [w, p, idx] -> w != "" and p in @pos and String.match?(idx, ~r/^\d+$/)
      _ -> false
    end
  end

  def normalize_word(w), do: w |> String.trim() |> String.downcase()
  def normalize_pos(nil), do: "unk"

  def normalize_pos(pos) do
    case String.downcase(String.trim(pos)) do
      "adjective" -> "adj"
      "adverb" -> "adv"
      "interjection" -> "interj"
      "preposition" -> "prep"
      "conjunction" -> "conj"
      "pronoun" -> "pron"
      other -> other
    end
  end

  @doc "Runtime check for sense ids like \"word|pos|idx\"."
  @spec looks_like_id?(binary) :: boolean
  def looks_like_id?(bin) when is_binary(bin) do
    case String.split(bin, "|") do
      [w, pos, idx] ->
        w != "" and pos_ok?(pos) and integer_string?(idx)

      _ ->
        false
    end
  end

  defp pos_ok?(pos) do
    norm = normalize_pos(pos)
    norm in ["noun", "verb", "adj", "adv", "interj", "prep", "conj", "pron", "unk"]
  end

  defp integer_string?(s) do
    case Integer.parse(s) do
      {_, ""} -> true
      _ -> false
    end
  end
end
