defmodule Core.Response.LlmPrompt.WorkingMemory do
  @moduledoc """
  Working-memory term summarization for LLM prompt context.
  """

  @summary_window 5

  @low_info_terms MapSet.new([
                    "a",
                    "an",
                    "the",
                    "and",
                    "or",
                    "to",
                    "of",
                    "in",
                    "on",
                    "for",
                    "with",
                    "at",
                    "by",
                    "is",
                    "are",
                    "was",
                    "were",
                    "be",
                    "been",
                    "being",
                    "i",
                    "you",
                    "he",
                    "she",
                    "it",
                    "we",
                    "they",
                    "me",
                    "him",
                    "her",
                    "us",
                    "them",
                    "my",
                    "your",
                    "his",
                    "its",
                    "our",
                    "their",
                    "this",
                    "that",
                    "these",
                    "those",
                    "have",
                    "has",
                    "had",
                    "tell"
                  ])

  @spec summarize(list()) :: [String.t()]
  def summarize(wm) when is_list(wm) do
    terms =
      wm
      |> Enum.take(@summary_window)
      |> Enum.map(&wm_item_term/1)
      |> Enum.reject(&(&1 == ""))
      |> Enum.uniq()

    phrases = Enum.filter(terms, &meaningful_phrase?/1)

    if phrases == [] do
      terms
    else
      phrase_tokens = phrase_token_set(phrases)

      others =
        terms
        |> Enum.reject(&(&1 in phrases))
        |> Enum.reject(&low_info_term?/1)
        |> Enum.reject(&overlapping_singleton?(&1, phrase_tokens))

      (phrases ++ others)
      |> Enum.take(@summary_window)
    end
  end

  def summarize(_), do: []

  defp meaningful_phrase?(term) when is_binary(term) do
    String.contains?(term, " ")
  end

  defp meaningful_phrase?(_), do: false

  defp phrase_token_set(phrases) do
    phrases
    |> Enum.flat_map(fn phrase ->
      phrase
      |> String.downcase()
      |> String.split(~r/\s+/u, trim: true)
    end)
    |> MapSet.new()
  end

  defp low_info_term?(term) when is_binary(term) do
    MapSet.member?(@low_info_terms, String.downcase(term))
  end

  defp low_info_term?(_), do: false

  defp overlapping_singleton?(term, phrase_tokens) when is_binary(term) do
    not String.contains?(term, " ") and
      MapSet.member?(phrase_tokens, String.downcase(term))
  end

  defp overlapping_singleton?(_, _), do: false

  defp wm_item_term(item) when is_map(item) do
    payload = map_get(item, :payload)

    payload_lemma =
      if is_map(payload) do
        map_get(payload, :lemma)
      else
        nil
      end

    item_lemma = map_get(item, :lemma)
    id = map_get(item, :id)

    normalize_term(payload_lemma || item_lemma || id)
  end

  defp wm_item_term(other), do: normalize_term(other)

  defp normalize_term(nil), do: ""

  defp normalize_term(term) when is_binary(term) do
    String.trim(term)
  end

  defp normalize_term(term) do
    term
    |> to_string()
    |> String.trim()
  end

  defp map_get(map, key, default \\ nil)

  defp map_get(map, key, default) when is_map(map) and is_atom(key) do
    Map.get(map, key, Map.get(map, Atom.to_string(key), default))
  end

  defp map_get(_, _, default), do: default
end
