defmodule Core.Response.LlmPrompt do
  @moduledoc false

  @spec build_system_prompt(map(), map(), map(), list()) :: String.t()
  def build_system_prompt(features, decision, mood, wm_items \\ []) when is_list(wm_items) do
    tone = Map.get(decision, :tone)
    mode = Map.get(decision, :mode)
    intent = Map.get(features, :intent)
    exp = getv(mood, :exploration)
    inh = getv(mood, :inhibition)
    vig = getv(mood, :vigilance)
    plast = getv(mood, :plasticity)
    tone_hint = Map.get(mood, :tone_hint)

    wm_summary = summarize_wm(wm_items)

    """
    You are Symbrella, a brain-inspired AI assistant.
    #{tone_directive(tone, tone_hint)}
    #{mood_context(exp, inh, vig, plast)}
    #{mode_directive(mode, intent)}
    #{wm_context(wm_summary)}
    Keep your response concise and directly relevant to the user's input.
    Do not explain your reasoning. Just respond naturally.
    """
    |> String.trim()
  end

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

  @spec summarize_wm(list()) :: [String.t()]
  def summarize_wm(wm) when is_list(wm) do
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

  def summarize_wm(_), do: []

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

  defp tone_directive(:warm, _), do: "Respond in a warm, engaged, and encouraging tone."

  defp tone_directive(:deescalate, _),
    do: "Respond calmly and gently. Keep things grounded and constructive."

  defp tone_directive(:firm, _), do: "Respond clearly and directly. Stay focused and purposeful."

  defp tone_directive(:neutral, :deescalate),
    do: "Respond in a measured, steady tone. Things are settling down."

  defp tone_directive(:neutral, _), do: "Respond in a balanced, clear tone."
  defp tone_directive(_, _), do: "Respond helpfully and clearly."

  defp mood_context(exp, inh, vig, plast) do
    []
    |> maybe_add(exp > 0.65, "You feel curious and ready to explore.")
    |> maybe_add(exp < 0.35, "You are in a conservative, careful state.")
    |> maybe_add(vig > 0.80, "Vigilance is elevated — stay measured.")
    |> maybe_add(inh > 0.70, "Inhibition is high — keep things calm.")
    |> maybe_add(plast > 0.65, "You are in a receptive, learning-ready state.")
    |> case do
      [] -> ""
      notes -> "Current mood: " <> Enum.join(notes, " ")
    end
  end

  defp maybe_add(notes, true, note), do: notes ++ [note]
  defp maybe_add(notes, false, _), do: notes

  defp mode_directive(:pair_programmer, _),
    do: "You are acting as a pair programmer. Be concise, action-oriented, and practical."

  defp mode_directive(:coach, :bug),
    do: "You are coaching through a bug. Be patient, methodical, and encouraging."

  defp mode_directive(:coach, _),
    do: "You are coaching. Guide toward a small, clear next step."

  defp mode_directive(:explainer, _),
    do: "You are explaining a concept. Be clear and succinct — 2-4 sentences."

  defp mode_directive(:scribe, _),
    do: "You are in a conversational mode. Keep it natural and brief."

  defp mode_directive(:editor, _),
    do: "You are reviewing carefully. Point out concerns clearly but constructively."

  defp mode_directive(_, _),
    do: "Respond helpfully."

  defp wm_context([]), do: ""
  defp wm_context(lemmas), do: "Active concepts: #{Enum.join(lemmas, ", ")}."

  defp wm_item_term(item) when is_map(item) do
    payload = Map.get(item, :payload) || Map.get(item, "payload")

    payload_lemma =
      if is_map(payload) do
        Map.get(payload, :lemma) || Map.get(payload, "lemma")
      else
        nil
      end

    item_lemma = Map.get(item, :lemma) || Map.get(item, "lemma")
    id = Map.get(item, :id) || Map.get(item, "id")

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

  defp getv(mood, key) do
    case {get_in(mood, [:mood, key]), Map.get(mood, key)} do
      {v, _} when is_number(v) -> v * 1.0
      {_, v} when is_number(v) -> v * 1.0
      _ -> 0.5
    end
  end
end
