defmodule Core.Semantic.EventFrames do
  @moduledoc """
  Small symbolic event-frame extraction from already-normalized Core state.

  This sits after intent and LIFG sense selection: it does not decide the response,
  it preserves structured meaning for later memory, action selection, and UI traces.
  """

  alias Core.Pipeline.Trace

  @medication_terms ~w(quetiapine seroquel medication medicine meds dose)

  @spec attach(map(), keyword()) :: map()
  def attach(si, _opts \\ [])

  def attach(%{} = si, _opts) do
    case health_support_frame(si) do
      nil ->
        si

      frame ->
        si
        |> Map.put(:symbolic_frame, frame)
        |> Trace.append(:event_frame,
          decision: :attached,
          reason: :health_support_semantics,
          scores: %{confidence: Map.get(frame, :confidence, 0.0)},
          meta: frame
        )
    end
  end

  def attach(other, _opts), do: other

  defp health_support_frame(si) do
    texts = text_sources(si)
    text = combined_text(texts)
    choices = lifg_choices(si)

    with true <- health_support_turn?(si, text),
         medication when is_binary(medication) <- medication_mention(si, text, choices) do
      forgot? = forgot_medication?(text, choices)
      sleep_inability? = sleep_inability?(text, choices)
      temporal_now? = temporal_now?(text, choices)
      negative? = negative_polarity?(text, choices) or sleep_inability?

      if forgot? or sleep_inability? do
        %{
          type: :health_support_event,
          subject: subject_for(text, choices),
          event: event_for(forgot?, medication),
          medication: medication,
          consequence: consequence_for(sleep_inability?),
          temporal_context: temporal_context_for(temporal_now?),
          domain: :health_support,
          polarity: polarity_for(negative?),
          confidence:
            frame_confidence(%{
              medication?: true,
              forgot?: forgot?,
              sleep_inability?: sleep_inability?,
              temporal_now?: temporal_now?,
              negative?: negative?
            })
        }
        |> drop_nil_values()
      end
    else
      _ -> nil
    end
  end

  defp health_support_turn?(si, text) do
    map_get(si, :intent) == :health_support or
      map_get(si, :topic_domain) in [:health_sleep_medication, :health_medication] or
      Regex.match?(~r/\b(quetiapine|seroquel|medication|medicine|meds|dose)\b/u, text)
  end

  defp medication_mention(si, text, choices) do
    medication_from_choices(choices) ||
      medication_from_fuzzy_corrections(si) ||
      medication_from_text(text)
  end

  defp medication_from_choices(choices) when is_list(choices) do
    Enum.find_value(choices, fn
      %{} = choice ->
        id = map_get(choice, :id)
        lemma = map_get(choice, :lemma)
        canonical = map_get(choice, :canonical)
        entity_type = map_get(choice, :entity_type)
        source = map_get(choice, :source) || map_get(choice, :src)

        medication? =
          medication_entity_id?(id) or
            entity_type in [:medication, "medication"] or
            source in [:medical_entity_fallback, "medical_entity_fallback"]

        if medication? do
          normalize(canonical || lemma || phrase_from_id(id))
        end

      _ ->
        nil
    end)
  end

  defp medication_from_choices(_), do: nil

  defp medication_from_fuzzy_corrections(si) do
    si
    |> map_get(:fuzzy_text, %{})
    |> map_get(:corrections, [])
    |> List.wrap()
    |> Enum.find_value(fn
      %{} = correction ->
        replacement = correction |> map_get(:replacement, "") |> normalize()
        original = correction |> map_get(:original, "") |> normalize()

        cond do
          replacement in @medication_terms -> replacement
          original in @medication_terms -> original
          true -> nil
        end

      _ ->
        nil
    end)
  end

  defp medication_from_text(text) do
    Enum.find(@medication_terms, fn term ->
      Regex.match?(~r/\b#{Regex.escape(term)}\b/u, text)
    end)
  end

  defp medication_entity_id?(id) when is_binary(id), do: String.contains?(id, "|medication")
  defp medication_entity_id?(_), do: false

  defp forgot_medication?(text, choices) do
    Regex.match?(~r/\b(forgot|forget|missed|skip(?:ped)?)\b/u, text) or
      choice_lemma?(choices, ~w(forgot forget missed skipped skip))
  end

  defp sleep_inability?(text, choices) do
    Regex.match?(
      ~r/\b(can'?t|cant|cannot|can\s+not|unable\s+to|not\s+able\s+to)\s+sleep\b/u,
      text
    ) or
      Regex.match?(~r/\b(can'?t|cant|cannot|can\s+not)\s+(?:fall|get)\s+(?:to\s+)?sleep\b/u, text) or
      Regex.match?(~r/\b(haven'?t|have\s+not)\s+been\s+able\s+to\s+sleep\b/u, text) or
      Regex.match?(~r/\b(trouble|difficulty|difficulties)\s+sleep(?:ing)?\b/u, text) or
      Regex.match?(~r/\bsleep\s+(?:trouble|difficulty|difficulties|problem|problems)\b/u, text) or
      (choice_lemma?(choices, ["sleep"]) and negative_polarity?(text, choices))
  end

  defp temporal_now?(text, choices) do
    Regex.match?(~r/\bnow\b/u, text) or choice_lemma?(choices, ["now"])
  end

  defp negative_polarity?(text, choices) do
    Regex.match?(~r/\b(not|cannot|can\s+not|can't|cant|unable|haven't|have\s+not)\b/u, text) or
      choice_lemma?(choices, ["not", "cannot", "unable"])
  end

  defp subject_for(text, choices) do
    if Regex.match?(~r/\b(i|me|my|mine)\b/u, text) or
         choice_lemma?(choices, ["i", "me", "my", "mine"]) do
      :user
    else
      :user
    end
  end

  defp event_for(true, medication) when is_binary(medication), do: :forgot_medication
  defp event_for(_, _), do: :health_disclosure

  defp consequence_for(true), do: :sleep_inability
  defp consequence_for(_), do: nil

  defp temporal_context_for(true), do: :now
  defp temporal_context_for(_), do: nil

  defp polarity_for(true), do: :negative
  defp polarity_for(_), do: nil

  defp frame_confidence(flags) when is_map(flags) do
    0.55
    |> add_if(Map.get(flags, :medication?), 0.15)
    |> add_if(Map.get(flags, :forgot?), 0.10)
    |> add_if(Map.get(flags, :sleep_inability?), 0.10)
    |> add_if(Map.get(flags, :temporal_now?), 0.05)
    |> add_if(Map.get(flags, :negative?), 0.05)
    |> min(0.95)
    |> Float.round(2)
  end

  defp add_if(score, true, amount), do: score + amount
  defp add_if(score, _, _amount), do: score

  defp text_sources(si) when is_map(si) do
    fuzzy = map_get(si, :fuzzy_text, %{})

    [
      map_get(fuzzy, :text),
      map_get(fuzzy, :normalized),
      map_get(fuzzy, :original),
      map_get(si, :primary_text),
      map_get(si, :keyword),
      map_get(si, :sentence),
      map_get(si, :text)
    ]
    |> Enum.map(&normalize/1)
    |> Enum.reject(&(&1 == ""))
    |> Enum.uniq()
  end

  defp text_sources(_), do: []

  defp combined_text(texts) when is_list(texts) do
    texts
    |> Enum.join(" ")
    |> normalize()
  end

  defp combined_text(_), do: ""

  defp lifg_choices(si) when is_map(si) do
    si
    |> map_get(:lifg_choices, [])
    |> List.wrap()
    |> Enum.filter(&is_map/1)
  end

  defp lifg_choices(_), do: []

  defp choice_lemma?(choices, terms) when is_list(choices) and is_list(terms) do
    terms = Enum.map(terms, &normalize/1)

    Enum.any?(choices, fn
      %{} = choice ->
        lemma =
          choice
          |> choice_text()
          |> normalize()

        lemma in terms

      _ ->
        false
    end)
  end

  defp choice_lemma?(_, _), do: false

  defp choice_text(choice) when is_map(choice) do
    map_get(choice, :lemma) ||
      map_get(choice, :canonical) ||
      map_get(choice, :norm) ||
      map_get(choice, :phrase) ||
      phrase_from_id(map_get(choice, :id)) ||
      ""
  end

  defp choice_text(_), do: ""

  defp normalize(text) when is_binary(text) do
    text
    |> String.downcase()
    |> String.replace(~r/[^\p{L}\p{N}'\s]/u, " ")
    |> String.replace(~r/\s+/u, " ")
    |> String.trim()
  end

  defp normalize(nil), do: ""
  defp normalize(text), do: text |> to_string() |> normalize()

  defp phrase_from_id(id) when is_binary(id) do
    id
    |> String.split("|", parts: 2)
    |> List.first()
    |> normalize()
  end

  defp phrase_from_id(_), do: ""

  defp map_get(map, key, default \\ nil)

  defp map_get(map, key, default) when is_map(map) and is_atom(key) do
    Map.get(map, key, Map.get(map, Atom.to_string(key), default))
  end

  defp map_get(map, key, default) when is_map(map) and is_binary(key) do
    Map.get(map, key, Map.get(map, String.to_atom(key), default))
  rescue
    ArgumentError -> Map.get(map, key, default)
  end

  defp map_get(_map, _key, default), do: default

  defp drop_nil_values(map), do: Map.reject(map, fn {_key, value} -> is_nil(value) end)
end
