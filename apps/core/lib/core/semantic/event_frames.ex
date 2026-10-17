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
    case health_support_frame(si) || location_question_frame(si) || obligation_frame(si) do
      nil ->
        si

      frame ->
        si
        |> Map.put(:symbolic_frame, frame)
        |> Trace.append(:event_frame,
          decision: :attached,
          reason: :structured_semantics,
          scores: %{confidence: Map.get(frame, :confidence, 0.0)},
          meta: frame
        )
    end
  end

  def attach(other, _opts), do: other

  @doc """
  Classify only the geographical relation requested by the user's question.
  This does not establish or verify any real-world geographic claim. The pure
  helper is also used by response review if a runtime context loses its frame.
  """
  @spec location_question(String.t()) :: map() | nil
  def location_question(sentence) when is_binary(sentence),
    do: location_question_frame(%{sentence: sentence})

  def location_question(_), do: nil

  # A syntactically clear location question is not a verified geographic fact.
  # Keep the predicate and antecedent explicit, without claiming an external
  # population or distance measurement exists.
  defp location_question_frame(si) do
    sentence =
      (Map.get(si, :sentence) || "")
      |> String.trim()
      |> String.replace(~r/\A[“"]+\s*/u, "")
      |> String.replace(~r/\s*[”"]+\z/u, "")
      |> String.trim()
      |> String.replace(~r/^\s*no[,]?\s+it\s+is\s+not[.!]?\s+(?=how\s+far\s+is\s+it\s+from\b)/iu, "")

    reference = Map.get(si, :discourse_reference) || %{}
    subject = Map.get(reference, :antecedent)
    named_comparison = named_population_comparison(sentence)

    cond do
      match =
          Regex.run(
            ~r/^\s*how\s+far\s+is\s+it\s+from\s+([\p{L}][\p{L}\p{M}\s,.'-]{1,78})\s*[?.!]?\s*$/iu,
            sentence
          ) ->
        [_, target] = match

        %{
          type: :location_question,
          domain: :geography,
          relation: :proximity,
          subject: subject,
          target: clean_entity(target),
          antecedent_source: Map.get(reference, :source),
          unresolved_roles: if(is_nil(subject), do: [:subject], else: []),
          grounding_status: :unverified,
          confidence: if(is_binary(subject), do: 0.9, else: 0.6)
        }

      # Explicitly named comparisons establish the two referents directly.
      # Parsing a question is not evidence that either population is known.
      is_list(named_comparison) ->
        %{
          type: :location_question,
          domain: :geography,
          relation: :population,
          comparison: :greater,
          subjects: named_comparison,
          antecedent_source: :explicit_user_question,
          unresolved_roles: [],
          grounding_status: :unverified,
          confidence: 0.9
        }

      # A comparison is a relation over the previously resolved entities.
      # The interpretation alone supplies no population values or ranking.
      Map.get(reference, :source) == :wm_comprehension and
          is_list(Map.get(reference, :antecedents)) and
          length(Map.get(reference, :antecedents)) == 2 and
          Regex.match?(
            ~r/^\s*which\s+(?:(?:of\s+)?(?:those|these|the)\s+)?(?:cities|city|places|one)\s+has\s+the\s+(?:larger|greater|higher)\s+population\s*[?.!]*\s*$/iu,
            sentence
          ) ->
        %{
          type: :location_question,
          domain: :geography,
          relation: :population,
          comparison: :greater,
          subjects: Map.get(reference, :antecedents),
          antecedent_source: :wm_comprehension,
          unresolved_roles: [],
          grounding_status: :unverified,
          confidence: 0.85
        }

      # Plural follow-ups can use the preceding same-session comprehension.
      # These are discourse referents, NOT population claims or verified facts.
      Map.get(reference, :source) == :wm_comprehension and
          is_list(Map.get(reference, :antecedents)) and
          length(Map.get(reference, :antecedents)) >= 2 and
          Regex.match?(
            ~r/^\s*what\s+are\s+(?:the\s+)?populations?\s+of\s+(?:each|both)\s*[?.!]*\s*$/iu,
            sentence
          ) ->
        %{
          type: :location_question,
          domain: :geography,
          relation: :population,
          subjects: Enum.take(Map.get(reference, :antecedents), 4),
          antecedent_source: :wm_comprehension,
          unresolved_roles: [],
          grounding_status: :unverified,
          confidence: 0.8
        }

      # Directly named places do not require discourse antecedent recovery.
      # This creates only a question frame; coordinates must still be sourced
      # and matched to both entities before any numeric claim may be returned.
      match =
          Regex.run(
            ~r/^\s*how\s+far\s+is\s+([\p{L}][\p{L}\p{M}\s,.'-]{1,78}?)\s+from\s+([\p{L}][\p{L}\p{M}\s,.'-]{1,78})\s*[?.!]?\s*$/iu,
            sentence
          ) ->
        [_, named_subject, named_target] = match

        %{
          type: :location_question,
          domain: :geography,
          relation: :proximity,
          subject: clean_entity(named_subject),
          target: clean_entity(named_target),
          antecedent_source: :explicit_user_question,
          unresolved_roles: [],
          grounding_status: :unverified,
          confidence: 0.9
        }

      match =
          Regex.run(
            ~r/^\s*is\s+it\s+close\s+to\s+(\p{Lu}[\p{L}\p{M}\s,.'-]{1,78})\s*\?\s*$/iu,
            sentence
          ) ->
        [_, target] = match

        %{
          type: :location_question,
          domain: :geography,
          relation: :proximity,
          subject: subject,
          target: clean_entity(target),
          antecedent_source: Map.get(reference, :source),
          unresolved_roles: if(is_nil(subject), do: [:subject], else: []),
          grounding_status: :unverified,
          confidence: if(is_binary(subject), do: 0.9, else: 0.6)
        }

      Map.get(reference, :source) == :recent_geographic_entity and
          (Regex.match?(~r/^\s*what\s+is\s+[\p{L}][\p{L}\p{M}\s,.'-]{1,78}\s*\?\s*$/iu, sentence) or
             Regex.match?(~r/^\s*(?:that|this)\s+is\s+not\s+where\b/iu, sentence)) ->
        %{
          type: :location_question,
          domain: :geography,
          relation: :identity,
          subject: subject,
          antecedent_source: :recent_geographic_entity,
          unresolved_roles: [],
          grounding_status: :unverified,
          confidence: 0.9
        }

      match =
          Regex.run(
            ~r/^\s*(?:what(?:'s|\s+is)\s+the\s+population\s+of|how\s+many\s+people\s+live\s+in)\s+([\p{L}][\p{L}\p{M}\s,.'-]{1,78})\s*[?.!]*\s*$/iu,
            sentence
          ) ->
        [_, named_subject] = match

        %{
          type: :location_question,
          domain: :geography,
          relation: :population,
          subject: clean_entity(named_subject),
          antecedent_source: :explicit_user_question,
          unresolved_roles: [],
          grounding_status: :unverified,
          confidence: 0.9
        }

      Map.get(reference, :source) == :recent_population_recheck ->
        %{
          type: :location_question,
          domain: :geography,
          relation: :population,
          subject: subject,
          antecedent_source: :recent_population_recheck,
          freshness: :latest_available,
          unresolved_roles: if(is_binary(subject), do: [], else: [:subject]),
          grounding_status: :unverified,
          confidence: if(is_binary(subject), do: 0.9, else: 0.6)
        }

      Regex.match?(~r/^\s*what\s+is\s+its\s+population\s*\?\s*$/iu, sentence) ->
        %{
          type: :location_question,
          domain: :geography,
          relation: :population,
          subject: subject,
          antecedent_source: Map.get(reference, :source),
          unresolved_roles: if(is_nil(subject), do: [:subject], else: []),
          grounding_status: :unverified,
          confidence: if(is_binary(subject), do: 0.9, else: 0.6)
        }

      true ->
        nil
    end
  end

  # Recognize only an explicit two-entity comparative construction. The
  # named operands are syntactic roles, not externally verified places.
  # Require a single "or" separator and name-like entities to avoid treating
  # ordinary discussion containing population as a comparison instruction.
  defp named_population_comparison(sentence) do
    case Regex.run(
           ~r/^\s*which\s+(?:(?:city|town|place|municipality)\s+)?(?:had|has)\s+(?:the\s+)?(?:larger|greater|higher)\s+population(?:\s+in\s+(?:19|20)\d{2})?\s*,?\s+(.+?)\s*[?.!]*\s*$/iu,
           sentence,
           capture: :all_but_first
         ) do
      [candidates] ->
        case Regex.split(~r/\s+or\s+/iu, candidates, trim: true) do
          [first, second] ->
            names =
              [first, second]
              |> Enum.map(&clean_entity/1)
              |> Enum.map(&String.trim(&1, " ,"))
              |> Enum.map(&String.replace(&1, ~r/\s*,\s*/u, ", "))

            if Enum.all?(names, &valid_named_place?/1) and
                 length(Enum.uniq_by(names, &String.downcase/1)) == 2 do
              names
            end

          _ ->
            nil
        end

      _ ->
        nil
    end
  end

  defp valid_named_place?(name) when is_binary(name) do
    String.length(name) in 3..100 and
      Regex.match?(~r/^\p{Lu}[\p{L}\p{M}\p{N}\s,.'’-]*$/u, name) and
      not Regex.match?(~r/\b(?:which|what|who|where|how|why)\b/iu, name)
  end

  defp valid_named_place?(_), do: false

  defp clean_entity(text) do
    text
    |> String.trim()
    |> String.trim_trailing(".")
    |> String.trim_trailing("?")
    |> String.replace(~r/\s+/u, " ")
  end

  # Recover a bounded agent/action proposition for recognized obligation.
  # Reject implicit subject attribution: only explicit "I've" is supported.
  defp obligation_frame(si) do
    tokens = List.wrap(Map.get(si, :tokens))
    choices = List.wrap(Map.get(si, :lifg_choices))

    si
    |> Map.get(:sentence_signatures, [])
    |> List.wrap()
    |> Enum.find_value(fn
      %{id: :have_verb_to_infinitive, roles: %{auxiliary: aux, verb: verb, complement: comp}} ->
        aux_token = Enum.find(tokens, &(map_get(&1, :index) == aux))
        aux_word = if is_map(aux_token), do: map_get(aux_token, :phrase), else: nil
        action_token = Enum.find(tokens, &(map_get(&1, :index) == comp))

        action =
          (if is_map(action_token), do: map_get(action_token, :phrase), else: "")
          |> normalize()
          |> String.replace(~r/[^\p{L}-]/u, "")

        obligation? =
          Enum.any?(choices, fn c ->
            map_get(c, :token_index) == verb and map_get(c, :id) == "got|verb|1"
          end)

        complement_verb? =
          Enum.any?(choices, fn c ->
            map_get(c, :token_index) == comp and
              String.contains?(to_string(map_get(c, :id) || ""), "|verb|")
          end)

        if is_binary(aux_word) and String.downcase(aux_word) == "i've" and
             obligation? and action != "" do
          %{
            type: :obligation_event,
            subject: :speaker,
            modality: :obligation,
            action: action,
            confidence: if(complement_verb?, do: 0.8, else: 0.5),
            uncertain_roles: if(complement_verb?, do: [], else: [:complement])
          }
        end

      _ -> nil
    end)
  end

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
    atom_key =
      try do
        String.to_existing_atom(key)
      rescue
        ArgumentError -> nil
      end

    if atom_key, do: Map.get(map, key, Map.get(map, atom_key, default)), else: Map.get(map, key, default)
  end

  defp map_get(_map, _key, default), do: default

  defp drop_nil_values(map), do: Map.reject(map, fn {_key, value} -> is_nil(value) end)
end
