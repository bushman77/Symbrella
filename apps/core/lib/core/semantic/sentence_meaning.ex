defmodule Core.Semantic.SentenceMeaning do
  @moduledoc """
  A conservative, sentence-level interpretation assembled from independently
  observed grammatical roles and ATL lexical winners.

  This is *not* a dictionary entry for a whole sentence. For recognized
  question frames, it records a compositional reading with its evidence.
  Unrecognized sentences retain an explicitly absent definition. No BrainCell
  IDs, external facts, or sense confidences are invented.
  """

  alias Core.Pipeline.Trace
  alias Core.Semantic.EpisodicProjection

  @max_topic_tokens 16

  @spec attach(map()) :: map()
  def attach(%{} = si) do
    meaning = build(si)

    si
    |> Map.put(:sentence_meaning, meaning)
    |> Trace.append(:sentence_meaning,
      decision: meaning.status,
      reason: :compositional_sentence_interpretation,
      meta: meaning
    )
  end

  def attach(other), do: other

  @spec build(map()) :: map()
  def build(%{} = si) do
    sentence = Map.get(si, :sentence)
    tokens = indexed_base_tokens(Map.get(si, :tokens, []))
    frames = List.wrap(Map.get(si, :sentence_signatures))
    slate = Map.get(si, :atl_slate) || %{}
    constituents = List.wrap(Map.get(slate, :constituents))

    question =
      Enum.find_value(frames, fn
        %{id: :wh_aux_topic_question, span: span, roles: roles} = frame ->
          atl =
            Enum.find(constituents, fn
              %{id: :wh_aux_topic_question, span: ^span, status: :resolved} -> true
              _ -> false
            end)

          if atl && is_map(roles), do: compose_question(frame, atl, frames, tokens), else: nil

        _ -> nil
      end)

    # Preserve exactly what the user wrote; token reconstruction is only a
    # fallback for maps that lack a sentence field.
    question =
      if is_map(question) do
        question
        |> Map.put(:proposition, Core.Semantic.Proposition.build(si, question))
        |> then(fn meaning ->
          if is_binary(sentence), do: Map.put(meaning, :sentence, sentence), else: meaning
        end)
      else
        question
      end

    question || participant_meaning(si, sentence) || %{
      version: 1,
      sentence: sentence,
      definition: nil,
      kind: :unknown,
      status: :not_yet_interpreted,
      source: :insufficient_verified_sentence_structure
    }
  end

  def build(_), do: %{
    version: 1, sentence: nil, definition: nil, kind: :unknown,
    status: :not_yet_interpreted, source: :invalid_input
  }

  # A validated participant event provides structural sentence understanding,
  # not an externally verified fact. Keep this representation separate from
  # lexical :understood and from any persistence acknowledgement.
  defp participant_meaning(si, sentence) when is_binary(sentence) do
    case EpisodicProjection.validated_participant(si) do
      %{source_frame: :finite_present_svo, relation: relation,
        value: patient, participants: %{
          "actor" => actor, "predicate" => lemma,
          "surface_predicate" => surface, "patient" => stored_patient,
          "tense" => "present"
        }} = event ->
        if relation == "verb_" <> lemma and patient == stored_patient and
             participant_surface?([actor, lemma, surface, patient]) do
          roles = %{
            actor: surface_role(actor),
            predicate: %{surface: surface, lemma: lemma,
              grounding_status: :selected_not_contextually_verified},
            patient: surface_role(patient)
          }

          participant_frame(sentence, event, :action, roles, %{
            predicate: %{surface: surface, lemma: lemma},
            arguments: %{actor: actor, patient: patient},
            modifiers: %{tense: :present}
          }, "The speaker reports that #{actor} #{surface} #{patient}.")
        end

      %{source_frame: :progressive_spatial, relation: "spatial_state",
        value: location, participants: %{
          "actor" => actor, "predicate" => predicate, "aspect" => "progressive",
          "position" => position, "location" => stored_location
        }} = event ->
        if location == stored_location and
             participant_surface?([actor, predicate, position, location]) do
          roles = %{
            actor: surface_role(actor),
            predicate: %{surface: predicate, aspect: :progressive,
              grounding_status: :attested_form_not_sense_verified},
            position: surface_role(position),
            location: surface_role(location)
          }

          participant_frame(sentence, event, :spatial, roles, %{
            predicate: %{surface: predicate, aspect: :progressive},
            arguments: %{actor: actor, location: location},
            modifiers: %{position: position}
          }, "The speaker reports that #{actor} is #{predicate} #{position} #{location}.")
        end

      _ ->
        nil
    end
  end

  defp participant_meaning(_, _), do: nil

  defp participant_frame(sentence, event, kind, roles, content, definition) do
    # Provenance describes the interpretation path, NOT a persistence claim.
    provenance = %{
      source: :user_statement,
      frame: event.source_frame,
      evidence: :candidate_revalidated_against_original_input,
      persistence: :not_claimed
    }

    grounding = %{
      grammatical_roles: :revalidated_episodic_projection,
      exact_lexical_meanings: :not_verified,
      external_facts: :not_checked
    }

    %{
      version: 2,
      sentence: sentence,
      kind: :participant_assertion,
      status: :structurally_interpreted,
      speech_act: :assertion,
      definition: definition,
      roles: roles,
      polarity: :positive,
      grounding: grounding,
      source: :revalidated_episodic_projection,
      proposition: %{
        version: 1,
        type: :participant_assertion,
        event_type: kind,
        epistemic_status: :user_reported_unverified,
        content: content,
        grounding: grounding,
        provenance: provenance
      }
    }
  end

  defp surface_role(surface),
    do: %{surface: surface, grounding_status: :structurally_bound}

  defp participant_surface?(values) do
    Enum.all?(values, fn value ->
      is_binary(value) and String.trim(value) != "" and byte_size(value) <= 160
    end)
  end

  defp compose_question(
         %{span: {start, last}, roles: roles},
         atl, signatures, indexed
       )
       when is_integer(start) and is_integer(last) and is_map(roles) and
              is_map(atl) and is_map(indexed) do
    with %{wh: wh, auxiliary: auxiliary, subject: subject,
           predicate: predicate, preposition: prep} <- roles,
         true <- Enum.all?([wh, auxiliary, subject, predicate, prep], &is_integer/1),
         true <- last > prep and last - prep <= @max_topic_tokens,
         %{wh: %{sense_id: wh_id},
           auxiliary: %{sense_id: aux_id},
           subject: %{sense_id: subject_id},
           predicate: %{sense_id: predicate_id},
           preposition: %{sense_id: prep_id}} <- atl.roles,
         true <- Enum.all?([wh_id, aux_id, subject_id, predicate_id, prep_id],
           &is_binary/1),
         {:ok, topic} <- surface_range(indexed, prep + 1, last),
         {:ok, wh_text} <- surface_at(indexed, wh),
         {:ok, aux_text} <- surface_at(indexed, auxiliary),
         {:ok, subject_text} <- surface_at(indexed, subject),
         {:ok, predicate_text} <- surface_at(indexed, predicate),
         {:ok, prep_text} <- surface_at(indexed, prep) do

      possessive =
        Enum.find_value(signatures, fn
          %{id: :possessive_noun_phrase,
            span: {owner_start, owner_end},
            surface_possessor: owner,
            roles: %{possessor: pos_index}}
          when is_binary(owner) and owner_start > prep and
               owner_end <= last and pos_index == owner_start ->
            %{possessor: owner, token_index: pos_index,
              relation: :genitive_nominal_modifier,
              grounding_status: :unverified}

          _ -> nil
        end)

      negation =
        case Map.get(roles, :negation) do
          n when is_integer(n) -> case surface_at(indexed, n) do {:ok, text} -> text; _ -> nil end
          _ -> nil
        end

      pred_modifier =
        case Map.get(roles, :predicate_modifier) do
          n when is_integer(n) -> case surface_at(indexed, n) do {:ok, text} -> text; _ -> nil end
          _ -> nil
        end

      wh_modifier =
        case Map.get(roles, :wh_modifier) do
          n when is_integer(n) -> case surface_at(indexed, n) do {:ok, text} -> text; _ -> nil end
          _ -> nil
        end

      definition =
        definition(%{
          question: wh_text, auxiliary: aux_text, subject: subject_text,
          predicate: predicate_text, preposition: prep_text, topic: topic,
          negation: negation, predicate_modifier: pred_modifier,
          wh_modifier: wh_modifier
        })

      negative_auxiliary? = negative_auxiliary?(aux_text)
      polarity = if(is_binary(negation) or negative_auxiliary?,
        do: :negative, else: :positive)

      operators = %{
        polarity: %{value: polarity, scope: :predicate,
          source: :sentence_signature_and_atl},
        auxiliary_negation?: negative_auxiliary?,
        negation: optional_role(roles, atl.roles, indexed, :negation),
        question_modifier: optional_role(roles, atl.roles, indexed, :wh_modifier),
        predicate_modifier:
          optional_role(roles, atl.roles, indexed, :predicate_modifier)
      }

      %{
        version: 1,
        sentence: original_sentence(indexed, start, last),
        definition: definition,
        kind: :wh_topic_question,
        status: :structurally_interpreted,
        speech_act: :request_information,
        polarity: polarity,
        operators: operators,
        roles: %{
          question_word: %{surface: wh_text, token_index: wh,
            sense_id: wh_id},
          auxiliary: %{surface: aux_text, token_index: auxiliary,
            sense_id: aux_id},
          subject: %{surface: subject_text, token_index: subject,
            sense_id: subject_id,
            reference: if(String.downcase(subject_text) == "you",
              do: :addressee, else: :unspecified)},
          predicate: %{surface: predicate_text, token_index: predicate,
            sense_id: predicate_id},
          preposition: %{surface: prep_text, token_index: prep,
            sense_id: prep_id},
          topic: %{surface: topic, span: {prep + 1, last},
            possession: possessive, grounding_status: :not_fully_verified}
        },
        unresolved: if(possessive, do: [:topic_possessor_lexical_grounding], else: []),
        grounding: %{
          grammatical_roles: :atl_verified,
          exact_lexical_meanings: :not_verified,
          topic_facts: :not_checked
        },
        source: :sentence_signature_and_atl
      }
    else
      _ -> nil
    end
  end

  defp compose_question(_, _, _, _), do: nil

  # Only expose optional modifiers when ATL resolved the same token index
  # and provided an actual sense ID. The surface is not a verified sense gloss.
  defp optional_role(roles, resolved, tokens, kind) do
    case {Map.get(roles, kind), Map.get(resolved, kind)} do
      {index, %{token_index: resolved_index, sense_id: id}}
      when is_integer(index) and index == resolved_index and is_binary(id) ->
        case surface_at(tokens, index) do
          {:ok, surface} -> %{
            surface: surface, token_index: index, sense_id: id,
            grounding_status: :selected_not_contextually_verified
          }
          _ -> nil
        end

      _ -> nil
    end
  end

  defp negative_auxiliary?(surface) when is_binary(surface) do
    word = String.downcase(surface)
    String.contains?(word, "n't") or String.contains?(word, "n’t") or
      word == "cannot"
  end

  defp negative_auxiliary?(_), do: false



  defp definition(%{question: question, auxiliary: aux, subject: subject,
                     predicate: predicate, preposition: prep, topic: topic} = parts) do
    subject_low = String.downcase(subject)
    wh_modifier = Map.get(parts, :wh_modifier)
    negation = Map.get(parts, :negation)
    modifier = Map.get(parts, :predicate_modifier)
    negative_aux? = String.contains?(aux, "n't") or String.contains?(aux, "n’t")
    is_negative = is_binary(negation) or negative_aux?

    if String.downcase(question) == "what" and
         subject_low in ~w(i you we they) do
      # Put the auxiliary back only for polarity; ordinary do-support carries
      # no separate lexical proposition. Do not try to inflect 3rd-person verbs.
      clause =
        if is_negative do
          Enum.reject([subject, aux, negation, modifier, predicate, prep, topic],
            &is_nil/1)
        else
          Enum.reject([subject, modifier, predicate, prep, topic], &is_nil/1)
        end
        |> Enum.join(" ")

      prefix = if is_binary(wh_modifier), do: "#{wh_modifier} ", else: ""
      "The speaker asks #{prefix}what #{clause}."
    else
      # A technical gloss is safer than guessing English agreement, scope
      # or the meaning of a verb with a different syntactic subject.
      "An information question about #{topic}, with subject '#{subject}' " <>
        "and predicate '#{predicate}' (relation '#{prep}')."
    end
  end

  defp indexed_base_tokens(tokens) when is_list(tokens) do
    tokens
    |> Enum.with_index()
    |> Enum.reduce(%{}, fn {token, fallback_index}, acc ->
      if is_map(token) and Map.get(token, :n, 1) == 1 do
        index = Map.get(token, :index, fallback_index)
        surface = Map.get(token, :phrase) || Map.get(token, :norm)

        if is_integer(index) and index >= 0 and is_binary(surface) do
          Map.put(acc, index, String.trim(surface))
        else
          acc
        end
      else
        acc
      end
    end)
  end

  defp indexed_base_tokens(_), do: %{}

  defp surface_at(indexed, index) do
    case Map.get(indexed, index) do
      s when is_binary(s) ->
        case clean_word(s) do
          "" -> :error
          cleaned -> {:ok, cleaned}
        end

      _ -> :error
    end
  end

  defp surface_range(indexed, first, last)
       when is_integer(first) and is_integer(last) and last >= first and
              last - first < @max_topic_tokens do
    results = for index <- first..last, do: surface_at(indexed, index)

    if Enum.all?(results, &match?({:ok, _}, &1)) do
      {:ok, results |> Enum.map(fn {:ok, s} -> s end) |> Enum.join(" ")}
    else
      :error
    end
  end

  defp surface_range(_, _, _), do: :error

  defp original_sentence(indexed, first, last) do
    first..last
    |> Enum.map(&Map.get(indexed, &1, ""))
    |> Enum.join(" ")
    |> String.trim()
  end

  defp clean_word(s) do
    s
    |> String.trim()
    |> String.replace(~r/^\p{P}+|\p{P}+$/u, "")
  end
end
