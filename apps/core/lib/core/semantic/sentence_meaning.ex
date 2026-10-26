defmodule Core.Semantic.SentenceMeaning do
  @moduledoc """
  A conservative, sentence-level interpretation assembled from independently
  observed grammatical roles, ATL lexical winners and independently
  revalidated event projections.

  This is *not* a dictionary entry for a whole sentence. Recognized question
  and participant-assertion frames become compositional propositions with
  explicit epistemic status. Unrecognized sentences retain an absent definition.
  No BrainCell IDs, external facts, or sense confidences are invented.
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

          if atl && is_map(roles), do: compose_question(frame, atl, frames, tokens, sentence), else: nil

        _ -> nil
      end)

    # Preserve exactly what the user wrote; token reconstruction is only a
    # fallback for maps that lack a sentence field.
    question =
      if is_map(question) do
        question
        |> Map.put(:proposition, Core.Semantic.Proposition.build(si, question))
        |> then(fn meaning ->
          if get_in(meaning, [:proposition, :content, :complement, :argument, :type]) ==
               :unparsed_topic do
            Map.update(meaning, :unresolved, [:topic_internal_structure],
              &Enum.uniq(&1 ++ [:topic_internal_structure]))
          else
            meaning
          end
        end)
        |> then(fn meaning ->
          if is_binary(sentence), do: Map.put(meaning, :sentence, sentence), else: meaning
        end)
      else
        question
      end

    question || object_question(si, sentence, frames, constituents, tokens) ||
      participant_meaning(si, sentence) || %{
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

  @doc """
  Read-only explanation of why a resolved WH-topic frame may fail composition.
  This diagnostic never promotes a frame into a sentence meaning.
  """
  def question_diagnostic(%{} = si) do
    frames = List.wrap(Map.get(si, :sentence_signatures))
    slate = Map.get(si, :atl_slate) || %{}
    constituents = List.wrap(Map.get(slate, :constituents))
    indexed = indexed_base_tokens(Map.get(si, :tokens, []))

    case Enum.find(frames, &(Map.get(&1, :id) == :wh_aux_topic_question)) do
      nil ->
        %{status: :no_question_signature}

      %{span: {first, last}, roles: roles} when is_map(roles) ->
        atl = Enum.find(constituents, fn constituent ->
          Map.get(constituent, :id) == :wh_aux_topic_question and
            Map.get(constituent, :span) == {first, last} and
            Map.get(constituent, :status) == :resolved
        end)

        required = [:wh, :auxiliary, :subject, :predicate, :preposition]
        missing_frame = Enum.reject(required, &(is_integer(Map.get(roles, &1))))
        atl_roles = if is_map(atl), do: Map.get(atl, :roles) || %{}, else: %{}
        missing_atl = Enum.reject(required, fn key ->
          match?(%{sense_id: id} when is_binary(id), Map.get(atl_roles, key))
        end)
        prep = Map.get(roles, :preposition)
        topic_span =
          if is_integer(prep), do: last - prep, else: nil

        surface_gaps =
          if is_map(atl) and missing_frame == [] and missing_atl == [] and last > prep and topic_span <= @max_topic_tokens do
            indices = Enum.map(required, &Map.fetch!(roles, &1)) ++
              Enum.to_list((prep + 1)..last)
            Enum.filter(indices, &(not match?({:ok, _}, surface_at(indexed, &1))))
          else
            []
          end

        cond do
          is_nil(atl) -> %{status: :atl_constituent_unresolved}
          missing_frame != [] -> %{status: :missing_grammar_roles, roles: missing_frame}
          missing_atl != [] -> %{status: :missing_atl_role_senses, roles: missing_atl}
          not (is_integer(prep) and last > prep and topic_span <= @max_topic_tokens) ->
            %{status: :topic_span_out_of_bounds, span_tokens: topic_span}
          surface_gaps != [] and
              match?({:ok, _}, question_topic_surface(indexed, prep + 1, last, Map.get(si, :sentence))) ->
            %{status: :punctuation_gap_witnessed, indices: Enum.take(surface_gaps, 16)}
          surface_gaps != [] ->
            %{status: :missing_token_surfaces,
              indices: Enum.take(surface_gaps, 16),
              available_indices: indexed |> Map.keys() |> Enum.sort() |> Enum.take(64),
              missing_context: Enum.map(Enum.take(surface_gaps, 16), fn index ->
                left = Map.get(indexed, index - 1)
                right = Map.get(indexed, index + 1)

                between =
                  if is_binary(left) and is_binary(right) and is_binary(Map.get(si, :sentence)) do
                    expression = Regex.compile!(
                      Regex.escape(left) <> "(.{0,80}?)" <> Regex.escape(right), "ius"
                    )

                    case Regex.run(expression, Map.get(si, :sentence)) do
                      [_, gap] -> gap
                      _ -> nil
                    end
                  else
                    nil
                  end

                %{index: index, preceding: left, following: right,
                  original_between: between}
              end)}
          true ->
            %{status: :eligible_for_composition, note: :inspect_remaining_guards}
        end

      _ ->
        %{status: :invalid_question_signature}
    end
  end

  def question_diagnostic(_), do: %{status: :invalid_input}

  # WH + auxiliary + determined subject + verb asks for the unbound object.
  # This is a question about a relation, NOT evidence that the action occurred.
  # The complete Core syntax signature, resolved ATL roles and independent POS
  # adjudication must agree before we issue a structural interpretation.
  @object_question_evidence [:wh_fronted, :auxiliary_inversion,
    :bounded_determiner_surface, :attested_nominal_subject,
    :lexical_verb_available, :question_punctuation, :contiguous_surface]

  defp object_question(si, sentence, frames, constituents, indexed)
       when is_binary(sentence) and is_list(frames) and is_list(constituents) and
              is_map(indexed) do
    if String.ends_with?(String.trim(sentence), "?") and
         Enum.sort(Map.keys(indexed)) == Enum.to_list(0..4) do
      questions =
        Enum.filter(frames, fn
          %{id: :wh_aux_object_question, source: :core_syntax,
            span: {0, 4}, roles: %{
              wh: 0, auxiliary: 1, subject_determiner: 2,
              subject_head: 3, predicate: 4}, evidence: evidence}
          when is_list(evidence) ->
            Enum.all?(@object_question_evidence, &(&1 in evidence))

          _ -> false
        end)

      case questions do
        [frame] -> compose_object_question(si, sentence, frame, constituents, indexed)
        _ -> nil
      end
    end
  end

  defp object_question(_, _, _, _, _), do: nil

  defp normalize_object_question(sentence) do
    sentence
    |> String.trim()
    |> String.trim_trailing("?")
    |> String.trim()
    |> String.downcase()
  end

  defp compose_object_question(si, sentence, _frame, constituents, indexed) do
    question_constituents = Enum.filter(constituents, fn
      %{id: :wh_aux_object_question, span: {0, 4}, status: :resolved,
        unresolved_roles: []} -> true
      _ -> false
    end)

    role_competition =
      case Map.get(si, :role_competition) do
        %{} = roles -> roles
        _ -> %{}
      end

    grammar_roles =
      case Map.get(role_competition, :roles) do
        %{} = roles -> roles
        _ -> %{}
      end

    with [atl] <- question_constituents,
         true <- Map.get(role_competition, :source) ==
                   :core_syntax_and_attested_dictionary_pos,
         %{status: :supported, pos: "noun", role: :subject_head,
           basis: :determined_subject_in_wh_aux_question} <- Map.get(grammar_roles, 3),
         %{status: :supported, pos: "verb", role: :predicate,
           basis: :predicate_in_wh_aux_question} <- Map.get(grammar_roles, 4),
         %{wh: wh, auxiliary: auxiliary, subject_determiner: determiner,
           subject_head: subject, predicate: predicate} <- atl.roles,
         true <- Enum.all?([wh, auxiliary, determiner, subject, predicate], fn
           %{token_index: index, sense_id: id} when is_integer(index) and
             is_binary(id) -> not String.ends_with?(id, "|fallback")
           _ -> false
         end),
         true <- wh.token_index == 0 and auxiliary.token_index == 1 and
                   determiner.token_index == 2 and subject.token_index == 3 and
                   predicate.token_index == 4,
         true <- object_roles_agree_with_winners?(si, atl.roles),
         {:ok, wh_surface} <- surface_at(indexed, 0),
         {:ok, auxiliary_surface} <- surface_at(indexed, 1),
         {:ok, subject_surface} <- surface_range(indexed, 2, 3),
         {:ok, predicate_surface} <- surface_at(indexed, 4),
         true <- String.downcase(wh_surface) in ["what", "who"],
         true <- String.downcase(auxiliary_surface) in ["do", "does", "did"],
         {:ok, reconstructed} <- surface_range(indexed, 0, 4),
         true <- String.downcase(reconstructed) == normalize_object_question(sentence) do
      roles = %{
        question_word: %{surface: wh_surface, token_index: 0, sense_id: wh.sense_id},
        auxiliary: %{surface: auxiliary_surface, token_index: 1,
          sense_id: auxiliary.sense_id},
        subject: %{surface: subject_surface, span: {2, 3},
          determiner_sense_id: determiner.sense_id,
          head_sense_id: subject.sense_id,
          grounding_status: :structurally_bound_not_lexically_verified},
        predicate: %{surface: predicate_surface, token_index: 4,
          sense_id: predicate.sense_id,
          grounding_status: :selected_not_contextually_verified},
        object: %{role: :patient, status: :unbound}
      }

      meaning = %{
        version: 1,
        sentence: sentence,
        definition: "The speaker asks for the object of predicate '#{predicate_surface}' " <>
          "with subject '#{subject_surface}'.",
        kind: :wh_object_question,
        status: :structurally_interpreted,
        speech_act: :request_information,
        source_frame: :wh_aux_object_question,
        roles: roles,
        unresolved: [:patient],
        grounding: %{
          grammatical_roles: :independent_syntax_and_atl_agreement,
          exact_lexical_meanings: :not_verified,
          event_occurrence: :not_asserted,
          external_facts: :not_checked
        },
        source: :sentence_signature_and_atl
      }

      Map.put(meaning, :proposition, Core.Semantic.Proposition.build(si, meaning))
    else
      _ -> nil
    end
  end

  # A constituent cannot independently assign BrainCell IDs. Each role
  # must match both ATL's token-level winner and the final lexical selection.
  defp object_roles_agree_with_winners?(si, roles) do
    atl = Map.get(si, :atl_slate) || %{}
    winners = Map.get(atl, :winners)
    selections =
      case Map.get(si, :semantic_choices) do
        selected when is_list(selected) -> selected
        _ -> Map.get(si, :lifg_choices)
      end

    is_list(winners) and is_list(selections) and
      Enum.all?(roles, fn
        {_role, %{token_index: index, sense_id: sense_id}} ->
          Enum.count(winners, fn winner ->
            Map.get(winner, :token_index) == index and Map.get(winner, :id) == sense_id
          end) == 1 and
            Enum.count(selections, fn selected ->
              Map.get(selected, :token_index) == index and
                (Map.get(selected, :id) || Map.get(selected, :chosen_id)) == sense_id
            end) == 1

        _ -> false
      end)
  end

  # A validated participant event provides structural sentence understanding,
  # not an externally verified fact. Keep this representation separate from
  # lexical :understood and from any persistence acknowledgement.
  defp participant_meaning(si, sentence) when is_binary(sentence) do
    case EpisodicProjection.validated_participant(si) do
      %{source_frame: frame, relation: relation,
        value: patient, participants: %{
          "actor" => actor, "predicate" => lemma,
          "surface_predicate" => surface, "patient" => stored_patient,
          "tense" => tense
        }} = event
        when frame in [:finite_present_svo, :finite_past_svo] ->
        if ((frame == :finite_present_svo and tense == "present") or
             (frame == :finite_past_svo and tense == "past")) and
             relation == "verb_" <> lemma and patient == stored_patient and
             participant_surface?([actor, lemma, surface, patient]) do
          roles = %{
            actor: surface_role(actor),
            predicate: %{surface: surface, lemma: lemma,
              grounding_status: :selected_not_contextually_verified},
            patient: surface_role(patient)
          }

          participant_frame(sentence, event, :action, roles,
            "The speaker reports that #{actor} #{surface} #{patient}.")
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

          participant_frame(sentence, event, :spatial, roles,
            "The speaker reports that #{actor} is #{predicate} #{position} #{location}.")
        end

      _ ->
        nil
    end
  end

  defp participant_meaning(_, _), do: nil

  defp participant_frame(sentence, event, kind, roles, definition) do
    grounding = %{
      grammatical_roles: :revalidated_episodic_projection,
      exact_lexical_meanings: :not_verified,
      external_facts: :not_checked
    }

    meaning = %{
      version: 2,
      sentence: sentence,
      kind: :participant_assertion,
      event_type: kind,
      source_frame: event.source_frame,
      status: :structurally_interpreted,
      speech_act: :assertion,
      definition: definition,
      roles: roles,
      polarity: :positive,
      grounding: grounding,
      source: :revalidated_episodic_projection
    }

    Map.put(meaning, :proposition, Core.Semantic.Proposition.build(%{}, meaning))
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
         atl, signatures, indexed, sentence
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
         {:ok, topic} <- question_topic_surface(indexed, prep + 1, last, sentence),
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

  defp compose_question(_, _, _, _, _), do: nil

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

  # Canonical token indices can contain a removed punctuation token.
  # Never skip an unknown word: each single missing position needs an
  # independently visible punctuation between its two neighbors.
  defp question_topic_surface(indexed, first, last, sentence)
       when is_integer(first) and is_integer(last) and last >= first and
              last - first < @max_topic_tokens and is_binary(sentence) do
    indices = Enum.to_list(first..last)
    present = Enum.filter(indices, &Map.has_key?(indexed, &1))
    gaps = indices -- present

    valid_gaps? =
      Enum.all?(gaps, fn index ->
        with {:ok, left} <- surface_at(indexed, index - 1),
             {:ok, right} <- surface_at(indexed, index + 1) do
          pattern = Regex.compile!(
            Regex.escape(left) <> ~S(\s*\p{P}+\s*) <> Regex.escape(right),
            "iu"
          )
          Regex.match?(pattern, sentence)
        else
          _ -> false
        end
      end)

    words = Enum.map(present, &surface_at(indexed, &1))

    if valid_gaps? and Enum.all?(words, &match?({:ok, _}, &1)) do
      {:ok, words |> Enum.map(fn {:ok, word} -> word end) |> Enum.join(" ")}
    else
      :error
    end
  end

  defp question_topic_surface(_, _, _, _), do: :error

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
