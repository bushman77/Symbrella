defmodule Core.Response.SymbolicGeneration do
  @moduledoc """
  Native, LLM-free realization from bounded post-comprehension evidence.

  "Hello" retains its selected canonical interjection witness. Other direct
  social acts remain explicitly surface-only. Participant assertions require
  independent revalidation of LIFG/ATL roles; their truth is not asserted.
  """

  alias Core.Semantic.Composition
  alias Core.Semantic.{EpisodicProjection, SentenceMeaning, SpeechActs}

  # Accept the intent labels the live classifier actually emits for
  # complete social turns. These labels by themselves never authorize output:
  # social_realization/2 still revalidates the whole-utterance SpeechActs frame.
  @social_intents [
    :greet, :greeting, :gratitude, :smalltalk, :feedback, :bye, :unknown,
    :tell, :statement, :ask_info, :question
  ]

  @spec generate(String.t(), map(), map(), map()) ::
          {:ok, String.t(), map()} | :not_applicable
  def generate(user_text, %{risk_bucket: :low, intent: intent}, %{action: action}, si)
      when is_binary(user_text) and is_map(si) and
             intent in @social_intents and action in [:answer, :offer_options, :act_first] do
    case social_realization(user_text, si) do
      {:ok, _, _} = realized -> realized
      :not_applicable ->
        case maybe_realize_compositional_event(user_text, intent, si) do
          {:ok, _, _} = event -> event
          :not_applicable -> maybe_realize_report(user_text, intent, si)
        end
    end
  end

  def generate(_text, _features, _decision, _si), do: :not_applicable

  defp social_realization(user_text, si) do
    case Map.get(si, :speech_act_frame) do
      %{
        version: 1, speech_act: act, speaker: :user,
        addressee: addressee, temporal_anchor: anchor,
        lexical_evidence: lexical, source_text: ^user_text
      } = frame
      when act in [:greeting, :gratitude, :farewell, :wellbeing_query] and
             addressee in [:self, :unspecified] and is_map(lexical) ->
        # Check the bound frame against the *current* sentence and current
        # lexical winners. A stale or forged frame cannot produce a reply.
        with {:ok, ^frame} <- SpeechActs.from_si(si),
             true <- supported_lexical_evidence?(act, lexical) do
          {:ok, realize(act, anchor), social_provenance(frame)}
        else
          _ -> :not_applicable
        end

      _ ->
        :not_applicable
    end
  end

  defp supported_lexical_evidence?(:greeting, %{
         level: :canonical_lifg_selection,
         status: :provisional,
         witness: %{id: "hello|interjection|1", token_index: 0}
       }), do: true

  defp supported_lexical_evidence?(_act, %{
         level: :surface_only, status: :provisional, witness: nil
       }), do: true

  defp supported_lexical_evidence?(_, _), do: false

  defp social_provenance(frame) do
    lexical = frame.lexical_evidence
    provenance =
      case {frame.speech_act, lexical.level} do
        {:greeting, :surface_only} -> :surface_greeting
        {_, :canonical_lifg_selection} -> :canonical_lifg_choice
        _ -> :surface_social_act
      end

    %{
      status: :symbolic_realized,
      speech_act: reciprocal_act(frame.speech_act),
      interpreted_speech_act: frame.speech_act,
      speaker: frame.speaker,
      addressee: frame.addressee,
      vocative: frame.vocative,
      temporal_anchor: frame.temporal_anchor,
      provenance: provenance,
      lexical_witness: Map.get(lexical, :witness),
      semantic_verified?: false,
      factual_claims?: false,
      llm_called?: false
    }
  end

  defp reciprocal_act(:greeting), do: :reciprocate_greeting
  defp reciprocal_act(:gratitude), do: :acknowledge_gratitude
  defp reciprocal_act(:farewell), do: :reciprocate_farewell
  defp reciprocal_act(:wellbeing_query), do: :respond_to_checkin

  defp realize(:greeting, :morning), do: "Good morning."
  defp realize(:greeting, :afternoon), do: "Good afternoon."
  defp realize(:greeting, :evening), do: "Good evening."
  defp realize(:greeting, _), do: "Hello."
  defp realize(:gratitude, _), do: "You're welcome."
  defp realize(:farewell, :night), do: "Good night."
  defp realize(:farewell, _), do: "Take care."
  defp realize(:wellbeing_query, _), do: "I'm here and ready to talk."

  # Render only the current-turn, proof-checked event PROPERTY. All words
  # below are closed-vocabulary terms; never interpolate untrusted input text
  # or promote the mathematical proof to a fact about the outside world.
  @event_actors %{bird: "bird", cat: "cat", dog: "dog"}
  @event_landmarks %{tree: "tree", table: "table", fence: "fence"}
  @event_activities %{sit: "sitting", sleep: "sleeping"}
  @event_static_relations %{
    on: "on", in: "in", under: "under", beside: "beside",
    above: "above", below: "below"
  }

  defp maybe_realize_compositional_event(user_text, intent, si)
       when intent in [:tell, :statement] do
    with true <- Map.get(si, :source) in [:user, :prod, :test],
         true <- Map.get(si, :intent) in [:tell, :statement],
         ^user_text <- Map.get(si, :sentence),
         true <- not String.ends_with?(String.trim(user_text), "?"),
         %{compositional_evidence: evidence} = meaning <-
           Map.get(si, :sentence_meaning),
         ^meaning <- SentenceMeaning.build(si),
         %{kind: :progressive_event_property, formally_verified?: true,
           factual_authority?: false, event_semantics: reported_semantics} <- evidence,
         {:ok, %{kind: :progressive_event_property, status: :derived,
           event_semantics: semantics, quantifier_applied?: false,
           factual_authority?: false} = proof} <- Composition.verified(si),
         true <- reported_semantics == semantics,
         {:ok, reply} <- describe_event(semantics) do
      {:ok, reply, %{
        status: :symbolic_realized,
        speech_act: :acknowledge_user_report,
        interpreted_speech_act: :assertion,
        provenance: :revalidated_event_composition,
        construction: proof.construction,
        proof_steps: length(proof.steps),
        compositional_proof_verified?: true,
        event_occurrence_verified?: false,
        response_is_memory_acknowledgment?: false,
        factual_claims?: false,
        llm_called?: false
      }}
    else
      _ -> :not_applicable
    end
  end

  defp maybe_realize_compositional_event(_text, _intent, _si),
    do: :not_applicable

  defp describe_event(%{
         aspect: :progressive,
         predicate: predicate,
         actor: %{kind: actor_kind, referent: :unresolved},
         spatial: %{relation: relation, landmark_kind: landmark_kind,
                    reading: reading, arrival: :not_established},
         event_existence: :not_asserted
       }) do
    with {:ok, actor} <- Map.fetch(@event_actors, actor_kind),
         {:ok, landmark} <- Map.fetch(@event_landmarks, landmark_kind) do
      case {predicate, relation, reading} do
        {:fly, :into, :directional} ->
          {:ok, "You're describing a #{actor} flying into a #{landmark}. " <>
            "The direction is expressed, but arrival and actual occurrence aren't established."}

        {activity, spatial_relation, :locative} when activity in [:sit, :sleep] ->
          with {:ok, word} <- Map.fetch(@event_activities, activity),
               {:ok, prep} <- Map.fetch(@event_static_relations, spatial_relation) do
            {:ok, "You're describing a #{actor} #{word} #{prep} a #{landmark}. " <>
              "That's a spatial description, not independent proof the event occurred."}
          else
            _ -> :not_applicable
          end

        _ ->
          :not_applicable
      end
    else
      _ -> :not_applicable
    end
  end

  defp describe_event(_), do: :not_applicable

  # No generic fallback to parroting text: only one verified participant event
  # may license a compositional response. Exact user report, LIFG/ATL
  # alignment, and the freshly recomputed meaning must all agree.
  defp maybe_realize_report(user_text, intent, si)
       when intent in [:tell, :statement, :unknown] do
    with true <- Map.get(si, :source) in [:user, :prod, :test],
         ^user_text <- Map.get(si, :sentence),
         %{version: 2, kind: :participant_assertion,
           status: :structurally_interpreted, speech_act: :assertion,
           polarity: :positive, source: :revalidated_episodic_projection,
           grounding: %{external_facts: :not_checked}} = meaning <-
             Map.get(si, :sentence_meaning),
         ^meaning <- SentenceMeaning.build(si),
         %{source_frame: source_frame} <- EpisodicProjection.validated_participant(si),
         ^source_frame <- Map.get(meaning, :source_frame),
         {:ok, reply} <- compose_report(meaning) do
      {:ok, reply, %{
        status: :symbolic_realized,
        speech_act: :acknowledge_user_report,
        interpreted_speech_act: :assertion,
        event_kind: meaning.event_type,
        source_frame: source_frame,
        provenance: :revalidated_episodic_projection,
        evidence_level: :structurally_interpreted_not_verified,
        semantic_verified?: false,
        event_occurrence_verified?: false,
        exact_verb_sense_verified?: false,
        response_is_memory_acknowledgment?: false,
        factual_claims?: false,
        llm_called?: false
      }}
    else
      _ -> :not_applicable
    end
  end

  defp maybe_realize_report(_text, _intent, _si), do: :not_applicable

  defp compose_report(%{
         event_type: :action, source_frame: frame,
         roles: %{actor: %{surface: actor},
                  predicate: %{surface: verb},
                  patient: %{surface: patient}}
       }) when frame in [:finite_present_svo, :finite_past_svo] do
    if safe_parts?([actor, verb, patient]),
      do: {:ok, "You're saying that #{actor} #{verb} #{patient}."},
      else: :not_applicable
  end

  defp compose_report(%{
         event_type: :spatial, source_frame: :progressive_spatial,
         roles: %{actor: %{surface: actor},
                  predicate: %{surface: verb},
                  position: %{surface: position},
                  location: %{surface: location}}
       }) do
    if safe_parts?([actor, verb, position, location]),
      do: {:ok, "You're describing #{actor} as #{verb} #{position} #{location}."},
      else: :not_applicable
  end

  defp compose_report(_), do: :not_applicable

  defp safe_parts?(parts) do
    Enum.all?(parts, fn
      word when is_binary(word) ->
        byte_size(word) <= 120 and
          Regex.match?(~r/\A[\p{L}\p{N}][\p{L}\p{N} '\-]*\z/u, word)
      _ -> false
    end)
  end
end
