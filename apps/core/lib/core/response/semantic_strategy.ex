defmodule Core.Response.SemanticStrategy do
  @moduledoc """
  Evidence-bounded bridge from sentence meaning to a reusable response strategy.

  Different topics can share a response operation and realization family. This
  module never treats a sentence signature as a fact, retrieves an episode, or
  chooses a dictionary sense. Plans describe *how* to respond; existing native
  generators retain responsibility for authorizing response content.

  Unresolved questions are deliberately planned but not realized. Their
  missing evidence remains visible in response metadata.
  """

  alias Core.Response.{LexicalDefinition, SymbolicGeneration}
  alias Core.Semantic.Composition
  alias Core.Semantic.{DiscourseComposition, FrameUnifier, ProvisionalInterpretation,
    SentenceMeaning, SpeechActs, TemporalEventComposition,
    TemporalClaimReconciliation, TemporalClarificationResolution,
    TemporalMemoryRetrieval}

  @type strategy :: map()

  @spec select(map()) :: strategy() | :not_applicable
  def select(%{sentence: sentence, source: source} = si)
      when is_binary(sentence) and byte_size(sentence) <= 2048 and
           source in [:user, :prod, :test] do
    definition_strategy(si, sentence) ||
      sentence_meaning_strategy(si) ||
      social_strategy(si) ||
      provisional_strategy(si) ||
      :not_applicable
  end

  def select(_), do: :not_applicable

  @doc """
  Execute only an unchanged, same-turn, eligible strategy through an existing
  evidence-checking realizer. No free-form template interpolation occurs here.
  """
  @spec realize(strategy(), String.t(), map(), map(), map()) ::
          {:ok, String.t(), atom(), map()} | :not_applicable
  def realize(%{version: 1, status: :eligible, realizer: realizer} = plan,
              text, %{risk_bucket: :low} = features, %{action: action} = decision, si)
      when is_binary(text) and is_map(si) and
           action in [:answer, :offer_options, :act_first, :ask_first] do
    if Map.get(si, :sentence) == text and select(si) == plan do
      result =
        case realizer do
          :symbolic_generation ->
            SymbolicGeneration.generate(text, features, decision, si)

          :lexical_definition ->
            LexicalDefinition.generate(text, features, decision, si)

          _ ->
            :not_applicable
        end

      case result do
        {:ok, reply, reflection} when is_binary(reply) and is_map(reflection) ->
          source =
            if realizer == :lexical_definition,
              do: :symbolic_lexical_definition, else: :symbolic_generation

          {:ok, reply, source, Map.put(reflection, :strategy_operation, plan.operation)}

        _ ->
          :not_applicable
      end
    else
      :not_applicable
    end
  end

  def realize(_, _, _, _, _), do: :not_applicable

  defp definition_strategy(si, sentence) do
    case FrameUnifier.verified(si, sentence) do
      {:ok, %{operation: :define, target: %{lemma: term}, construction: construction}} ->
        plan(:answer_definition, :definition, :eligible, :lexical_definition,
          :semantic_request_frame,
          %{target: term, construction: construction, missing: [],
            semantic_verification: :dictionary_sense_unresolved})

      _ ->
        nil
    end
  end

  defp sentence_meaning_strategy(si) do
    attached = Map.get(si, :sentence_meaning)

    if is_map(attached) and attached == SentenceMeaning.build(si) do
      case attached do
        %{kind: :temporal_preference_recall_question, status: :provisional,
          source: :revalidated_temporal_preference_ledger,
          factual_authority?: false} ->
          case TemporalMemoryRetrieval.verified(si) do
            {:ok, %{temporal_evidence: %{
              kind: :recalled_temporal_speaker_preference,
              event_occurrence_verified?: false,
              actual_event_chronology_verified?: false,
              factual_authority?: false
            }}} ->
              plan(:answer_intended_temporal_order,
                :temporal_preference_recall,
                :eligible, :symbolic_generation,
                :revalidated_temporal_preference_ledger,
                %{missing: [:event_identity, :event_occurrence,
                  :actual_event_chronology], factual_authority?: false})

            _ -> nil
          end

        %{kind: :temporal_clarification_resolution, status: :provisional,
          source: :revalidated_temporal_clarification_answer,
          factual_authority?: false} ->
          case TemporalClarificationResolution.verified(si) do
            {:ok, %{chosen_report_seq: chosen, pending_request_seq: pending}}
                when is_integer(chosen) and is_integer(pending) ->
              plan(:acknowledge_intended_temporal_report,
                :temporal_clarification_resolution,
                :eligible, :symbolic_generation,
                :revalidated_temporal_clarification_answer,
                %{missing: [:event_identity, :event_occurrence,
                  :actual_event_chronology], factual_authority?: false})
            _ -> nil
          end

        %{kind: :temporal_claim_conflict_question, status: :provisional,
          source: :revalidated_temporal_claim_reconciliation,
          factual_authority?: false} ->
          case TemporalClaimReconciliation.verified(si) do
            {:ok, %{conflict_evidence: %{
              kind: :candidate_temporal_order_conflict,
              contradiction_verified?: false,
              factual_authority?: false
            }}} ->
              plan(:clarify_opposing_temporal_reports,
                :temporal_claim_reconciliation,
                :eligible, :symbolic_generation,
                :revalidated_temporal_claim_reconciliation,
                %{missing: [:event_identity, :event_occurrence,
                  :actual_event_chronology], factual_authority?: false})

            _ -> nil
          end

        %{kind: :reported_event_order_question, status: :provisional,
          source: :revalidated_discourse_report_order,
          factual_authority?: false} ->
          case TemporalEventComposition.verified(si) do
            {:ok, %{temporal_evidence: %{order: :reported_before,
                                         actual_event_order: :not_established}}} ->
              plan(:answer_earlier_report, :reported_event_order,
                :eligible, :symbolic_generation,
                :revalidated_discourse_report_order,
                %{missing: [:actual_event_chronology, :actor_identity],
                  factual_authority?: false})

            _ -> nil
          end

        %{kind: :anaphoric_spatial_report, status: :provisional,
          source: :revalidated_cross_turn_composition, factual_authority?: false} ->
          if Map.get(si, :intent) in [:unknown, :statement, :tell] do
            case DiscourseComposition.verified(si) do
              {:ok, %{kind: :anaphoric_spatial_report,
                       factual_authority?: false, unresolved: unresolved}}
                  when is_list(unresolved) ->
                plan(:clarify_anaphoric_spatial_report, :anaphoric_spatial_report,
                  :eligible, :symbolic_generation, :revalidated_cross_turn_composition,
                  %{missing: unresolved, factual_authority?: false})

              _ ->
                nil
            end
          end

        %{compositional_evidence: %{kind: :nominal_property,
            formally_verified?: true, factual_authority?: false}} ->
          if Map.get(si, :intent) in [:unknown, :statement, :tell] and
               not String.ends_with?(String.trim(Map.get(si, :sentence, "")), "?") do
            case Composition.verified(si) do
              {:ok, %{kind: :nominal_property,
                      construction: :stacked_adjective_noun,
                      quantifier_applied?: false, factual_authority?: false}} ->
                plan(:clarify_nominal_description, :nominal_fragment, :eligible,
                  :symbolic_generation, :revalidated_nominal_composition,
                  %{missing: [:dialogue_goal], factual_authority?: false})

              _ ->
                nil
            end
          end

        %{compositional_evidence: %{kind: :progressive_event_property,
            formally_verified?: true, factual_authority?: false}} ->
          if Map.get(si, :intent) in [:statement, :tell] do
            case Composition.verified(si) do
              {:ok, %{kind: :progressive_event_property, factual_authority?: false}} ->
                plan(:explain_reported_event, :event_description, :eligible,
                  :symbolic_generation, :revalidated_event_composition,
                  %{missing: [:real_world_event_grounding],
                    factual_authority?: false})

              _ ->
                nil
            end
          end

        %{kind: :participant_assertion, status: :structurally_interpreted,
          speech_act: :assertion, source: :revalidated_episodic_projection,
          event_type: kind, source_frame: frame}
        when kind in [:action, :spatial] ->
          plan(:acknowledge_report, :attributed_event, :eligible,
            :symbolic_generation, :sentence_meaning,
            %{event_type: kind, construction: frame, missing: [],
              factual_authority?: false})

        %{kind: :wh_object_question, status: :structurally_interpreted,
          speech_act: :request_information, source_frame: :wh_aux_object_question} ->
          plan(:answer_relation_object, :relational_answer, :needs_evidence, nil,
            :sentence_meaning,
            %{requested_role: :patient, missing: [:grounded_event, :patient],
              factual_authority?: false})

        %{kind: :wh_topic_question, status: :structurally_interpreted,
          speech_act: :request_information} ->
          plan(:answer_topic_question, :topic_answer, :needs_evidence, nil,
            :sentence_meaning,
            %{missing: [:topic_facts], factual_authority?: false})

        _ ->
          nil
      end
    end
  end

  defp social_strategy(si) do
    case {Map.get(si, :speech_act_frame), SpeechActs.from_si(si)} do
      {%{speech_act: act, addressee: addressee} = frame, {:ok, recomputed}}
      when act in [:greeting, :gratitude, :farewell, :wellbeing_query] and
           addressee in [:self, :unspecified] ->
        if frame == recomputed do
          plan(:respond_to_social_act, :social_reciprocity, :eligible,
            :symbolic_generation, :speech_act_frame,
            %{speech_act: act, missing: [], factual_authority?: false})
        end

      _ ->
        nil
    end
  end

  defp provisional_strategy(si) do
    case ProvisionalInterpretation.verified(si) do
      {:ok, %{operation: :locate_entity, requested_relation: :location,
              target: %{lemma: lemma}, missing: missing}} ->
        plan(:answer_location, :direct_relation, :needs_evidence, nil,
          :provisional_interpretation,
          %{target: lemma, relation: :location, temporal_scope: :unspecified,
            missing: missing, factual_authority?: false})

      {:ok, %{operation: :define_concept, target: %{lemma: lemma},
              missing: missing}} ->
        plan(:answer_definition, :definition, :needs_evidence, nil,
          :provisional_interpretation,
          %{target: lemma, missing: missing, factual_authority?: false})

      {:ok, %{operation: :ambiguous_topic, target: %{lemma: lemma},
              missing: missing}} ->
        plan(:clarify_topic, :clarification, :needs_evidence, nil,
          :provisional_interpretation,
          %{target: lemma, missing: missing, factual_authority?: false})

      _ ->
        nil
    end
  end

  defp plan(operation, family, status, realizer, source, details) do
    Map.merge(%{
      version: 1,
      operation: operation,
      realization_family: family,
      status: status,
      realizer: realizer,
      source: source
    }, details)
  end
end
