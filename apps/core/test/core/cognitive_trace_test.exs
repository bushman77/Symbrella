defmodule Core.CognitiveTraceTest do
  use ExUnit.Case, async: true

  alias Core.CognitiveTrace

  @event [:symbrella, :cognitive_trace, :stage]

  setup do
    id = {__MODULE__, self()}
    assert :ok = :telemetry.attach(id, @event, fn _event, measurements, metadata, pid ->
      send(pid, {:observed, measurements, metadata})
    end, self())

    on_exit(fn -> :telemetry.detach(id) end)
    :ok
  end

  test "actual step returns unchanged result and emits correlated execution receipts" do
    result =
      CognitiveTrace.with_trace("turn-abcdef12", fn ->
        CognitiveTrace.step(%{sentence: "private"}, :phase_1_input, fn si ->
          Map.put(si, :processed, true)
        end)
      end)

    assert result == %{sentence: "private", processed: true}

    assert_receive {:observed, %{duration_us: 0}, %{trace_id: "turn-abcdef12",
                                                     stage: :turn, status: :started}}
    assert_receive {:observed, %{duration_us: step_us}, %{trace_id: "turn-abcdef12",
                                                          stage: :phase_1_input, status: :ok,
                                                          evidence: %{}}}
    assert is_integer(step_us) and step_us >= 0
    assert_receive {:observed, %{duration_us: turn_us}, %{trace_id: "turn-abcdef12",
                                                          stage: :turn, status: :ok}}
    assert is_integer(turn_us) and turn_us >= 0
  end

  test "degraded comprehension remains evidence metadata rather than a stage exception" do
    CognitiveTrace.with_trace("turn-aabbccdd", fn ->
      CognitiveTrace.stage(:comprehension_summary, fn ->
        %{comprehension: %{degraded?: true, understood: [], uncertain: [:a, :b],
                           unresolved: [:x]}}
      end)
    end)

    assert_receive {:observed, _, %{status: :started}}
    assert_receive {:observed, _, %{stage: :comprehension_summary, status: :ok,
                                   evidence: evidence}}

    assert evidence.degraded == true
    assert evidence.understood == 0
    assert evidence.uncertain == 2
    assert evidence.unresolved == 1
    refute Map.has_key?(evidence, :sentence)
  end

  test "comprehension diagnosis emits causes and verification boundaries without input text" do
    summary = %{
      degraded?: true,
      reasons: [:weak_decision_rate_high, :fallback_rate_high],
      understood: [], uncertain: ["PRIVATE TERM"], unresolved: [],
      lexical_meanings: [],
      stats: %{total: 3, weak_rate: 0.666667, fallback_rate: 0.333333,
               unresolved_content: 0}
    }
    CognitiveTrace.with_trace("turn-ca55e0ab", fn ->
      CognitiveTrace.stage(:comprehension_summary, fn ->
        %{
          sentence: "PRIVATE UTTERANCE",
          source: :user, session_id: "PRIVATE SESSION",
          comprehension: summary,
          structural_competition: %{status: :preferred},
          sentence_meaning: %{status: :provisional},
          integration_audit: %{status: :consistent},
          lifg_feedback: %{mode: :bounded_reanalysis, applied?: false,
                           applied_revisions: [%{from: "PRIVATE ID"}]}
        }
      end)
    end)

    assert_receive {:observed, _, %{stage: :turn, status: :started}}
    assert_receive {:observed, _, %{stage: :comprehension_summary, status: :ok,
                                   evidence: diagnosis}}
    assert diagnosis.degraded == true
    assert diagnosis.understood == 0
    assert diagnosis.uncertain == 1
    assert diagnosis.unresolved == 0
    assert diagnosis.reasons == [:weak_decision_rate_high, :fallback_rate_high]
    assert diagnosis.choice_count == 3
    assert diagnosis.weak_rate == 0.666667
    assert diagnosis.fallback_rate == 0.333333
    assert diagnosis.verified_lexical_count == 0
    assert diagnosis.structural_status == :preferred
    assert diagnosis.sentence_meaning_status == :provisional
    assert diagnosis.integration_status == :consistent
    assert diagnosis.feedback_mode == :bounded_reanalysis
    assert diagnosis.feedback_applied == false
    assert diagnosis.repair_hint == :review_lexical_evidence
    assert diagnosis.curiosity_gap_gate == :needs_unresolved_content
    refute inspect(diagnosis) =~ "PRIVATE"
    refute Map.has_key?(diagnosis, :sentence)
    refute Map.has_key?(diagnosis, :lexical_meanings)
    assert_receive {:observed, _, %{stage: :turn, status: :ok}}
  end

  test "mixed social-update receipts keep component order but never the utterance" do
    CognitiveTrace.with_trace("turn-f008abcd", fn ->
      CognitiveTrace.stage(:speech_act_binding, fn ->
        %{sentence: "PRIVATE hey whats new with you?",
          speech_act_frame: %{
            speech_act: :social_update_query,
            components: [:greeting, :social_update_query],
            source_text: "PRIVATE MESSAGE",
            lexical_evidence: %{witness: "PRIVATE SENSE"},
            sentence_meaning_verified?: false
          }}
      end)
    end)

    assert_receive {:observed, _, %{stage: :turn, status: :started}}
    assert_receive {:observed, _, %{stage: :speech_act_binding, status: :ok,
                                   evidence: evidence}}
    assert evidence ==
             %{speech_act: :social_update_query,
               social_components: [:greeting, :social_update_query]}
    refute inspect(evidence) =~ "PRIVATE"
    assert_receive {:observed, _, %{stage: :turn, status: :ok}}
  end

  test "an unrecognized social component combination is not promoted to an act" do
    CognitiveTrace.with_trace("turn-f008abcd", fn ->
      CognitiveTrace.stage(:speech_act_binding, fn ->
        %{speech_act_frame: %{
          speech_act: :social_update_query,
          components: [:greeting, :unknown_request],
          source_text: "PRIVATE"
        }}
      end)
    end)

    assert_receive {:observed, _, %{stage: :turn, status: :started}}
    assert_receive {:observed, _, %{stage: :speech_act_binding, status: :ok,
                                   evidence: %{speech_act: :not_detected}}}
    assert_receive {:observed, _, %{stage: :turn, status: :ok}}
  end

  test "exceptions preserve failure and restore trace isolation" do
    assert_raise ArgumentError, "intended", fn ->
      CognitiveTrace.with_trace("turn-aacc0011", fn ->
        CognitiveTrace.stage(:lifg_selection, fn -> raise ArgumentError, "intended" end)
      end)
    end

    assert_receive {:observed, _, %{stage: :turn, status: :started}}
    assert_receive {:observed, _, %{stage: :lifg_selection, status: :error}}
    assert_receive {:observed, _, %{stage: :turn, status: :error}}

    assert :not_traced == CognitiveTrace.stage(:phase_3_lexical, fn -> :not_traced end)
    refute_receive {:observed, _, _}, 25
  end

  test "executive receipts are correlated and exclude raw user and planner data" do
    CognitiveTrace.with_trace("turn-a1b2c3dd", fn ->
      CognitiveTrace.stage(:prefrontal_control, fn ->
        %{
          sentence: "PRIVATE INPUT",
          prefrontal: %{
            feedback_quality: :heuristic_proxy,
            signals: %{top_k: 4, branch_budget: 2, secret: "PRIVATE CONTROL"},
            planners: [
              %{planner: Brain.DLPFC.Policy, status: :produced, reason: :validated,
                proposed_keys: [:top_k, :secret],
                accepted_keys: [:top_k], superseded_keys: []},
              %{planner: Brain.FPC, status: :unavailable, reason: :module_not_loaded,
                proposed_keys: [], accepted_keys: [], superseded_keys: []}
            ]
          }
        }
      end)

      CognitiveTrace.stage(:action_selection, fn ->
        %{
          action_meta: %{
            selected: :answer_user,
            prefrontal_shadow: %{
              status: :evaluated, reason: :bounded_candidate_modulation,
              live_selected: :answer_user,
              shadow_selected: :ask_clarifying_question,
              changed?: true,
              applied_keys: [:branch_budget, :secret],
              unused_keys: [:top_k],
              applied_sources: [%{key: :branch_budget, planner: Brain.FPC}],
              adjustments: [
                %{action: :answer_user, baseline_score: 0.50,
                  shadow_score: 0.45, delta: -0.05,
                  applied_keys: [:secret]}
              ],
              conversation_text: "PRIVATE INPUT"
            }
          }
        }
      end)
    end)

    assert_receive {:observed, _, %{trace_id: "turn-a1b2c3dd",
                                    stage: :prefrontal_control, status: :ok,
                                    evidence: prefrontal}}
    assert prefrontal.feedback_quality == :heuristic_proxy
    assert prefrontal.controls == %{top_k: 4, branch_budget: 2}
    assert [%{region: :dlpfc, status: :produced, merged_keys: [:top_k]},
            %{region: :fpc, status: :unavailable}] = prefrontal.planners
    refute inspect(prefrontal) =~ "PRIVATE"

    assert_receive {:observed, _, %{trace_id: "turn-a1b2c3dd",
                                    stage: :action_selection, status: :ok,
                                    evidence: action}}
    assert action.live_action == :answer_user
    assert action.shadow_action == :ask_clarifying_question
    assert action.changed == true
    assert action.applied_keys == [:branch_budget]
    assert action.applied_sources == [%{key: :branch_budget, region: :fpc}]
    assert [%{action: :answer_user, baseline: 0.50, proposed: 0.45}] =
             action.adjustments
    refute inspect(action) =~ "PRIVATE"
  end

  test "post-response lineage is turn-scoped and excludes private decision data" do
    CognitiveTrace.with_trace("turn-0ddba11", fn ->
      CognitiveTrace.stage(:response_planning, fn ->
        %{
          sentence: "PRIVATE INPUT",
          response_text: "PRIVATE REPLY",
          decision_lineage: %{
            initial_action: :ask_clarifying_question,
            final_action: :answer_user,
            initial_safety_gate: :approved,
            final_safety_gate: :approved,
            reconciliation: :reconciled,
            reason: :social_intent_reconciliation,
            shadow_action: :observe_silently,
            shadow_status: :evaluated,
            shadow_applied_keys: [:utility_prior, :private_key],
            initial_command_statuses: [:deferred, :private_status],
            final_command_statuses: [],
            initial_decision_trace_id: "PRIVATE DECISION ID",
            response_text: "PRIVATE LEAK"
          }
        }
      end)
    end)

    assert_receive {:observed, _, %{stage: :turn, status: :started}}
    assert_receive {:observed, _, %{stage: :response_planning, status: :ok,
                                   trace_id: "turn-0ddba11", evidence: lineage}}
    assert lineage.initial_action == :ask_clarifying_question
    assert lineage.final_action == :answer_user
    assert lineage.reconciliation == :reconciled
    assert lineage.reason == :social_intent_reconciliation
    assert lineage.shadow_action == :observe_silently
    assert lineage.shadow_applied_keys == [:utility_prior]
    assert lineage.initial_command_statuses == [:deferred, :unknown]
    assert lineage.final_command_statuses == []
    assert lineage.delivery_status == :not_observed
    refute Map.has_key?(lineage, :initial_decision_trace_id)
    refute inspect(lineage) =~ "PRIVATE"
    assert_receive {:observed, _, %{stage: :turn, status: :ok}}
  end

  test "untraced calls do not emit executive metadata" do
    value = %{prefrontal: %{signals: %{top_k: 2}}}
    assert value == CognitiveTrace.stage(:prefrontal_control, fn -> value end)
    refute_receive {:observed, _, %{stage: :prefrontal_control}}, 25
  end
end
