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

  test "untraced calls do not emit executive metadata" do
    value = %{prefrontal: %{signals: %{top_k: 2}}}
    assert value == CognitiveTrace.stage(:prefrontal_control, fn -> value end)
    refute_receive {:observed, _, %{stage: :prefrontal_control}}, 25
  end
end
