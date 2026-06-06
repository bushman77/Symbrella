defmodule Core.Response.SelfStateFeaturesTest do
  use ExUnit.Case, async: true

  alias Core.Response.SelfStateFeatures

  test "self_state/4 normalizes model signals and merges monitor, memory, and agency effects" do
    state =
      SelfStateFeatures.self_state(
        %{
          v: 2,
          uncertainty: 0.8,
          confidence: 1.2,
          stability: 0.2,
          cognitive_load: 0.9,
          focus: "clarify"
        },
        %{
          status: :warning,
          warnings: [%{kind: :high_cognitive_load}],
          recovery_suggestions: [:reduce_scope]
        },
        %{
          memories: [%{kind: "self_monitor_warning"}],
          warning_kinds: [:high_cognitive_load],
          recovery_suggestions: [:stabilize_before_acting]
        },
        %{
          v: 1,
          event_count: 2,
          reasons: [:previous_repair],
          stats: %{ok: 1},
          effects: [:prefer_repair]
        }
      )

    assert state.v == 2
    assert state.confidence == 1.0
    assert state.uncertainty == 0.8
    assert state.stability == 0.2
    assert state.cognitive_load == 0.9
    assert state.focus == :clarify

    assert :hedge_under_uncertainty in state.effects
    assert :prefer_repair in state.effects
    assert :reduce_scope in state.effects
    assert :ask_clarifying_question in state.effects
    assert :stabilize_before_acting in state.effects

    assert state.self_monitor.warning_kinds == [:high_cognitive_load]
    assert state.self_memory_recall.memory_count == 1
    assert state.agency_memory.event_count == 2
  end

  test "append_curiosity_probe/5 appends one uncertainty question when policy action allows it" do
    features = %{
      self_state: %{
        uncertainty: 0.8,
        focus: :clarify,
        effects: [:ask_clarifying_question]
      }
    }

    {text, probe} =
      SelfStateFeatures.append_curiosity_probe(
        "I can work from that.",
        features,
        %{action: :offer_options},
        %{guardrail?: false},
        nil
      )

    assert text == "I can work from that. What detail would reduce the uncertainty most?"

    assert probe == %{
             text: "What detail would reduce the uncertainty most?",
             reason: :uncertainty_reduction
           }
  end

  test "append_curiosity_probe/5 does not append for guardrails, skills, or existing questions" do
    features = %{
      self_state: %{uncertainty: 0.9, focus: :clarify, effects: [:ask_clarifying_question]}
    }

    decision = %{action: :offer_options}

    assert {"No.", nil} =
             SelfStateFeatures.append_curiosity_probe(
               "No.",
               features,
               decision,
               %{guardrail?: true},
               nil
             )

    assert {"Done.", nil} =
             SelfStateFeatures.append_curiosity_probe(
               "Done.",
               features,
               decision,
               %{guardrail?: false},
               %{id: :time}
             )

    assert {"What now?", nil} =
             SelfStateFeatures.append_curiosity_probe(
               "What now?",
               features,
               decision,
               %{guardrail?: false},
               nil
             )
  end
end
