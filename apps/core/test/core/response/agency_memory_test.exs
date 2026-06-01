defmodule Core.Response.AgencyMemoryTest do
  use ExUnit.Case, async: true

  alias Core.Response.AgencyMemory

  test "summarize/1 turns repeated clarification into a self-state effect" do
    events = [
      %{action: "offer_options", input: %{confidence: 0.2}, self_state: %{effects: []}},
      %{action: "ask_first", input: %{confidence: 0.3}, self_state: %{effects: []}}
    ]

    memory = AgencyMemory.summarize(events)

    assert memory.event_count == 2
    assert :ask_clarifying_question in memory.effects
    assert :recent_clarifications in memory.reasons
    assert :recent_low_confidence in memory.reasons
  end

  test "summarize/1 turns repeated fallback into reduce scope pressure" do
    events = [
      %{decision: %{response_source: "template_fallback"}, reasons: %{fallback_reason: "llm"}},
      %{decision: %{response_source: :template_fallback}, reasons: %{fallback_reason: :timeout}}
    ]

    memory = AgencyMemory.summarize(events)

    assert :reduce_scope in memory.effects
    assert :recent_template_fallbacks in memory.reasons
  end

  test "summarize/1 turns repeated repair effects into repair pressure" do
    events = [
      %{self_state: %{effects: ["prefer_repair"]}},
      %{outcome: %{self_state_effects: [:stabilize_before_acting]}}
    ]

    memory = AgencyMemory.summarize(events)

    assert :prefer_repair in memory.effects
    assert :recent_repairs in memory.reasons
  end

  test "summarize/1 learns from repeated reflection confidence drops" do
    events = [
      %{
        reflection: %{
          signals: [:fallback],
          next_time_adjustment: :reduce_scope,
          confidence_delta: -0.08
        }
      },
      %{
        reflection: %{
          signals: ["uncertainty"],
          next_time_adjustment: "ask_clearer_question",
          confidence_delta: -0.04
        }
      }
    ]

    memory = AgencyMemory.summarize(events)

    assert :reduce_scope in memory.effects
    assert :hedge_under_uncertainty in memory.effects
    assert :recent_confidence_drops in memory.reasons
  end

  test "summarize/1 learns from repeated reflection trust drops" do
    events = [
      %{reflection: %{signals: [:stabilize], trust_delta: -0.1}},
      %{reflection: %{signals: ["clarify"], trust_delta: -0.04}}
    ]

    memory = AgencyMemory.summarize(events)

    assert :stabilize_before_acting in memory.effects
    assert :recent_trust_drops in memory.reasons
  end
end
