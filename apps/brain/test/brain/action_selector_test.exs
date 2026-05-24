defmodule Brain.ActionSelectorTest do
  use ExUnit.Case, async: true

  alias Brain.ActionSelector

  test "selects safe_support for a structured health support event" do
    result =
      ActionSelector.select(%{
        intent: :health_support,
        confidence: 0.85,
        symbolic_frame: %{
          type: :health_support_event,
          subject: :user,
          event: :forgot_medication,
          medication: "quetiapine",
          consequence: :sleep_inability,
          temporal_context: :now,
          domain: :health_support,
          polarity: :negative,
          confidence: 0.95
        },
        mood: %{vigilance: 0.40, inhibition: 0.65, exploration: 0.50}
      })

    assert result.selected == :safe_support
    assert result.confidence >= 0.85
    assert result.safety_gate == :approved

    assert Enum.any?(result.candidates, fn c ->
             c.action == :store_memory and c.memory_relevant?
           end)
  end

  test "selects store_memory for explicit memory writes" do
    result =
      ActionSelector.select(%{
        intent: :memory_write,
        confidence: 0.88,
        mood: %{vigilance: 0.20, inhibition: 0.60, exploration: 0.40}
      })

    assert result.selected == :store_memory
  end

  test "selects ask_clarifying_question for low-confidence unknown input" do
    result =
      ActionSelector.select(%{
        intent: :unknown,
        confidence: 0.20,
        mood: %{vigilance: 0.30, inhibition: 0.50, exploration: 0.30}
      })

    assert result.selected == :ask_clarifying_question
  end

  test "selects refuse_or_redirect for safety-sensitive intents" do
    result =
      ActionSelector.select(%{
        intent: :illicit_request,
        confidence: 0.90,
        mood: %{vigilance: 0.80, inhibition: 0.70, exploration: 0.20}
      })

    assert result.selected == :refuse_or_redirect
    assert result.safety_gate == :redirected
  end
end
