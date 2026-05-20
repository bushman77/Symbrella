defmodule Core.Response.PlanWarmCollaboratorFlowTest do
  use ExUnit.Case, async: true

  alias Core.Response

  test "plan/2 routes a normal helpful request into warm collaborator flow" do
    si = %{
      intent: :refactor,
      keyword: "refactor Brain.LIFG",
      confidence: 0.9,
      text: "help me refactor Brain.LIFG for better tests"
    }

    mood = %{
      mood: %{
        vigilance: 0.3,
        inhibition: 0.4,
        exploration: 0.6,
        plasticity: 0.5
      },
      tone_hint: nil
    }

    {tone, text, meta} = Response.plan(si, mood)

    # Policy shape: warm collaborator
    assert tone == :warm
    assert meta.mode == :collaborator
    assert meta.action == :act_first
    assert meta.intent_inferred == :refactor
    assert meta.profile == :warm_collaborator

    # With no LLM registered in this test, text comes from the deterministic fallback.
    assert text =~ "Suggested next step: Make the next concrete engineering move."
    refute text =~ "full file"
    refute text =~ "paste-ready"
    refute text =~ "drop-in"
  end
end
