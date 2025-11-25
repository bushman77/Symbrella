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
    assert meta.mode == :pair_programmer
    assert meta.action == :act_first
    assert meta.intent_inferred == :refactor
    assert meta.profile == :warm_collaborator

    # Text should look like warm pair-programmer, not greeting/coach/editor.
    assert text =~ "plan"
    assert text =~ "full file"
    assert text =~ "paste-ready"
  end
end

