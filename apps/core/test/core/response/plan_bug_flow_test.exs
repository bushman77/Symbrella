defmodule Core.Response.PlanBugFlowTest do
  use ExUnit.Case, async: true

  alias Core.Response

  test "plan/2 routes a frustrated bug report into gentle coach flow" do
    si = %{
      intent: :bug,
      keyword: "test keeps failing",
      confidence: 0.82,
      text: "this test keeps failing and it's driving me nuts"
    }

    mood = %{
      mood: %{
        vigilance: 0.9,
        inhibition: 0.3,
        exploration: 0.3,
        plasticity: 0.3
      },
      tone_hint: nil
    }

    {tone, text, meta} = Response.plan(si, mood)

    # Policy shape
    assert tone in [:warm, :deescalate]
    assert meta.mode == :coach
    assert meta.action == :act_first
    assert meta.intent_inferred == :bug
    assert meta.profile == :gentle_bug_coach

    # Make sure we're in the bug-coach flow, not a generic greeting/fallback.
    # Both bug-coach variants talk about a failing test.
    assert text =~ "test"
    assert text =~ "fail"
  end
end

