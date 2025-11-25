defmodule Core.Response.PlanCalmExplainerFlowTest do
  use ExUnit.Case, async: true

  alias Core.Response

  test "plan/2 routes a brain-meta question into calm explainer profile" do
    si = %{
      intent: :explain,
      keyword: "explain LIFG",
      confidence: 0.88,
      text: "can you explain what LIFG is doing in Symbrella's pipeline?"
    }

    mood = %{
      mood: %{
        vigilance: 0.4,
        inhibition: 0.4,
        exploration: 0.5,
        plasticity: 0.5
      },
      tone_hint: nil
    }

    {tone, text, meta} = Response.plan(si, mood)

    assert tone == :warm
    assert meta.mode == :explainer
    assert meta.action == :offer_options
    assert meta.intent_inferred == :explain
    assert meta.profile == :calm_explainer

    # Should be using explainer bullets, not pair_programmer/coach/editor copy
    assert text =~ "Here's the short version of how this works"
    assert text =~ "1) What changes, at a glance"
  end
end

