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

    # With no LLM registered in this test, text comes from the deterministic fallback.
    assert text =~ "Suggested next step:"
    assert text =~ "Explain from the available Symbrella evidence without implying sentience."
    refute text =~ "Here's the short version of how this works"
    refute text =~ "1) What changes, at a glance"
  end
end
