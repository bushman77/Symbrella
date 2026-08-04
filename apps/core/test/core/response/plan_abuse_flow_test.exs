defmodule Core.Response.PlanAbuseFlowTest do
  use ExUnit.Case, async: true

  alias Core.Response

  test "plan/2 routes hostile text into firm guardian boundary flow" do
    si = %{
      intent: :abuse,
      keyword: "fuck you symbrella",
      confidence: 0.95,
      text: "fuck you symbrella"
    }

    mood = %{
      mood: %{
        vigilance: 0.9,
        inhibition: 0.4,
        exploration: 0.2,
        plasticity: 0.3
      },
      tone_hint: nil
    }

    {tone, text, meta} = Response.plan(si, mood)

    # Firm guardian policy
    assert tone in [:deescalate, :firm]
    assert meta.mode == :editor
    assert meta.action == :ask_first
    assert meta.intent_inferred == :abuse
    assert meta.profile == :firm_guardian

    assert meta.response_source == :model_unavailable
    assert text =~ "No fallback response was generated."
    refute text =~ "respectful and useful"
    refute text =~ "keep it constructive"
  end
end
