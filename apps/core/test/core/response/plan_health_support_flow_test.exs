defmodule Core.Response.PlanHealthSupportFlowTest do
  use ExUnit.Case, async: true

  alias Core.Response

  test "plan/2 routes health support away from engineering collaborator fallback" do
    si = %{
      intent: :health_support,
      keyword: "i forgot my quetiapine and have not been able to sleep",
      confidence: 0.76,
      text: "Good morning, I forgot my quetiapine and haven't been able to sleep.",
      opener_intent: :greet,
      primary_text: "i forgot my quetiapine and haven't been able to sleep",
      conversation_act: :personal_disclosure,
      topic_domain: :health_sleep_medication
    }

    mood = %{
      mood: %{
        vigilance: 0.3,
        inhibition: 0.45,
        exploration: 0.6,
        plasticity: 0.5
      },
      tone_hint: nil
    }

    {tone, text, meta} = Response.plan(si, mood)

    assert tone == :warm
    assert meta.intent_inferred == :health_support
    assert meta.mode == :supportive_care
    assert meta.action == :safe_support
    assert meta.profile == :supportive_care
    assert meta.response_source == :model_unavailable
    assert meta.response_fallback_reason in [:llm_not_available, :lazy_start_disabled]

    assert text =~ "No fallback response was generated."
    refute text =~ "missed medication dose"
    refute text =~ "pharmacist or prescriber"
    refute text =~ "should not tell you how to change the dose"

    refute text =~ "module"
    refute text =~ "file"
    refute text =~ "failing output"
    refute text =~ "engineering move"
  end
end
