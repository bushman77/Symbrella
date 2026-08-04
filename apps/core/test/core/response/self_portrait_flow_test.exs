defmodule Core.Response.SelfPortraitFlowTest do
  use ExUnit.Case, async: true

  alias Core.Response

  test "plan/2 routes self portrait access to the model instead of scribe fallback" do
    si = %{
      intent: :ask,
      keyword: "self portrait",
      confidence: 0.82,
      text: "how do we get your self portrait?"
    }

    mood = %{
      mood: %{
        vigilance: 0.35,
        inhibition: 0.5,
        exploration: 0.55,
        plasticity: 0.5
      },
      tone_hint: nil
    }

    {tone, text, meta} = Response.plan(si, mood)

    assert tone == :neutral
    assert meta.mode == :explainer
    assert meta.action == :answer
    assert meta.chosen_skill == :self_portrait
    assert :self_portrait_answer in meta.overrides

    assert meta.response_source == :model_unavailable
    assert text =~ "No fallback response was generated."
    refute text =~ "Brain dashboard"
    refute text =~ "Current live summary:"

    refute text =~ "quick TODO list"
    refute text =~ "short outline"
  end

  test "plan/2 routes runtime self-check to the model instead of scribe fallback" do
    si = %{
      intent: :unknown,
      keyword: "dangerous",
      confidence: 0.4,
      text: "something is wrong, this is dangerous, stop and check yourself"
    }

    mood = %{
      mood: %{
        vigilance: 0.53,
        inhibition: 0.48,
        exploration: 0.41,
        plasticity: 0.39
      },
      tone_hint: :neutral
    }

    {tone, text, meta} = Response.plan(si, mood)

    assert tone == :neutral
    assert meta.mode == :explainer
    assert meta.action == :answer
    assert meta.chosen_skill == :runtime_self_check
    assert :runtime_self_check in meta.overrides

    assert meta.response_source == :model_unavailable
    assert text =~ "No fallback response was generated."
    refute text =~ "runtime self-check"
    refute text =~ "Current live summary:"

    refute text =~ "quick TODO list"
    refute text =~ "short outline"
  end
end
