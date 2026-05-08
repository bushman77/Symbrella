defmodule Core.Response.SelfPortraitFlowTest do
  use ExUnit.Case, async: true

  alias Core.Response

  test "plan/2 answers self portrait access directly instead of scribe fallback" do
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

    assert text =~ "Brain dashboard"
    assert text =~ "/brain"
    assert text =~ "Brain.SelfPortrait.snapshot()"
    assert text =~ "Brain.Introspect.snapshot(:self_portrait)"
    assert text =~ "Current live summary:"
    assert text =~ "software self-state snapshot"

    refute text =~ "quick TODO list"
    refute text =~ "short outline"
  end

  test "plan/2 answers runtime self-check directly instead of scribe fallback" do
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

    assert text =~ "runtime self-check"
    assert text =~ "Current live summary:"
    assert text =~ "software state"

    refute text =~ "quick TODO list"
    refute text =~ "short outline"
  end
end
