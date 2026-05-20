defmodule Core.Response.CasualChatFlowTest do
  use ExUnit.Case, async: true

  alias Core.Response

  @mood %{
    mood: %{vigilance: 0.4, inhibition: 0.6, exploration: 0.4, plasticity: 0.4},
    tone_hint: nil
  }

  test "casual reactions route through LLM/fallback instead of inline chat templates" do
    si = %{
      intent: :unknown,
      confidence: 0.2,
      text: "that was interesting a clock that told stories hahaha"
    }

    {tone, text, meta} = Response.plan(si, @mood)

    assert tone == :neutral
    assert meta.mode == :scribe
    assert meta.action == :offer_options
    refute :casual_chat_answer in meta.overrides
    refute text =~ "quick TODO list"
    refute text =~ "short outline"
    refute text =~ "interesting one"
  end

  test "laughter-only turns do not use a canned inline laugh response" do
    si = %{intent: :unknown, confidence: 0.1, text: "hahahah"}

    {_tone, text, meta} = Response.plan(si, @mood)

    refute :casual_chat_answer in meta.overrides
    refute text =~ "Haha"
    refute text =~ "quick TODO list"
  end

  test "alarm requests answer capability directly" do
    si = %{intent: :question, confidence: 0.7, text: "are you able to set an alarm forr me?"}

    {tone, text, meta} = Response.plan(si, @mood)

    assert tone == :warm
    assert meta.action == :answer
    assert :alarm_capability_answer in meta.overrides
    assert text =~ "can't set a real device alarm yet"
    refute text =~ "quick TODO list"
  end
end
