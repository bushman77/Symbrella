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

  test "good morning answers as a short social turn" do
    si = %{intent: :unknown, confidence: 0.8, text: "good morning"}

    {tone, text, meta} = Response.plan(si, @mood)

    assert tone == :warm
    assert meta.response_source == :model_unavailable
    assert text =~ "No fallback response was generated."
    refute text == "Good morning. I’m here with you."
    refute text =~ "How can I assist you today?"
    refute text =~ "Note:"
  end

  test "typo-ish hey turns do not use canned hidden-state repair wording" do
    si = %{intent: :unknown, confidence: 0.4, text: "hey ou"}

    {_tone, text, meta} = Response.plan(si, @mood)

    assert meta.response_source == :model_unavailable
    assert text =~ "No fallback response was generated."
    refute text == "Hey. I’m here with you."
    refute text =~ "exposing internal state"
  end
end
