defmodule Core.Response.OverrideSkillsTest do
  use ExUnit.Case, async: true

  alias Core.Response.OverrideSkills

  @decision %{tone: :neutral, mode: :editor, action: :answer, scores: %{}, overrides: []}

  test "guardrails prevent deterministic override skills" do
    assert {@decision, nil} =
             OverrideSkills.apply(
               %{intent: :illicit_request, text: "what time is it"},
               @decision,
               %{guardrail?: true}
             )
  end

  test "illicit redirect has highest non-guardrail priority" do
    {decision, skill} =
      OverrideSkills.apply(
        %{intent: :illicit_request, text: "what time is it"},
        @decision,
        %{guardrail?: false}
      )

    assert decision.action == :safe_redirect
    assert :illicit_request_redirect in decision.overrides
    assert skill.id == :illicit_request_redirect
    assert OverrideSkills.deterministic_inline_text(skill) =~ "I can't help"
  end

  test "companion repair runs before utility time answers" do
    {decision, skill} =
      OverrideSkills.apply(
        %{
          intent: :smalltalk,
          confidence_bucket: :low,
          text: "i didnt make you to write code i made you as a companion what time is it"
        },
        @decision,
        %{guardrail?: false}
      )

    assert decision.action == :companion_repair
    assert :companion_repair in decision.overrides
    assert skill.id == :companion_repair
  end

  test "self-state read-only mood query detection delegates to self-state override policy" do
    assert OverrideSkills.read_only_mood_query?("how are your mood indices?")
    assert OverrideSkills.read_only_mood_query?("runtime self-check")
    refute OverrideSkills.read_only_mood_query?("please refactor this module")
  end

  test "time query answers in Pacific Time with daylight-aware abbreviation" do
    {decision, skill} =
      OverrideSkills.apply(
        %{intent: :ask, text: "what time is it right now in PST?"},
        @decision,
        %{guardrail?: false}
      )

    text = OverrideSkills.deterministic_inline_text(skill)

    assert decision.action == :time
    assert :time_skill in decision.overrides
    assert skill.id == :time
    assert text =~ "Pacific Time"
    assert text =~ "America/Vancouver"
    assert text =~ ~r/\b(?:PDT|PST)\b/
    refute text =~ "Pacific Standard Time"
  end

  test "current events request discloses lack of live news instead of generic clarification" do
    {decision, skill} =
      OverrideSkills.apply(
        %{intent: :ask, text: "what current events are happening today?"},
        @decision,
        %{guardrail?: false}
      )

    text = OverrideSkills.deterministic_inline_text(skill)

    assert decision.action == :capability_disclosure
    assert :current_events_capability_answer in decision.overrides
    assert skill.id == :current_events_capability
    assert text =~ "don't have a live news source"
    refute text =~ "provide more context"
  end

  test "short recognizable conflict topic offers likely interpretation" do
    {decision, skill} =
      OverrideSkills.apply(
        %{intent: :unknown, text: "the US Iran war"},
        @decision,
        %{guardrail?: false}
      )

    text = OverrideSkills.deterministic_inline_text(skill)

    assert decision.action == :offer_likely_interpretation
    assert :recognizable_topic_fallback in decision.overrides
    assert skill.id == :recognizable_topic_fallback
    assert text =~ "US-Iran conflict"
    assert text =~ "don't have live news access"
    refute text =~ "Can you please clarify"
  end

  test "alarm capability remains a deterministic inline skill" do
    {decision, skill} =
      OverrideSkills.apply(
        %{intent: :question, text: "can you set an alarm for me?"},
        @decision,
        %{guardrail?: false}
      )

    assert decision.action == :answer
    assert :alarm_capability_answer in decision.overrides
    assert skill.id == :alarm_capability
    assert OverrideSkills.deterministic_inline_text(skill) =~ "can't set a real device alarm"
  end
end
