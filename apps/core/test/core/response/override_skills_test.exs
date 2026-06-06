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
