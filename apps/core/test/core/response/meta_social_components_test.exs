defmodule Core.Response.MetaSocialComponentsTest do
  use ExUnit.Case, async: true

  alias Core.Response.Meta

  defp reflection_summary(reflection) do
    Meta.build(%{
      decision: %{
        policy_version: 1, tone: :neutral, mode: :scribe,
        action: :answer, scores: %{}, overrides: []
      },
      intent0: :smalltalk,
      intent: :smalltalk,
      conf: 0.4,
      confidence_bucket: :low,
      risk_bucket: :low,
      profile: :social,
      benign?: true,
      hostile?: false,
      tone_hint: :neutral,
      mood_sample: %{},
      skill: nil,
      guard: %{guardrail?: false, approve_token?: nil, flags: []},
      session_id: "metadata-social-test",
      extracted_name: nil,
      planner_explanation: "",
      curiosity_probe: nil,
      response_source: :symbolic_generation,
      response_fallback_reason: nil,
      response_reflection: reflection,
      agency_decision: nil,
      agency_commands: nil,
      agency_command_results: nil,
      agency_memory: nil
    }).response_reflection
  end

  test "both approved social-update sequences survive response reflection summarization" do
    for components <- [
      [:greeting, :social_update_query],
      [:social_update_query]
    ] do
      summary =
        reflection_summary(%{
          status: :symbolic_realized,
          interpreted_speech_act: :social_update_query,
          speech_act: :respond_to_social_update,
          social_components: components,
          semantic_verified?: false,
          factual_claims?: false,
          source_text: "PRIVATE INPUT",
          lexical_witness: nil
        })

      assert summary.social_components == components
      assert summary.semantic_verified? == false
      assert summary.factual_claims? == false
      refute Map.has_key?(summary, :source_text)
      refute inspect(summary) =~ "PRIVATE"
    end
  end

  test "forged components and non-social reflections cannot pass the allowlist" do
    for {act, components} <- [
      {:social_update_query, [:greeting, :unknown_request]},
      {:social_update_query, [:greeting, :social_update_query, :untrusted]},
      {:social_update_query, ["PRIVATE INPUT"]},
      {:greeting, [:greeting, :social_update_query]}
    ] do
      summary =
        reflection_summary(%{
          status: :symbolic_realized,
          interpreted_speech_act: act,
          social_components: components
        })

      refute Map.has_key?(summary, :social_components)
      refute inspect(summary) =~ "PRIVATE"
    end
  end
end
