defmodule Core.Response.MetaGroundedRevisionTest do
  use ExUnit.Case, async: false

  alias Core.Response.Meta

  setup do
    prior = Application.get_env(:core, :expose_llm_drafts_in_explain?, :unset)

    on_exit(fn ->
      if prior == :unset do
        Application.delete_env(:core, :expose_llm_drafts_in_explain?)
      else
        Application.put_env(:core, :expose_llm_drafts_in_explain?, prior)
      end
    end)

    :ok
  end

  defp metadata(reflection) do
    Meta.build(%{
      decision: %{
        policy_version: 1, tone: :neutral, mode: :scribe,
        action: :answer, scores: %{}, overrides: []
      },
      intent0: :ask,
      intent: :ask,
      conf: 0.683,
      confidence_bucket: :medium,
      risk_bucket: :low,
      profile: :semantic_repair,
      benign?: true,
      hostile?: false,
      tone_hint: :neutral,
      mood_sample: %{},
      skill: nil,
      guard: %{guardrail?: false, approve_token?: nil, flags: []},
      session_id: "grounded-metadata-test",
      extracted_name: nil,
      planner_explanation: "",
      curiosity_probe: nil,
      response_source: :llm,
      response_fallback_reason: nil,
      response_reflection: reflection,
      agency_decision: nil,
      agency_commands: nil,
      agency_command_results: nil,
      agency_memory: nil
    })
  end

  defp repaired_trace do
    %{
      status: :repair,
      issues: [:geographic_claim_unverified],
      draft_sha256: "abc",
      final_sha256: "def",
      applied?: true,
      repair_count: 1,
      grounded_revision: %{
        attempted?: true,
        outcome: :rejected,
        source: :core_fallback,
        prior_issues: [:geographic_claim_unverified],
        candidate_issues: [:geographic_claim_unverified],
        first_draft: "The population of Kamloops, BC is 97,902.",
        candidate: "The population of Kamloops, BC is 979,020 people."
      }
    }
  end

  test "metadata preserves actual rejected grounded retry without exposing drafts by default" do
    Application.put_env(:core, :expose_llm_drafts_in_explain?, false)

    summary = metadata(repaired_trace()).response_reflection
    assert summary.status == :repair
    assert summary.grounded_revision.attempted? == true
    assert summary.grounded_revision.outcome == :rejected
    assert summary.grounded_revision.source == :core_fallback
    assert summary.grounded_revision.candidate_issues == [:geographic_claim_unverified]
    refute Map.has_key?(summary.grounded_revision, :first_draft)
    refute Map.has_key?(summary.grounded_revision, :candidate)
  end

  test "dev diagnostics retain bounded first and second model drafts" do
    Application.put_env(:core, :expose_llm_drafts_in_explain?, true)

    trace = repaired_trace()
    first = trace.grounded_revision.first_draft
    candidate = trace.grounded_revision.candidate

    summary = metadata(trace).response_reflection.grounded_revision

    assert summary.outcome == :rejected
    assert summary.first_draft == first
    assert summary.candidate == candidate

    long_trace = put_in(trace, [:grounded_revision, :candidate], String.duplicate("x", 700))
    bounded = metadata(long_trace).response_reflection.grounded_revision

    assert String.length(bounded.candidate) == 500
  end

  test "missing revision is not synthesized as a rejected or attempted pass" do
    summary = metadata(%{status: :repair, issues: [:geographic_claim_unverified]})
      .response_reflection

    refute Map.has_key?(summary, :grounded_revision)
  end
end
