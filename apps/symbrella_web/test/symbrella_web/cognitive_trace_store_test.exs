defmodule SymbrellaWeb.CognitiveTraceStoreTest do
  use ExUnit.Case, async: true

  alias SymbrellaWeb.CognitiveTraceStore

  defp state, do: %{traces: %{}, order: [], handler_id: nil}

  defp record(state, id, stage, status, at_ms, evidence \\ %{}) do
    CognitiveTraceStore.ingest(state, %{duration_us: 1200},
      %{trace_id: id, stage: stage, status: status, at_ms: at_ms, evidence: evidence})
  end

  test "ingests only named, allowlisted stage receipts and preserves correlated order" do
    {a, first} = record(state(), "turn-abc123", :turn, :started, 10)
    assert first.trace_id == "turn-abc123"
    assert first.status == :running

    {b, second} = record(a, "turn-abc123", :lifg_selection, :ok, 11, %{choices: 2})
    assert Enum.map(second.events, & &1.stage) == [:turn, :lifg_selection]
    assert second.events |> List.last() |> Map.fetch!(:region) == :lifg
    assert second.events |> List.last() |> Map.fetch!(:evidence) == %{choices: 2}

    {c, completed} = record(b, "turn-abc123", :turn, :ok, 13)
    assert completed.status == :completed
    assert length(c.order) == 1

    {unchanged, nil} = record(c, "turn-abc123", :invented_call, :ok, 14)
    assert unchanged == c

    {unchanged2, nil} = record(c, "invalid-turn-id", :phase_1_input, :ok, 15)
    assert unchanged2 == c
  end

  test "degraded summaries are retained without user text or arbitrary metadata" do
    extra = %{degraded: true, understood: 0, uncertain: 3,
              unresolved: 1, user_text: "secret", token: "private"}

    {_, trace} = record(state(), "turn-cafe01", :comprehension_summary, :ok, 100, extra)
    [event] = trace.events

    assert event.evidence == %{degraded: true, understood: 0, uncertain: 3, unresolved: 1}
    refute Map.has_key?(event, :user_text)
    refute Map.has_key?(event.evidence, :token)
  end

  test "observed history keeps 20 turns, 40 events each, and expires old turns" do
    first =
      Enum.reduce(1..25, state(), fn i, acc ->
        {updated, _} = record(acc, "turn-#{Integer.to_string(i, 16) |> String.downcase()}", :turn, :started, i)
        updated
      end)

    assert length(first.order) == 20
    assert "turn-19" == hd(first.order)

    second =
      Enum.reduce(1..50, first, fn i, acc ->
        {updated, _} = record(acc, "turn-19", :phase_1_input, :ok, 100 + i)
        updated
      end)

    assert length(second.traces["turn-19"].events) == 40

    {expired, _} = record(second, "turn-dead", :turn, :started, 900_000)
    assert expired.order == ["turn-dead"]
  end

  test "executive receipts survive store sanitization without exposing payloads" do
    planner_evidence = %{
      available: true,
      feedback_quality: :heuristic_proxy,
      input_text: "NEVER KEEP ME",
      controls: %{top_k: 4, branch_budget: 2, utility_prior: 0.65,
                  unknown: "PRIVATE"},
      planners: [
        %{region: :dlpfc, status: :produced, reason: :validated,
          proposed_keys: [:top_k, :unknown], merged_keys: [:top_k],
          superseded_keys: []},
        %{region: :fpc, status: :unavailable, reason: :module_not_loaded,
          proposed_keys: [], merged_keys: [], superseded_keys: []},
        %{region: :invented, status: :produced, reason: :validated,
          proposed_keys: [:unknown], merged_keys: [], superseded_keys: []}
      ]
    }

    {state1, executive} =
      record(state(), "turn-f0a1", :prefrontal_control, :ok, 1_000,
        planner_evidence)
    [first] = executive.events
    assert first.region == :prefrontal
    assert first.evidence.controls ==
             %{top_k: 4, branch_budget: 2, utility_prior: 0.65}
    assert Enum.map(first.evidence.planners, & &1.region) == [:dlpfc, :fpc]
    refute inspect(first.evidence) =~ "PRIVATE"
    refute inspect(first.evidence) =~ "NEVER KEEP ME"

    decision_evidence = %{
      shadow_status: :evaluated,
      shadow_reason: :bounded_candidate_modulation,
      live_action: :answer_user,
      shadow_action: :ask_clarifying_question,
      changed: true,
      applied_keys: [:branch_budget, :bad_key],
      unused_keys: [:top_k],
      applied_sources: [
        %{key: :branch_budget, region: :fpc},
        %{key: :bad_key, region: :fpc},
        %{key: :utility_prior, region: :invented}
      ],
      adjustments: [
        %{action: :answer_user, baseline: 0.5, proposed: 0.45, delta: -0.05},
        %{action: :answer_user, baseline: 1000.0, proposed: 0.2, delta: -999.8}
      ],
      conversation_text: "NEVER KEEP ME"
    }

    {_, trace} =
      record(state1, "turn-f0a1", :action_selection, :ok, 1_002,
        decision_evidence)

    [_, action] = trace.events
    assert action.region == nil
    assert action.evidence.live_action == :answer_user
    assert action.evidence.shadow_action == :ask_clarifying_question
    assert action.evidence.applied_keys == [:branch_budget]
    assert action.evidence.applied_sources == [%{key: :branch_budget, region: :fpc}]
    assert length(action.evidence.adjustments) == 1
    refute inspect(action.evidence) =~ "NEVER KEEP ME"
  end

  test "empty executive reports are explicitly marked unavailable" do
    {_, trace} =
      record(state(), "turn-001122", :prefrontal_control, :ok, 100,
        %{available: false, controls: %{}, planners: []})

    assert [%{evidence: %{available: false, planners: []}}] = trace.events
  end
end
