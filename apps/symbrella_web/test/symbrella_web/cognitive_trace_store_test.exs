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

  test "mixed social check-ins retain only a bounded, ordered act pair" do
    raw = %{
      speech_act: :social_update_query,
      social_components: [:greeting, :social_update_query],
      source_text: "PRIVATE INPUT", chosen_id: "PRIVATE SENSE",
      sentence_meaning_verified?: true
    }

    {_, traced} = record(state(), "turn-f008ee", :speech_act_binding, :ok, 100, raw)
    [event] = traced.events
    assert event.evidence ==
             %{speech_act: :social_update_query,
               social_components: [:greeting, :social_update_query]}
    refute inspect(event) =~ "PRIVATE"

    {_, single} = record(state(), "turn-f008ef", :speech_act_binding, :ok, 100,
      %{speech_act: :social_update_query,
        social_components: [:social_update_query]})
    assert hd(single.events).evidence.social_components == [:social_update_query]

    for bad <- [
      %{speech_act: :social_update_query,
        social_components: [:greeting, :unknown_request], source_text: "PRIVATE"},
      %{speech_act: :social_update_query,
        social_components: [:greeting, :social_update_query, :unsafe]},
      %{speech_act: :unknown_act,
        social_components: [:greeting, :social_update_query]}
    ] do
      {_, rejected} =
        record(state(), "turn-f008a1", :speech_act_binding, :ok, 100, bad)
      assert hd(rejected.events).evidence == %{}
    end
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

  test "keeps only allowlisted comprehension diagnoses for the selected turn" do
    raw = %{
      degraded: true, understood: 0, uncertain: 1, unresolved: 0,
      reasons: [:weak_decision_rate_high, :fallback_rate_high,
                :unrecognized_diagnosis, "PRIVATE REASON", :fallback_rate_high],
      choice_count: 3, unresolved_content_count: 0, verified_lexical_count: 0,
      weak_rate: 0.666667, fallback_rate: 0.333333,
      structural_status: :preferred, sentence_meaning_status: :provisional,
      integration_status: :consistent, feedback_mode: :bounded_reanalysis,
      feedback_applied: false, repair_hint: :review_lexical_evidence,
      curiosity_gap_gate: :needs_unresolved_content,
      sentence: "PRIVATE UTTERANCE", selected_sense: "PRIVATE ID",
      response: "PRIVATE RESPONSE"
    }

    {_, traced} = record(state(), "turn-ff0071", :comprehension_summary, :ok, 100, raw)
    [event] = traced.events
    assert event.evidence.degraded
    assert event.evidence.reasons ==
             [:weak_decision_rate_high, :fallback_rate_high]
    assert event.evidence.choice_count == 3
    assert event.evidence.verified_lexical_count == 0
    assert event.evidence.weak_rate == 0.666667
    assert event.evidence.fallback_rate == 0.333333
    assert event.evidence.structural_status == :preferred
    assert event.evidence.sentence_meaning_status == :provisional
    assert event.evidence.integration_status == :consistent
    assert event.evidence.feedback_applied == false
    assert event.evidence.repair_hint == :review_lexical_evidence
    assert event.evidence.curiosity_gap_gate == :needs_unresolved_content
    refute inspect(event.evidence) =~ "PRIVATE"
    assert length(traced.events) == 1

    malicious = %{
      raw | reasons: ["PRIVATE", :unlisted],
            choice_count: 100_001, weak_rate: 9.0,
            fallback_rate: -1.0, structural_status: :injected_status,
            curiosity_gap_gate: :untrusted, feedback_applied: "yes"
    }
    {_, bad} = record(state(), "turn-ff0072", :comprehension_summary, :ok, 105, malicious)
    [sanitized] = bad.events
    assert sanitized.evidence.reasons == []
    refute Map.has_key?(sanitized.evidence, :choice_count)
    assert sanitized.evidence.weak_rate == nil
    assert sanitized.evidence.fallback_rate == nil
    assert sanitized.evidence.structural_status == :not_observed
    assert sanitized.evidence.curiosity_gap_gate == :not_observed
    assert sanitized.evidence.feedback_applied == :not_observed
    refute inspect(sanitized) =~ "PRIVATE"
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

  test "decision lineage is allowlisted through the store without command or reply payloads" do
    raw = %{
      initial_action: :ask_clarifying_question,
      final_action: :answer_user,
      initial_safety_gate: :approved,
      final_safety_gate: :approved,
      reconciliation: :reconciled,
      reason: :social_intent_reconciliation,
      shadow_action: :observe_silently,
      shadow_status: :evaluated,
      shadow_applied_keys: [:utility_prior, :secret_control],
      initial_command_statuses: [:deferred, :secret_status],
      final_command_statuses: [],
      delivery_status: :delivered,
      response_text: "PRIVATE REPLY",
      user_text: "PRIVATE INPUT",
      initial_decision_trace_id: "PRIVATE DECISION ID"
    }

    {_, trace} = record(state(), "turn-dec1510", :response_planning, :ok, 100, raw)
    [event] = trace.events
    assert event.stage == :response_planning
    assert event.region == nil
    assert event.evidence.initial_action == :ask_clarifying_question
    assert event.evidence.final_action == :answer_user
    assert event.evidence.reconciliation == :reconciled
    assert event.evidence.reason == :social_intent_reconciliation
    assert event.evidence.shadow_applied_keys == [:utility_prior]
    assert event.evidence.initial_command_statuses == [:deferred, :unknown]
    assert event.evidence.final_command_statuses == []
    assert event.evidence.delivery_status == :not_observed
    refute inspect(event) =~ "PRIVATE"
    refute Map.has_key?(event.evidence, :initial_decision_trace_id)
  end

  test "empty executive reports are explicitly marked unavailable" do
    {_, trace} =
      record(state(), "turn-001122", :prefrontal_control, :ok, 100,
        %{available: false, controls: %{}, planners: []})

    assert [%{evidence: %{available: false, planners: []}}] = trace.events
  end
end
