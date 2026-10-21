defmodule Core.Curiosity.InvestigatorTest do
  use ExUnit.Case, async: false

  alias Brain.Hippocampus.SemanticAssociations
  alias Core.Cognition.AssociativeReasoning
  alias Core.Curiosity.{InvestigationHistory, Investigator}
  alias Core.Response.{
    AuthoritativeAnswerPlan, Consolidation, PopulationComparison, ResponseFrame
  }
  alias Db.{CognitiveIssue, CognitiveIssues, ConsolidatedConclusions}

  setup do
    keys = [
      :symbolic_consolidation_enabled?,
      :symbolic_composition_enabled?,
      :semantic_associative_recall_enabled?
    ]

    previous = Enum.map(keys, &{&1, Application.get_env(:core, &1)})
    previous_investigation = Application.get_env(:core, Investigator)

    Enum.each(keys, &Application.put_env(:core, &1, true))
    Application.put_env(:core, Investigator, enabled?: true)

    on_exit(fn ->
      Enum.each(previous, fn
        {key, nil} -> Application.delete_env(:core, key)
        {key, value} -> Application.put_env(:core, key, value)
      end)

      if is_nil(previous_investigation),
        do: Application.delete_env(:core, Investigator),
        else: Application.put_env(:core, Investigator, previous_investigation)
    end)

    n = System.unique_integer([:positive])
    %{a: "InvestAlpha#{n}", b: "InvestBridge#{n}",
      c: "InvestGamma#{n}"}
  end

  defp event(names) do
    %{type: :location_question, domain: :geography,
      relation: :population, comparison: :greater,
      subjects: names, grounding_status: :unverified}
  end

  defp fresh(names) do
    frame = event(names)

    %{
      user_text: "Which had the larger population in 2021?",
      features: %{intent: :question, symbolic_frame: frame},
      symbolic_frame: %{event: frame},
      decision: %{action: :answer}, mood: %{}
    }
  end

  defp capture!(a, b, values) do
    context = fresh([a, b])

    proof = %{
      status: :retrieved, resolution: :source_supported,
      relation: :population, provider: :wikidata,
      support: %{
        kind: :population_group, status: :source_supported,
        subjects: [a, b],
        results: Enum.zip([a, b], values) |> Enum.map(fn {subject, value} ->
          %{
            subject: subject, value: value, census_year: 2021,
            status: :source_supported, source_type: :wikidata,
            source_url: "https://example.invalid/#{subject}/2021"
          }
        end)
      },
      sources: [], attempted_count: 2, attempts: []
    }

    proved =
      context
      |> Map.put(:web_evidence, proof)
      |> PopulationComparison.attach()
      |> then(fn ctx -> Map.put(ctx, :response_frame, ResponseFrame.build(ctx)) end)

    plan = AuthoritativeAnswerPlan.build(proved)
    assert %{kind: :verified_population_comparison} = plan
    assert {:ok, saved} = Consolidation.capture(proved, plan)
    saved
  end

  defp missing_issue!(a, c) do
    context = Consolidation.recall(fresh([a, c]))
    assert context.consolidation_issue.kind == :missing_reasoning_link
    Db.get!(CognitiveIssue, context.consolidation_issue.id)
  end

  test "opt-in gate prevents investigating persisted knowledge", %{a: a, b: b, c: c} do
    capture!(a, b, [120_000, 100_000])
    issue = missing_issue!(a, c)
    capture!(b, c, [100_000, 80_000])

    Application.put_env(:core, Investigator, enabled?: false)

    assert %{status: :disabled} = Investigator.investigate_issue(issue.id)
    assert %{status: "open"} = Db.get!(CognitiveIssue, issue.id)
    assert %{status: :disabled} = Investigator.run_now()
  end

  test "missing intermediate evidence remains open without generating an answer", %{
    a: a, b: b, c: c
  } do
    capture!(a, b, [120_000, 100_000])
    issue = missing_issue!(a, c)

    # The single persisted parent is a two-hop planning hint only.
    assert Enum.any?(Core.Curiosity.UnresolvedIssues.reasoning_candidates(20,
      acquire_evidence?: true), &(&1.issue_id == issue.id and &1.strategy == :two_hop))

    assert %{status: :unresolved, reason: :no_two_hop_cue,
             history_id: audit_id} =
      Investigator.investigate_issue(issue.id)
    assert is_integer(audit_id)
    assert [%{id: ^audit_id, status: "unresolved", reason: "no_two_hop_cue"}] =
      InvestigationHistory.for_issue(issue.id)
    assert %{status: "open", resolved_at: nil} = Db.get!(CognitiveIssue, issue.id)
    assert {:ok, [_]} = ConsolidatedConclusions.for_subject(a)
  end

  test "autonomous investigation resolves only after hippocampus-guided verified composition", %{
    a: a, b: b, c: c
  } do
    ab = capture!(a, b, [120_000, 100_000])
    issue = missing_issue!(a, c)
    bc = capture!(b, c, [100_000, 80_000])

    assert ab.fingerprint in SemanticAssociations.cues([a, c])
    assert bc.fingerprint in SemanticAssociations.cues([a, c])
    assert %{kind: :two_hop_candidate, parent_proof_ids: ids} =
      AssociativeReasoning.propose([a, c])
    assert ids == Enum.sort([ab.id, bc.id])

    assert %{status: :resolved, issue_id: issue_id, reference_year: 2021,
             parent_proof_ids: ^ids, origin: :hippocampal_two_hop_cue} =
      Investigator.investigate_issue(issue.id)
    assert issue_id == issue.id

    stored = Db.get!(CognitiveIssue, issue.id)
    assert stored.status == "resolved"
    assert stored.resolution["parent_proof_ids"] == ids
    assert stored.resolution["reference_year"] == 2021
    assert stored.resolved_at != nil
    assert :verified_two_hop ==
      Brain.CuriosityLearning.classify(
        hd(InvestigationHistory.for_issue(issue.id)), stored
      )

    # No third source-backed comparison is fabricated by internal reasoning.
    assert {:ok, [^ab]} = ConsolidatedConclusions.for_subject(a)
    assert {:ok, [^bc]} = ConsolidatedConclusions.for_subject(c)

    # Closed issues cannot be repeatedly investigated.
    assert %{status: :skipped, reason: :invalid_or_stale_issue} =
      Investigator.investigate_issue(issue.id)
  end

  test "a newly verified direct proof resolves a stale missing-link issue without transitive lineage", %{
    a: a, b: b, c: c
  } do
    capture!(a, b, [120_000, 100_000])
    issue = missing_issue!(b, c)
    assert issue.evidence["stored_proof_ids"] != []

    direct = capture!(b, c, [100_000, 80_000])
    assert %{kind: :direct_proof_pointer, fingerprint: fp} =
      AssociativeReasoning.propose([b, c])
    assert fp == direct.fingerprint

    # A durable direct proof makes this a direct-check planning candidate,
    # not an inferred two-hop result. The proof gate below is still mandatory.
    assert Enum.any?(Core.Curiosity.UnresolvedIssues.reasoning_candidates(20),
      &(&1.issue_id == issue.id and &1.strategy == :direct))

    assert %{status: :resolved, issue_id: id, reference_year: 2021,
             proof_id: proof_id, origin: :verified_direct_proof} =
      Investigator.investigate_issue(issue.id)
    assert id == issue.id
    assert proof_id == direct.id

    resolved = Db.get!(CognitiveIssue, issue.id)
    assert resolved.status == "resolved"
    assert resolved.resolution["reasoning_rule"] == "direct_population_comparison"
    assert resolved.resolution["origin"] == "stored_symbolic_premises"
    assert resolved.resolution["proof_id"] == direct.id
    assert resolved.resolution["proof_fingerprint"] == direct.fingerprint
    refute Map.has_key?(resolved.resolution, "parent_proof_ids")
    assert resolved.resolved_at != nil
    assert :verified_existing_direct ==
      Brain.CuriosityLearning.classify(
        hd(InvestigationHistory.for_issue(issue.id)), resolved
      )
    refute Enum.any?(Core.Curiosity.UnresolvedIssues.reasoning_candidates(20),
      &(&1.issue_id == issue.id))
  end

  test "durable direct proof resolves a stale issue after hippocampal cue is absent", %{
    a: a, b: b, c: c
  } do
    capture!(a, b, [120_000, 100_000])
    issue = missing_issue!(b, c)

    # The direct proof is persisted while hippocampal association indexing
    # is unavailable. Simulates a missing/evicted cue after warm start.
    Application.put_env(:core, :semantic_associative_recall_enabled?, false)
    direct = capture!(b, c, [100_000, 80_000])
    Application.put_env(:core, :semantic_associative_recall_enabled?, true)

    refute direct.fingerprint in SemanticAssociations.cues([b, c])
    assert AssociativeReasoning.propose([b, c]) == nil

    assert %{status: :resolved, issue_id: id, reference_year: 2021,
             proof_id: proof_id, origin: :verified_direct_db_fallback} =
      Investigator.investigate_issue(issue.id)
    assert id == issue.id
    assert proof_id == direct.id

    resolved = Db.get!(CognitiveIssue, issue.id)
    assert resolved.status == "resolved"
    assert resolved.resolution["reasoning_rule"] == "direct_population_comparison"
    assert resolved.resolution["proof_fingerprint"] == direct.fingerprint
    refute Map.has_key?(resolved.resolution, "parent_proof_ids")
  end

  test "uncued contradictory direct proofs still fail closed", %{
    a: a, b: b, c: c
  } do
    capture!(a, b, [120_000, 100_000])
    issue = missing_issue!(b, c)

    Application.put_env(:core, :semantic_associative_recall_enabled?, false)
    first = capture!(b, c, [100_000, 80_000])
    second = capture!(b, c, [105_000, 80_000])
    Application.put_env(:core, :semantic_associative_recall_enabled?, true)

    refute first.fingerprint in SemanticAssociations.cues([b, c])
    refute second.fingerprint in SemanticAssociations.cues([b, c])
    assert AssociativeReasoning.propose([b, c]) == nil

    assert %{status: :unresolved, reason: :direct_proof_not_authorized} =
      Investigator.investigate_issue(issue.id)
    assert Db.get!(CognitiveIssue, issue.id).status == "open"
  end

  test "conflicting direct proofs do not resolve a stale missing-link issue", %{
    a: a, b: b, c: c
  } do
    capture!(a, b, [120_000, 100_000])
    issue = missing_issue!(b, c)
    capture!(b, c, [100_000, 80_000])
    capture!(b, c, [105_000, 80_000])

    assert %{kind: :direct_proof_pointer} =
      AssociativeReasoning.propose([b, c])

    assert %{status: :unresolved, reason: :direct_proof_not_authorized} =
      Investigator.investigate_issue(issue.id)
    assert %{status: "open", resolution: nil} = Db.get!(CognitiveIssue, issue.id)
  end

  test "conflicting bridge evidence cannot close an issue", %{a: a, b: b, c: c} do
    capture!(a, b, [120_000, 100_000])
    issue = missing_issue!(a, c)
    capture!(b, c, [100_000, 80_000])
    capture!(b, c, [110_000, 80_000])

    assert %{kind: :two_hop_candidate} = AssociativeReasoning.propose([a, c])
    assert %{status: :unresolved, reason: :proof_not_authorized} =
      Investigator.investigate_issue(issue.id)
    assert Db.get!(CognitiveIssue, issue.id).status == "open"
  end

  test "active SelfModel load gate defers real worker without an audit attempt", %{
    a: a, b: b, c: c
  } do
    capture!(a, b, [120_000, 100_000])
    issue = missing_issue!(a, c)
    # Do not compete with older regression tests that reserve priority 1.0
    # to select their own fixtures from a shared sandbox queue.
    issue
    |> CognitiveIssue.changeset(%{priority: 0.01})
    |> Db.update!()

    name = :curiosity_busy_gate_isolated_test
    spec =
      Supervisor.child_spec(
        {Investigator, [
          name: name, enabled?: true, self_model_policy?: true,
          self_state: %{cognitive_load: 0.98, inhibition: 0.5},
          interval_ms: 60_000
        ]},
        id: name
      )

    assert {:ok, worker} = Supervisor.start_child(Symbrella.Supervisor, spec)
    on_exit(fn ->
      _ = Supervisor.terminate_child(Symbrella.Supervisor, name)
      _ = Supervisor.delete_child(Symbrella.Supervisor, name)
    end)

    owner = :persistent_term.get({:symbrella, :sandbox_owner})
    :ok = Ecto.Adapters.SQL.Sandbox.allow(Db, owner, worker)

    assert %{status: :deferred, reason: :self_model_overloaded} =
      Investigator.run_now(name)

    assert Investigator.snapshot(name).cooldown_count == 0
    assert InvestigationHistory.for_issue(issue.id) == []
    assert Db.get!(CognitiveIssue, issue.id).status == "open"
  end

  test "supervised investigator survives echoed recall telemetry and stray messages" do
    worker = Process.whereis(Investigator)
    assert is_pid(worker)

    before = Investigator.snapshot()

    send(worker, {:telemetry, [:brain, :hippocampus, :recall],
      %{window_size: 6, cue_count: 2, returned: 2, top_score: 0.33},
      %{scope: %{kind: "semantic_association"}, source: :memory}})

    # The process must also tolerate unrelated info, never treating it as
    # an instruction or exposing its contents to the cognitive pipeline.
    send(worker, {:unexpected_internal_info, %{untrusted: "not an instruction"}})

    # A call from the same sender is processed after both sent messages.
    assert Investigator.snapshot() == before
    assert Process.alive?(worker)
  end

  test "supervised cycle selects a durable issue and emits bounded telemetry", %{
    a: a, b: b, c: c
  } do
    capture!(a, b, [120_000, 100_000])
    issue = missing_issue!(a, c)
    capture!(b, c, [100_000, 80_000])

    # Give this issue a unique queue priority so the test is deterministic
    # even when other sandbox tests have left open issues behind.
    issue
    |> CognitiveIssue.changeset(%{priority: 1.0})
    |> Db.update!()

    worker = Process.whereis(Investigator)
    assert is_pid(worker)
    owner = :persistent_term.get({:symbrella, :sandbox_owner})
    :ok = Ecto.Adapters.SQL.Sandbox.allow(Db, owner, worker)

    handler_id = {__MODULE__, System.unique_integer([:positive])}
    test_pid = self()

    :ok = :telemetry.attach(
      handler_id,
      [:core, :curiosity, :symbolic_investigation],
      fn _event, measures, metadata, pid ->
        send(pid, {:investigation_telemetry, measures, metadata})
      end,
      test_pid
    )

    on_exit(fn -> :telemetry.detach(handler_id) end)

    assert %{status: :resolved, issue_id: id} = Investigator.run_now()
    assert id == issue.id
    assert %{status: "resolved"} = Db.get!(CognitiveIssue, id)

    assert_receive {:investigation_telemetry, %{count: 1},
      %{status: :resolved, issue_id: ^id, parent_proof_ids: [_, _]}}

    status = Investigator.snapshot()
    assert status.enabled?
    assert status.last_result.issue_id == id
    assert status.cycles >= 1
    assert status.last_successful_result.issue_id == issue.id

    assert [%{status: "resolved", origin: "hippocampal_two_hop_cue",
              parent_proof_ids: parent_ids, completed_at: completed}] =
      InvestigationHistory.for_issue(issue.id)
    assert length(parent_ids) == 2
    assert completed != nil
  end

  test "isolated scheduled worker honors its opt-in while the normal worker remains disabled", %{
    a: a, b: b, c: c
  } do
    capture!(a, b, [120_000, 100_000])
    issue = missing_issue!(a, c)
    capture!(b, c, [100_000, 80_000])

    # Keep selection deterministic without changing unrelated issues.
    issue
    |> CognitiveIssue.changeset(%{priority: 1.0})
    |> Db.update!()

    Application.put_env(:core, Investigator, enabled?: false)
    assert %{status: :disabled} = Investigator.run_now()

    trial_name = :curiosity_isolated_scheduled_trial_test

    spec =
      Supervisor.child_spec(
        {Investigator,
         [name: trial_name, enabled?: true, interval_ms: 60_000,
          retry_ms: 60_000]},
        id: trial_name
      )

    assert {:ok, worker} = Supervisor.start_child(Symbrella.Supervisor, spec)

    on_exit(fn ->
      _ = Supervisor.terminate_child(Symbrella.Supervisor, trial_name)
      _ = Supervisor.delete_child(Symbrella.Supervisor, trial_name)
    end)

    owner = :persistent_term.get({:symbrella, :sandbox_owner})
    :ok = Ecto.Adapters.SQL.Sandbox.allow(Db, owner, worker)

    assert %{enabled?: true, cycles: 0} = Investigator.snapshot(trial_name)

    # Exercise the same scheduled handle_info(:tick) path without adding a
    # minute to ExUnit. No investigate_issue/1 or run_now/1 call is involved.
    send(worker, :tick)
    assert %{cycles: 1,
             last_result: %{status: :resolved, issue_id: issue_id}} =
      Investigator.snapshot(trial_name)
    assert issue_id == issue.id

    assert [%{status: "resolved", worker: "isolated"}] =
      InvestigationHistory.for_issue(issue.id)

    # Another tick may select another eligible issue left in the shared
    # test sandbox. If it succeeds, the worker must remember that newer
    # success; otherwise the first success must remain recorded.
    first_success = Investigator.snapshot(trial_name).last_successful_result
    send(worker, :tick)

    assert %{cycles: 2, last_result: second_result,
             last_successful_result: last_success} =
      Investigator.snapshot(trial_name)

    if second_result.status == :resolved do
      assert last_success.issue_id == second_result.issue_id
      assert last_success.status == :resolved
      refute second_result.issue_id == issue_id
    else
      assert last_success == first_success
    end

    # The original issue stays resolved and is not investigated twice.
    assert [%{status: "resolved", worker: "isolated"}] =
      InvestigationHistory.for_issue(issue.id)

    assert %{status: "resolved", resolution: resolution} =
      Db.get!(CognitiveIssue, issue.id)
    assert resolution["reasoning_rule"] == "transitive_population_order"
    assert %{status: :disabled} = Investigator.run_now()
  end

  test "opted-in scheduled acquisition rejects malformed bridge pointers without fetching", %{
    a: a, c: c
  } do
    capture!(a, "InvestBridge#{System.unique_integer([:positive])}", [120_000, 100_000])
    issue = missing_issue!(a, c)

    # The durable issue is the only planning authority. An absent stored
    # parent pointer must not trigger a web lookup or create new facts.
    issue =
      issue
      |> CognitiveIssue.changeset(%{
        priority: 1.0,
        evidence: Map.put(issue.evidence, "stored_proof_ids", [])
      })
      |> Db.update!()

    Application.put_env(:core, Investigator,
      enabled?: false, acquire_evidence?: false)

    trial_name = :curiosity_acquisition_reject_test
    spec =
      Supervisor.child_spec(
        {Investigator, [name: trial_name, enabled?: true,
          acquire_evidence?: true, interval_ms: 60_000]},
        id: trial_name
      )
    assert {:ok, worker} = Supervisor.start_child(Symbrella.Supervisor, spec)

    on_exit(fn ->
      _ = Supervisor.terminate_child(Symbrella.Supervisor, trial_name)
      _ = Supervisor.delete_child(Symbrella.Supervisor, trial_name)
    end)

    owner = :persistent_term.get({:symbrella, :sandbox_owner})
    :ok = Ecto.Adapters.SQL.Sandbox.allow(Db, owner, worker)

    send(worker, :tick)
    assert %{cycles: 1,
      last_result: %{status: :unresolved, issue_id: id,
        acquisition: :not_captured,
        acquisition_reason: :no_unique_valid_missing_bridge}} =
      Investigator.snapshot(trial_name)
    assert id == issue.id
    assert Db.get!(CognitiveIssue, issue.id).status == "open"

    # The Core test suite reuses a shared database sandbox across cases.
    # This deliberately malformed priority-1.0 fixture cannot be allowed
    # to starve subsequent scheduled-worker tests. Mark it *blocked* only
    # after verifying fail-closed behavior; never pretend it was resolved.
    issue
    |> CognitiveIssue.changeset(%{status: "blocked"})
    |> Db.update!()

    refute Enum.any?(Core.Curiosity.UnresolvedIssues.reasoning_candidates(20),
      &(&1.issue_id == issue.id))
  end

  test "other epistemic issues never enter this narrow autonomous cycle", %{
    a: a, c: c
  } do
    names = [a, c]
    subject_key =
      names
      |> Enum.map(&String.downcase/1)
      |> Enum.sort()
      |> Enum.join("|")
      |> then(&:crypto.hash(:sha256, &1))
      |> Base.encode16(case: :lower)

    {:ok, issue} =
      CognitiveIssues.open(%{
        fingerprint: String.duplicate("e", 63) <> Integer.to_string(rem(System.unique_integer([:positive]), 10)),
        subject_key: subject_key,
        reference_year: 2021,
        kind: "contradictory_premises", status: "open",
        priority: 0.9, next_action: "recheck_sources",
        evidence: %{subjects: names}
      })

    assert %{status: :skipped, reason: :invalid_or_stale_issue} =
      Investigator.investigate_issue(issue.id)
    assert Db.get!(CognitiveIssue, issue.id).status == "open"
  end
end
