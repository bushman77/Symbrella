defmodule Core.Curiosity.InvestigatorTest do
  use ExUnit.Case, async: false

  alias Brain.Hippocampus.SemanticAssociations
  alias Core.Cognition.AssociativeReasoning
  alias Core.Curiosity.Investigator
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

    assert %{status: :unresolved, reason: :no_two_hop_cue} =
      Investigator.investigate_issue(issue.id)
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

    # No third source-backed comparison is fabricated by internal reasoning.
    assert {:ok, [^ab]} = ConsolidatedConclusions.for_subject(a)
    assert {:ok, [^bc]} = ConsolidatedConclusions.for_subject(c)

    # Closed issues cannot be repeatedly investigated.
    assert %{status: :skipped, reason: :invalid_or_stale_issue} =
      Investigator.investigate_issue(issue.id)
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
