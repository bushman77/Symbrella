defmodule Core.Curiosity.EvidenceAcquisitionTest do
  use ExUnit.Case, async: false

  alias Core.Curiosity.{EvidenceAcquisition, Investigator}
  alias Core.Response.{
    AuthoritativeAnswerPlan, Consolidation, PopulationComparison, ResponseFrame
  }
  alias Db.{CognitiveIssue, ConsolidatedConclusions}

  setup do
    keys = [:symbolic_consolidation_enabled?, :symbolic_composition_enabled?,
            :semantic_associative_recall_enabled?]
    previous = Enum.map(keys, &{&1, Application.get_env(:core, &1)})
    old_investigator = Application.get_env(:core, Investigator)

    Enum.each(keys, &Application.put_env(:core, &1, true))
    Application.put_env(:core, Investigator, enabled?: true, acquire_evidence?: false)

    on_exit(fn ->
      Enum.each(previous, fn
        {key, nil} -> Application.delete_env(:core, key)
        {key, value} -> Application.put_env(:core, key, value)
      end)

      if is_nil(old_investigator),
        do: Application.delete_env(:core, Investigator),
        else: Application.put_env(:core, Investigator, old_investigator)
    end)

    n = System.unique_integer([:positive])
    %{a: "AcqAlpha#{n}", b: "AcqBridge#{n}", c: "AcqGamma#{n}"}
  end

  defp context(names) do
    frame = %{type: :location_question, domain: :geography,
              relation: :population, comparison: :greater,
              subjects: names, grounding_status: :unverified}

    %{
      user_text: "Which had the larger population in 2021, #{Enum.join(names, " or ")}?",
      session_id: "acquisition-test",
      features: %{intent: :question, symbolic_frame: frame},
      symbolic_frame: %{event: frame},
      decision: %{action: :answer}, mood: %{}
    }
  end

  defp claim(subject, value) do
    %{subject: subject, value: value, census_year: 2021, reference_year: 2021,
      status: :source_supported, source_type: :wikidata,
      measurement_type: :dated_population_statement, source_table: nil,
      source_url: "https://example.invalid/#{subject}/2021"}
  end

  defp evidence(names, values) do
    %{
      status: :retrieved, relation: :population, provider: :wikidata,
      resolution: :source_supported,
      support: %{kind: :population_group, status: :source_supported,
                 subjects: names,
                 results: Enum.zip_with(names, values, &claim/2)},
      sources: [], attempted_count: 2, attempts: []
    }
  end

  defp capture!(names, values) do
    ctx =
      context(names)
      |> Map.put(:web_evidence, evidence(names, values))
      |> PopulationComparison.attach()
      |> then(fn ctx -> Map.put(ctx, :response_frame, ResponseFrame.build(ctx)) end)

    plan = AuthoritativeAnswerPlan.build(ctx)
    assert %{kind: :verified_population_comparison} = plan
    assert {:ok, row} = Consolidation.capture(ctx, plan)
    row
  end

  defp issue!(a, c) do
    result = Consolidation.recall(context([a, c]))
    assert %{id: id, kind: :missing_reasoning_link} = result.consolidation_issue
    Db.get!(CognitiveIssue, id)
  end

  defp retriever(names, values) do
    fn ctx ->
      assert get_in(ctx, [:features, :symbolic_frame, :subjects]) == names
      assert String.contains?(ctx.user_text, "2021")
      Map.put(ctx, :web_evidence, evidence(names, values))
    end
  end

  test "planner derives only the missing pair from a revalidated durable parent", %{
    a: a, b: b, c: c
  } do
    parent = capture!([a, b], [120_000, 100_000])
    issue = issue!(a, c)

    assert {:ok, request} = EvidenceAcquisition.plan(issue)
    assert request.issue_id == issue.id
    assert request.parent_id == parent.id
    assert request.year == 2021
    assert request.subjects == [b, c]
    assert request.bridge_claim["value"] == 100_000
  end

  test "validated retrieved bridge is captured but ONLY Investigator closes original issue", %{
    a: a, b: b, c: c
  } do
    parent = capture!([a, b], [120_000, 100_000])
    issue = issue!(a, c)

    assert %{status: :captured, issue_id: id, proof_id: new_id} =
      EvidenceAcquisition.acquire(issue, retriever: retriever([b, c], [100_000, 80_000]))
    assert id == issue.id
    assert new_id != parent.id
    assert Db.get!(CognitiveIssue, issue.id).status == "open"

    assert {:ok, [new_row]} = ConsolidatedConclusions.for_subject(c)
    assert new_row.id == new_id
    assert new_row.provenance == "source_supported_symbolic_derivation"

    assert %{status: :resolved, parent_proof_ids: ids,
             origin: :hippocampal_two_hop_cue} =
      Investigator.investigate_issue(issue.id)
    assert ids == Enum.sort([parent.id, new_id])
    assert Db.get!(CognitiveIssue, issue.id).resolution["reasoning_rule"] ==
      "transitive_population_order"
  end

  test "conflicting intermediate value fails closed without persisting new claims", %{
    a: a, b: b, c: c
  } do
    capture!([a, b], [120_000, 100_000])
    issue = issue!(a, c)

    assert %{status: :unresolved, reason: :retrieval_or_authorization_failed} =
      EvidenceAcquisition.acquire(issue, retriever: retriever([b, c], [101_000, 80_000]))
    assert {:ok, []} = ConsolidatedConclusions.for_subject(c)
    assert Db.get!(CognitiveIssue, issue.id).status == "open"
  end

  test "partial or absent supporting evidence leaves original issue open", %{
    a: a, b: b, c: c
  } do
    capture!([a, b], [120_000, 100_000])
    issue = issue!(a, c)

    no_evidence = fn ctx -> Map.put(ctx, :web_evidence,
      %{resolution: :partial_support, support: nil}) end

    assert %{status: :unresolved} =
      EvidenceAcquisition.acquire(issue, retriever: no_evidence)
    assert {:ok, []} = ConsolidatedConclusions.for_subject(c)
    assert Db.get!(CognitiveIssue, issue.id).status == "open"
  end

  test "a closed issue cannot reacquire or create duplicate premises", %{
    a: a, b: b, c: c
  } do
    capture!([a, b], [120_000, 100_000])
    issue = issue!(a, c)
    capture!([b, c], [100_000, 80_000])

    assert {:error, :no_unique_valid_missing_bridge} =
      EvidenceAcquisition.plan(issue)
    assert %{status: :resolved} = Investigator.investigate_issue(issue.id)
    assert {:error, :no_unique_valid_missing_bridge} =
      EvidenceAcquisition.plan(Db.get!(CognitiveIssue, issue.id))
  end
end
