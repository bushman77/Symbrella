defmodule Core.Response.PopulationChainReasoningTest do
  @moduledoc """
  Real DB, real source-backed parent proofs, deterministic Core-only
  multi-hop inference. No fake model may supply a numerical conclusion.
  """
  use ExUnit.Case, async: false

  alias Core.Curiosity.UnresolvedIssues
  alias Core.Response.{
    AuthoritativeAnswerPlan, Consolidation, LlmPrompt, LlmSynthesis,
    PopulationComparison, ResponseFrame
  }
  alias Db.{CognitiveIssues, ConsolidatedConclusions}

  defmodule OfflineLlm do
    def chat(_, _), do: raise("Two-hop proof must not invoke Qwen")
  end

  setup do
    old_compose = Application.get_env(:core, :symbolic_composition_enabled?)
    old_consolidate = Application.get_env(:core, :symbolic_consolidation_enabled?)
    old_client = Application.get_env(:core, :llm_client)
    old_retrieval = Application.get_env(:core, :web_retrieval, [])
    old_llm_cfg = Application.get_env(:core, :llm_synthesis, [])

    Application.put_env(:core, :symbolic_composition_enabled?, true)
    Application.put_env(:core, :symbolic_consolidation_enabled?, true)
    Application.put_env(:core, :llm_client, OfflineLlm)
    Application.put_env(:core, :web_retrieval, Keyword.put(old_retrieval, :enabled?, false))
    Application.put_env(:core, :llm_synthesis,
      Keyword.put(old_llm_cfg, :authoritative_style_selection_enabled?, false))

    on_exit(fn ->
      restore(:symbolic_composition_enabled?, old_compose)
      restore(:symbolic_consolidation_enabled?, old_consolidate)
      restore(:llm_client, old_client)
      Application.put_env(:core, :web_retrieval, old_retrieval)
      Application.put_env(:core, :llm_synthesis, old_llm_cfg)
    end)

    uid = System.unique_integer([:positive])
    %{a: "ChainAlpha#{uid}", b: "ChainBeta#{uid}", c: "ChainGamma#{uid}",
      sid: "chain-#{uid}"}
  end

  defp restore(key, nil), do: Application.delete_env(:core, key)
  defp restore(key, value), do: Application.put_env(:core, key, value)

  defp event(names) do
    %{type: :location_question, domain: :geography,
      relation: :population, comparison: :greater,
      subjects: names, grounding_status: :unverified}
  end

  defp claim(subject, count, year, source_type) do
    %{
      subject: subject, value: count, census_year: year,
      status: :source_supported, source_type: source_type,
      source_url: "https://example.invalid/#{subject}/population/#{year}"
    }
  end

  defp sourced(a, b, values, year, source_type \\ :wikidata) do
    names = [a, b]
    frame = event(names)

    %{
      session_id: "prior-reasoning",
      user_text: "Which place had the larger population in #{year}?",
      features: %{intent: :question, symbolic_frame: frame},
      symbolic_frame: %{event: frame},
      mood: %{}, decision: %{action: :answer},
      web_evidence: %{
        status: :retrieved, resolution: :source_supported,
        provider: source_type, relation: :population,
        support: %{kind: :population_group, status: :source_supported,
          subjects: names,
          results: [
            claim(a, Enum.at(values, 0), year, source_type),
            claim(b, Enum.at(values, 1), year, source_type)
          ]},
        sources: [], attempted_count: 2, attempts: []
      }
    }
    |> PopulationComparison.attach()
    |> then(fn ctx -> Map.put(ctx, :response_frame, ResponseFrame.build(ctx)) end)
  end

  defp capture!(a, b, values, year \\ 2021, source_type \\ :wikidata) do
    ctx = sourced(a, b, values, year, source_type)
    plan = AuthoritativeAnswerPlan.build(ctx)
    assert %{kind: :verified_population_comparison} = plan
    assert {:ok, row} = Consolidation.capture(ctx, plan)
    row
  end

  defp fresh([a, c], question \\ "Which place has the larger population?") do
    frame = event([a, c])

    %{
      user_text: question,
      session_id: "new-session-#{System.unique_integer([:positive])}",
      features: %{intent: :question, symbolic_frame: frame},
      symbolic_frame: %{event: frame},
      mood: %{}, decision: %{action: :answer}
    }
  end

  test "two independently supported A>B and B>C proofs yield A>C without Qwen", ctx do
    %{a: a, b: b, c: c} = ctx
    ab = capture!(a, b, [120_000, 100_000])
    bc = capture!(b, c, [100_000, 80_000])

    result = Consolidation.recall(fresh([a, c]))
    assert result.composition.rule == :transitive_population_order
    assert result.composition.intermediate_subject == b
    assert result.composition.parent_proof_ids == Enum.sort([ab.id, bc.id])
    assert result.web_evidence.evidence_origin == :composed_symbolic_premises
    assert result.web_evidence.support.winner == a
    assert result.web_evidence.support.difference == 40_000
    assert result.web_evidence.support.year == 2021

    frame = ResponseFrame.build(result)
    plan = AuthoritativeAnswerPlan.build(Map.put(result, :response_frame, frame))
    assert plan.kind == :verified_population_comparison
    assert AuthoritativeAnswerPlan.render(plan, :brief) =~ "40,000"

    # A composed result must retain its lineage, NOT masquerade as a fresh
    # independent retrieval and write itself back as a source-backed parent.
    assert {:error, :not_authorized_for_consolidation} =
      Consolidation.capture(Map.put(result, :response_frame, frame), plan)
    assert ConsolidatedConclusions.for_subject(a) == {:ok, [ab]}

    features = %{
      session_id: "different-session-#{ctx.sid}",
      intent: :question,
      symbolic_frame: event([a, c])
    }

    assert {:ok, answer, reflection} =
      LlmSynthesis.generate("Which place has the larger population?",
        features, %{action: :answer}, %{}, :default)
    assert answer =~ a
    assert answer =~ "40,000"
    assert reflection.authoritative_plan.status == :enforced
    assert reflection.symbolic_composition.rule == :transitive_population_order
    assert reflection.symbolic_composition.parent_proof_ids == Enum.sort([ab.id, bc.id])
  end

  test "chain works in the other direction by independently rederiving its ordering", ctx do
    %{a: a, b: b, c: c} = ctx
    capture!(a, b, [80_000, 100_000])
    capture!(b, c, [100_000, 125_000])

    answer = Consolidation.recall(fresh([a, c], "Which had more residents in 2021?"))
    assert answer.composition.intermediate_subject == b
    assert answer.web_evidence.support.winner == c
    assert answer.web_evidence.support.difference == 45_000
  end

  test "a missing intermediate premise becomes a prioritized curiosity issue", ctx do
    %{a: a, b: b, c: c} = ctx
    capture!(a, b, [120_000, 100_000])

    unresolved = Consolidation.recall(fresh([a, c]))
    refute Map.has_key?(unresolved, :web_evidence)
    assert unresolved.consolidation_issue.kind == :missing_reasoning_link

    [issue | _] = CognitiveIssues.pending()
    assert issue.kind == "missing_reasoning_link"
    assert issue.next_action == "find_linking_premise"
    assert issue.evidence["subjects"] == [a, c]
    assert [%{issue_id: id} | _] = UnresolvedIssues.candidates()
    assert id == issue.id

    prompt = LlmPrompt.build_system_prompt(%{
      features: %{intent: :question, symbolic_frame: event([a, c])},
      consolidation_issue: unresolved.consolidation_issue,
      decision: %{action: :answer}, mood: %{}
    })
    assert prompt =~ "UNRESOLVED INFERENCE LINK"
  end

  test "curiosity issue resolves when the missing verified link is later added", ctx do
    %{a: a, b: b, c: c} = ctx
    capture!(a, b, [120_000, 100_000])

    first = Consolidation.recall(fresh([a, c]))
    issue_id = first.consolidation_issue.id
    assert Db.get!(Db.CognitiveIssue, issue_id).status == "open"

    capture!(b, c, [100_000, 80_000])
    second = Consolidation.recall(fresh([a, c]))

    assert second.composition.intermediate_subject == b
    assert second.web_evidence.support.difference == 40_000

    resolved = Db.get!(Db.CognitiveIssue, issue_id)
    assert resolved.status == "resolved"
    assert resolved.resolution["reasoning_rule"] == "transitive_population_order"
    assert resolved.resolved_at != nil
    refute Enum.any?(UnresolvedIssues.candidates(), &(&1.issue_id == issue_id))
  end

  test "contradictory intermediate populations cannot produce a transitive answer", ctx do
    %{a: a, b: b, c: c} = ctx
    ab = capture!(a, b, [120_000, 100_000])
    bc = capture!(b, c, [105_000, 80_000])

    unresolved = Consolidation.recall(fresh([a, c]))
    refute Map.has_key?(unresolved, :web_evidence)
    assert unresolved.consolidation_issue.kind == :contradictory_premises

    [issue | _] = CognitiveIssues.pending()
    assert issue.next_action == "recheck_sources"
    assert Enum.sort(issue.evidence["stored_proof_ids"]) == Enum.sort([ab.id, bc.id])
  end

  test "different years or source categories cannot be chained", ctx do
    %{a: a, b: b, c: c} = ctx
    capture!(a, b, [120_000, 100_000], 2021)
    capture!(b, c, [100_000, 80_000], 2022)

    incompatible_years = Consolidation.recall(fresh([a, c]))
    refute Map.has_key?(incompatible_years, :web_evidence)
    assert incompatible_years.consolidation_issue.kind == :missing_reasoning_link
  end

  test "different measurement sources never silently merge into a proof", ctx do
    %{a: a, b: b, c: c} = ctx
    capture!(a, b, [120_000, 100_000], 2021, :wikidata)
    capture!(b, c, [100_000, 80_000], 2021, :wikipedia)

    incompatible = Consolidation.recall(fresh([a, c]))
    refute Map.has_key?(incompatible, :web_evidence)
    assert incompatible.consolidation_issue.kind == :missing_reasoning_link
  end

  test "a nonmonotonic pair does not justify transitive ordering", ctx do
    %{a: a, b: b, c: c} = ctx
    capture!(a, b, [80_000, 100_000])
    capture!(b, c, [100_000, 90_000])

    result = Consolidation.recall(fresh([a, c]))
    refute Map.has_key?(result, :web_evidence)
    assert result.consolidation_issue.kind == :missing_reasoning_link
  end

  test "explicit freshness and feature gate prevent historical chain reuse", ctx do
    %{a: a, b: b, c: c} = ctx
    capture!(a, b, [120_000, 100_000])
    capture!(b, c, [100_000, 80_000])

    current = Consolidation.recall(fresh([a, c], "Which place has the current population?"))
    refute Map.has_key?(current, :web_evidence)

    Application.put_env(:core, :symbolic_composition_enabled?, false)
    disabled = Consolidation.recall(fresh([a, c]))
    refute Map.has_key?(disabled, :web_evidence)
    refute Map.has_key?(disabled, :composition)
  end
end
