defmodule Core.Response.PopulationChainReasoningTest do
  @moduledoc """
  Real DB, real source-backed parent proofs, deterministic Core-only
  multi-hop inference. No fake model may supply a numerical conclusion.
  """
  use ExUnit.Case, async: false

  alias Core.Curiosity.UnresolvedIssues
  alias Core.Cognition.MemoryBoundary
  alias Core.Cognition.AssociativeReasoning
  alias Brain.Hippocampus.SemanticAssociations
  alias Core.Response.{
    AuthoritativeAnswerPlan, Consolidation, LlmPrompt, LlmSynthesis,
    PopulationComparison, ResponseFrame
  }
  alias Db.ConsolidatedConclusions

  defmodule OfflineLlm do
    def chat(_, _), do: raise("Two-hop proof must not invoke Qwen")
  end

  setup do
    old_compose = Application.get_env(:core, :symbolic_composition_enabled?)
    old_consolidate = Application.get_env(:core, :symbolic_consolidation_enabled?)
    old_association = Application.get_env(:core, :semantic_associative_recall_enabled?)
    old_client = Application.get_env(:core, :llm_client)
    old_retrieval = Application.get_env(:core, :web_retrieval, [])
    old_llm_cfg = Application.get_env(:core, :llm_synthesis, [])

    Application.put_env(:core, :symbolic_composition_enabled?, true)
    Application.put_env(:core, :symbolic_consolidation_enabled?, true)
    Application.put_env(:core, :semantic_associative_recall_enabled?, true)
    Application.put_env(:core, :llm_client, OfflineLlm)
    Application.put_env(:core, :web_retrieval, Keyword.put(old_retrieval, :enabled?, false))
    Application.put_env(:core, :llm_synthesis,
      Keyword.put(old_llm_cfg, :authoritative_style_selection_enabled?, false))

    on_exit(fn ->
      restore(:symbolic_composition_enabled?, old_compose)
      restore(:symbolic_consolidation_enabled?, old_consolidate)
      restore(:semantic_associative_recall_enabled?, old_association)
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

  defp sourced(a, b, values, year, source_type) do
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
    # Parent proof IDs are inspectable internally but never published in the
    # user-facing reflection/Explain payload.
    refute Map.has_key?(reflection, :symbolic_composition)
  end

  test "actual production SI recognizes which-city wording and uses verified two-hop answer", ctx do
    %{a: a, b: b, c: c} = ctx
    capture!(a, b, [120_000, 100_000])
    capture!(b, c, [100_000, 80_000])

    # Run the user's chat-shaped wording through the entire Core pipeline.
    # The LLM stub raises if invoked; no invented numbers or free-form
    # response may substitute for the independently revalidated proof.
    question = "Which city had the larger population in 2021, #{a} or #{c}?"

    si = Core.resolve_input(question,
      mode: :prod,
      enrich_lexicon?: true,
      lexicon_stage?: true,
      lifg_opts: [grammar_weight: 1.0],
      session_id: "which-city-#{ctx.sid}"
    )

    assert %{type: :location_question, domain: :geography,
             relation: :population, comparison: :greater,
             subjects: [^a, ^c], grounding_status: :unverified} =
      si.symbolic_frame

    assert get_in(si.response_meta || %{}, [:response_source]) == :symbolic_verified
    assert is_binary(si.response_text)
    assert si.response_text =~ a
    assert si.response_text =~ "40,000"
  end

  test "hippocampal associations cue a two-hop Core inference and remain private", ctx do
    %{a: a, b: b, c: c} = ctx
    ab = capture!(a, b, [120_000, 100_000])
    bc = capture!(b, c, [100_000, 80_000])

    assert ab.fingerprint in SemanticAssociations.cues([a, c])
    assert bc.fingerprint in SemanticAssociations.cues([a, c])

    assert %{kind: :two_hop_candidate, parent_proof_ids: ids} =
      AssociativeReasoning.propose([a, c])
    assert ids == Enum.sort([ab.id, bc.id])

    recalled = MemoryBoundary.recall(fresh([a, c]))
    assert recalled.hippocampal_association.kind == :two_hop_candidate
    assert recalled.composition.origin == :hippocampal_two_hop_cue
    assert recalled.composition.parent_proof_ids == ids
    assert recalled.web_evidence.support.winner == a
    assert recalled.web_evidence.support.difference == 40_000

    expression = MemoryBoundary.for_expression(recalled)
    refute Map.has_key?(expression, :hippocampal_association)
    refute Map.has_key?(expression, :composition)
    assert expression.web_evidence.support.difference == 40_000
  end

  test "a valid but uncued second premise still works through DB fallback", ctx do
    %{a: a, b: b, c: c} = ctx
    ab = capture!(a, b, [120_000, 100_000])

    Application.put_env(:core, :semantic_associative_recall_enabled?, false)
    bc = capture!(b, c, [100_000, 80_000])
    Application.put_env(:core, :semantic_associative_recall_enabled?, true)

    assert ab.fingerprint in SemanticAssociations.cues([a, c])
    refute bc.fingerprint in SemanticAssociations.cues([a, c])
    assert AssociativeReasoning.propose([a, c]) == nil

    result = MemoryBoundary.recall(fresh([a, c]))
    assert result.composition.origin == :stored_independent_proofs
    assert result.web_evidence.support.difference == 40_000
    refute Map.has_key?(result, :hippocampal_association)
  end

  test "uncued contradictory evidence vetoes the cued two-hop inference", ctx do
    %{a: a, b: b, c: c} = ctx
    ab = capture!(a, b, [120_000, 100_000])
    bc = capture!(b, c, [100_000, 80_000])

    assert %{kind: :two_hop_candidate} = AssociativeReasoning.propose([a, c])

    Application.put_env(:core, :semantic_associative_recall_enabled?, false)
    competing = capture!(b, c, [105_000, 80_000])
    Application.put_env(:core, :semantic_associative_recall_enabled?, true)

    refute competing.fingerprint in SemanticAssociations.cues([a, c])
    assert ab.fingerprint in SemanticAssociations.cues([a, c])
    assert bc.fingerprint in SemanticAssociations.cues([a, c])

    result = MemoryBoundary.recall(fresh([a, c]))
    assert result.hippocampal_association.kind == :two_hop_candidate
    refute Map.has_key?(result, :web_evidence)
    assert result.consolidation_issue.kind == :contradictory_premises
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

    # Issue queries must be scoped to the issue produced by this specific
    # reasoning attempt. The curiosity queue is global and priority-ordered;
    # older unrelated issues may legitimately appear ahead of this one.
    issue = Db.get!(Db.CognitiveIssue, unresolved.consolidation_issue.id)
    assert issue.kind == "missing_reasoning_link"
    assert issue.status == "open"
    assert issue.next_action == "find_linking_premise"
    assert issue.evidence["subjects"] == [a, c]

    candidates = UnresolvedIssues.candidates(5)
    assert length(candidates) <= 5
    assert Enum.all?(candidates, &(&1.status in ["open", "investigating"]))

    prompt = LlmPrompt.build_system_prompt(%{
      features: %{intent: :question, symbolic_frame: event([a, c])},
      consolidation_issue: unresolved.consolidation_issue,
      decision: %{action: :answer}, mood: %{}
    })
    refute prompt =~ "UNRESOLVED INFERENCE LINK"
    refute prompt =~ "find_linking_premise"
    refute prompt =~ "consolidation_issue"
  end

  test "internal recall is silent and only question-relevant uncertainty reaches the answer", ctx do
    %{a: a, b: b, c: c} = ctx
    capture!(a, b, [120_000, 100_000])

    question = "Which place has the larger population?"
    private = MemoryBoundary.recall(fresh([a, c], question))
    assert private.consolidation_issue.kind == :missing_reasoning_link
    assert MemoryBoundary.unresolved_answer(private) ==
      "I don't have sufficient source-supported evidence to make that population comparison reliably."

    public =
      private
      |> Map.put(:composition, %{parent_proof_ids: [42, 45]})
      |> MemoryBoundary.for_expression()

    refute Map.has_key?(public, :composition)
    refute Map.has_key?(public, :consolidation_issue)

    nested =
      MemoryBoundary.for_expression(%{
        features: %{consolidation_issue: private.consolidation_issue,
          composition: %{parent_proof_ids: [42]}, intent: :question},
        consolidation_issue: private.consolidation_issue
      })

    refute Map.has_key?(nested, :consolidation_issue)
    refute Map.has_key?(nested.features, :composition)
    refute Map.has_key?(nested.features, :consolidation_issue)
    assert nested.features.intent == :question

    # Even with Qwen unavailable, Core gives only a neutral answer—not issue
    # IDs, curiosity actions, a proof trace, or an unsolicited question.
    features = %{
      session_id: "silent-recall-#{ctx.sid}", intent: :question,
      symbolic_frame: event([a, c])
    }

    assert {:ok, answer, %{status: :insufficient_verified_evidence} = reflection} =
      LlmSynthesis.generate(question, features, %{action: :answer}, %{}, :default)

    assert answer =~ "sufficient source-supported evidence"
    refute answer =~ "find_linking_premise"
    refute answer =~ "missing_reasoning_link"
    refute answer =~ "parent_proof_ids"
    refute Map.has_key?(reflection, :symbolic_composition)

    {_tone, planner_answer, meta} =
      Core.Response.plan(%{
        session_id: "planner-silent-#{ctx.sid}",
        sentence: question, text: question, intent: :unknown,
        symbolic_frame: event([a, c])
      }, %{})

    assert meta.response_source == :symbolic_unresolved
    assert meta.curiosity_probe == nil
    assert planner_answer == answer
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

    issue = Db.get!(Db.CognitiveIssue, unresolved.consolidation_issue.id)
    assert issue.kind == "contradictory_premises"
    assert issue.next_action == "recheck_sources"
    assert issue.evidence["subjects"] == [a, c]
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
