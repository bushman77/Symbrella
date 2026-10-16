defmodule Core.Response.SymbolicConsolidationTest do
  @moduledoc """
  Deterministic integration: backed by the real PostgreSQL test sandbox, not
  by chat history or a fake in-memory store. A new session and an unavailable
  LLM must still be able to re-derive a source-supported comparison.
  """
  use ExUnit.Case, async: false

  import Ecto.Query, only: [from: 2]

  alias Core.Curiosity.UnresolvedIssues
  alias Core.Response.{
    AuthoritativeAnswerPlan, Consolidation, LlmPrompt, LlmSynthesis,
    PopulationComparison, ResponseFrame
  }
  alias Db.{CognitiveIssue, CognitiveIssues, ConsolidatedConclusion, ConsolidatedConclusions}

  defmodule OfflineLlm do
    def chat(_, _), do: raise("Offline LLM should never be invoked")
  end

  setup do
    old_toggle = Application.get_env(:core, :symbolic_consolidation_enabled?)
    old_client = Application.get_env(:core, :llm_client)
    old_retrieval = Application.get_env(:core, :web_retrieval, [])
    old_llm_cfg = Application.get_env(:core, :llm_synthesis, [])

    Application.put_env(:core, :symbolic_consolidation_enabled?, true)
    Application.put_env(:core, :llm_synthesis,
      Keyword.put(old_llm_cfg, :authoritative_style_selection_enabled?, false))
    Application.put_env(:core, :llm_client, OfflineLlm)
    Application.put_env(:core, :web_retrieval,
      Keyword.put(old_retrieval, :enabled?, false))

    uid = System.unique_integer([:positive])
    names = ["ConsolidationTown#{uid}", "ReasoningTown#{uid}"]

    on_exit(fn ->
      if is_nil(old_toggle),
        do: Application.delete_env(:core, :symbolic_consolidation_enabled?),
        else: Application.put_env(:core, :symbolic_consolidation_enabled?, old_toggle)

      if is_nil(old_client),
        do: Application.delete_env(:core, :llm_client),
        else: Application.put_env(:core, :llm_client, old_client)

      Application.put_env(:core, :web_retrieval, old_retrieval)
      Application.put_env(:core, :llm_synthesis, old_llm_cfg)
    end)

    %{names: names, session: "consolidation-#{uid}"}
  end

  defp frame(names) do
    %{type: :location_question, domain: :geography,
      relation: :population, comparison: :greater,
      subjects: names, grounding_status: :unverified}
  end

  defp claims(names, values, years) do
    urls = [
      "https://www.wikidata.org/wiki/Q473209",
      "https://www.wikidata.org/wiki/Q16740"
    ]

    Enum.zip([names, values, years, urls])
    |> Enum.map(fn {name, value, year, url} ->
      %{subject: name, status: :source_supported, value: value,
        census_year: year, source_url: url, source_type: :wikidata}
    end)
  end

  defp sourced_context(names, values \\ [97_902, 90_990], years \\ [2021, 2021]) do
    event = frame(names)
    proof_sources = claims(names, values, years)

    %{
      user_text: "Which of the two places has the larger population?",
      session_id: "original-proof",
      features: %{intent: :question, symbolic_frame: event},
      symbolic_frame: %{event: event},
      decision: %{action: :answer},
      mood: %{},
      web_evidence: %{
        status: :retrieved, resolution: :source_supported,
        relation: :population, provider: :wikidata,
        support: %{
          kind: :population_group, status: :source_supported,
          subjects: names, results: proof_sources
        },
        sources: [], attempts: [], attempted_count: 2
      }
    }
    |> PopulationComparison.attach()
    |> then(fn context ->
      Map.put(context, :response_frame, ResponseFrame.build(context))
    end)
  end

  defp fresh_context(names, text \\ "Which place has the larger population?") do
    event = frame(names)

    %{
      session_id: "new-session-#{System.unique_integer([:positive])}",
      user_text: text,
      features: %{intent: :question, symbolic_frame: event},
      symbolic_frame: %{event: event},
      decision: %{action: :answer},
      mood: %{}
    }
  end

  defp capture!(ctx) do
    plan = AuthoritativeAnswerPlan.build(ctx)
    assert %{kind: :verified_population_comparison} = plan
    assert {:ok, %ConsolidatedConclusion{} = row} = Consolidation.capture(ctx, plan)
    row
  end

  test "source-backed deduction persists, deduplicates and survives a new session without Qwen", %{
    names: names
  } do
    original = sourced_context(names)
    first = capture!(original)
    again = capture!(original)

    assert first.id == again.id
    assert first.premises |> length() == 2
    assert first.derivation["difference"] == 6912
    assert first.provenance == "source_supported_symbolic_derivation"
    assert length(ConsolidatedConclusions.recent(first.pair_key)) == 1

    fresh = fresh_context(names)
    remembered = Consolidation.recall(fresh)
    assert remembered.consolidation.origin == :stored_symbolic_premises
    assert remembered.web_evidence.evidence_origin == :consolidated_symbolic_memory
    assert remembered.web_evidence.support.difference == 6912
    assert remembered.web_evidence.support.winner == hd(names)

    # Rendering still uses Core's independent grounded authorization gate.
    frame = ResponseFrame.build(remembered)
    plan = AuthoritativeAnswerPlan.build(Map.put(remembered, :response_frame, frame))
    assert plan.kind == :verified_population_comparison
    refute Map.has_key?(fresh, :web_evidence)

    features = %{
      session_id: fresh.session_id,
      intent: :question, symbolic_frame: frame(names)
    }

    assert {:ok, answer, reflection} = LlmSynthesis.generate(
      fresh.user_text, features, %{action: :answer}, %{}, :default)

    assert answer =~ hd(names)
    assert answer =~ "6,912"
    assert reflection.authoritative_plan.status == :enforced
  end

  test "reversed subject order re-derives the original proof safely", %{names: names} do
    capture!(sourced_context(names))
    reversed = Enum.reverse(names)

    new_context = fresh_context(reversed)
    recalled = Consolidation.recall(new_context)

    assert recalled.web_evidence.support.subjects == reversed
    assert recalled.web_evidence.support.winner == hd(names)
    assert recalled.web_evidence.support.difference == 6912
    assert %{} = AuthoritativeAnswerPlan.build(
      Map.put(recalled, :response_frame, ResponseFrame.build(recalled)))
  end

  test "freshness and mismatched year cannot silently promote a stored historical proof", %{
    names: names
  } do
    capture!(sourced_context(names))

    current = fresh_context(names, "Which place has the latest population?")
    refute Map.has_key?(Consolidation.recall(current), :web_evidence)

    year_query = fresh_context(names, "Which place had a larger population in 2022?")
    unresolved = Consolidation.recall(year_query)
    refute Map.has_key?(unresolved, :web_evidence)
    assert unresolved.consolidation_issue.kind == :missing_dated_evidence

    [issue | _] = CognitiveIssues.pending()
    assert issue.kind == "missing_dated_evidence"
    assert issue.reference_year == 2022
    assert issue.next_action == "find_dated_sources"
    assert issue.evidence["subjects"] == names

    assert [%{issue_id: id, kind: "missing_dated_evidence"} | _] =
      UnresolvedIssues.candidates()
    assert id == issue.id
  end

  test "a new year-specific supported proof resolves its outstanding evidence gap", %{
    names: names
  } do
    capture!(sourced_context(names))
    query = fresh_context(names, "Which place had a larger population in 2022?")

    first = Consolidation.recall(query)
    issue_id = first.consolidation_issue.id
    assert Db.get!(CognitiveIssue, issue_id).status == "open"

    newer = capture!(sourced_context(names, [101_000, 95_000], [2022, 2022]))
    assert newer.reference_year == 2022

    resolved = Db.get!(CognitiveIssue, issue_id)
    assert resolved.status == "resolved"
    assert resolved.resolution["proof_fingerprint"] == newer.fingerprint
    assert resolved.resolved_at != nil

    second = Consolidation.recall(query)
    assert second.web_evidence.support.year == 2022
    assert second.web_evidence.support.difference == 6_000
    refute Map.has_key?(second, :consolidation_issue)
  end

  test "contradictory supported premises remain unresolved instead of selecting latest claim", %{
    names: names
  } do
    first = capture!(sourced_context(names))
    second = capture!(sourced_context(names, [90_000, 99_000]))

    refute first.id == second.id
    assert length(ConsolidatedConclusions.recent(first.pair_key)) == 2

    unresolved = Consolidation.recall(fresh_context(names))
    refute Map.has_key?(unresolved, :web_evidence)
    assert unresolved.consolidation_issue.kind == :contradictory_premises

    [issue | _] = CognitiveIssues.pending()
    assert issue.kind == "contradictory_premises"
    assert issue.priority == 0.9
    assert issue.next_action == "recheck_sources"
    prompt = LlmPrompt.build_system_prompt(%{
      features: %{intent: :question, symbolic_frame: frame(names)},
      consolidation_issue: unresolved.consolidation_issue,
      decision: %{action: :answer}, mood: %{}
    })
    assert prompt =~ "UNRESOLVED EVIDENCE CONFLICT"
    assert prompt =~ "Do not choose a winner"
    assert Enum.sort(issue.evidence["stored_proof_ids"]) ==
      Enum.sort([first.id, second.id])

    repeat = Consolidation.recall(fresh_context(names))
    assert repeat.consolidation_issue.id == issue.id
    assert Db.aggregate(from(i in CognitiveIssue,
      where: i.subject_key == ^first.pair_key), :count) == 1
  end

  test "unverified and previously consolidated assistant text cannot create knowledge", %{
    names: names
  } do
    original = sourced_context(names)
    plan = AuthoritativeAnswerPlan.build(original)

    # The original context has valid source support: capturing it here
    # would legitimately persist a proof and invalidate the empty-store
    # assertion below. First verify that the opt-out gate causes NO write.
    Application.put_env(:core, :symbolic_consolidation_enabled?, false)
    assert :disabled = Consolidation.capture(original, plan)
    assert ConsolidatedConclusions.recent(first_pair_key(names)) == []

    Application.put_env(:core, :symbolic_consolidation_enabled?, true)

    # Unsupported input cannot inherit the authorization of an earlier plan.
    assert {:error, :not_authorized_for_consolidation} =
      Consolidation.capture(Map.delete(original, :web_evidence), plan)

    # A replayed/consolidated conclusion cannot certify itself as new evidence.
    remembered_origin = put_in(original, [:web_evidence, :evidence_origin],
      :consolidated_symbolic_memory)

    assert {:error, :not_authorized_for_consolidation} =
      Consolidation.capture(remembered_origin, plan)

    # A valid context cannot authorize a tampered symbolic conclusion.
    assert {:error, :not_authorized_for_consolidation} =
      Consolidation.capture(original, Map.put(plan, :winner, List.last(names)))

    # None of these rejected attempts created knowledge.
    assert ConsolidatedConclusions.recent(first_pair_key(names)) == []
  end

  defp first_pair_key(names) do
    names
    |> Enum.map(&(String.downcase(String.trim(&1))))
    |> Enum.sort()
    |> Enum.join("|")
    |> then(&:crypto.hash(:sha256, &1))
    |> Base.encode16(case: :lower)
  end
end
