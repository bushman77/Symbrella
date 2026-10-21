defmodule Core.Curiosity.InvestigationHistoryTest do
  use ExUnit.Case, async: false

  alias Core.Curiosity.InvestigationHistory
  alias Db.{CognitiveIssues, CuriosityInvestigations}

  setup do
    unique = System.unique_integer([:positive])
    key =
      :crypto.hash(:sha256, "audit-pair-" <> Integer.to_string(unique))
      |> Base.encode16(case: :lower)

    fingerprint =
      :crypto.hash(:sha256, "audit-issue-" <> Integer.to_string(unique))
      |> Base.encode16(case: :lower)

    {:ok, issue} =
      CognitiveIssues.open(%{
        fingerprint: fingerprint, subject_key: key,
        kind: "missing_reasoning_link", status: "blocked",
        reference_year: 2021, priority: 0.5,
        evidence: %{"subjects" => ["AuditAlpha", "AuditGamma"],
                    "reason" => "no_shared_intermediate_premise"},
        next_action: "find_linking_premise"
      })

    %{issue: issue}
  end

  test "start is durable and a failed/crashed attempt remains visibly unfinished", %{issue: issue} do
    id = InvestigationHistory.begin_attempt(issue.id, name: :audit_isolated_trial)
    assert is_integer(id)

    assert [%{id: ^id, status: "started", completed_at: nil}] =
      InvestigationHistory.for_issue(issue.id)

    assert {:ok, explanation} = InvestigationHistory.explain(issue.id)
    assert explanation.issue.status == "blocked"
    assert explanation.issue.subjects == ["AuditAlpha", "AuditGamma"]
    assert [%{status: "started", completed_at: nil}] = explanation.attempts
    assert explanation.accepted_proofs == []
  end

  test "completed attempt retains only safe status, provider summaries and proof pointers", %{
    issue: issue
  } do
    id = InvestigationHistory.begin_attempt(issue.id, name: :audit_isolated_trial)

    assert :ok = InvestigationHistory.complete_attempt(
      id,
      %{
        status: :unresolved, reason: :no_two_hop_cue,
        acquisition: :not_captured, acquisition_reason: :retrieval_or_authorization_failed,
        parent_proof_ids: [18, 22, 22, -1],
        acquired_proof_id: nil,
        provider_attempts: [
          %{provider: "wikidata", status: "source_supported"},
          %{provider: "arbitrary_external_site", status: "unsafe"},
          %{provider: "statistics_canada", status: "unavailable"}
        ],
        user_text: "private text never persisted",
        sources: ["https://untrusted.example/test"],
        origin: :retrieval_failed
      },
      23
    )

    assert [saved] = CuriosityInvestigations.for_issue(issue.id, 1)
    assert saved.id == id
    assert saved.status == "unresolved"
    assert saved.reason == "no_two_hop_cue"
    assert saved.worker == "isolated"
    assert saved.acquisition == "not_captured"
    assert saved.acquisition_reason == "retrieval_or_authorization_failed"
    assert saved.parent_proof_ids == [18, 22]
    assert saved.duration_ms == 23
    assert saved.provider_attempts == [
      %{"provider" => "wikidata", "status" => "source_supported"},
      %{"provider" => "statistics_canada", "status" => "unavailable"}
    ]
    assert saved.completed_at != nil

    assert {:error, :not_started} =
      CuriosityInvestigations.finish_attempt(id, %{status: "resolved"})

    assert {:ok, explained} = InvestigationHistory.explain(issue.id)
    assert [%{status: "unresolved", acquisition: "not_captured"}] =
      explained.attempts
    assert explained.resolution == nil
  end

  test "a durable audit attempt does not itself make an accepted learning success", %{issue: issue} do
    id = InvestigationHistory.begin_attempt(issue.id, [])
    assert :ok =
      InvestigationHistory.complete_attempt(
        id,
        %{status: :resolved, origin: :verified_direct_proof},
        10
      )

    [audit] = InvestigationHistory.for_issue(issue.id, 1)
    assert :ignored == Brain.CuriosityLearning.classify(audit, Db.get!(Db.CognitiveIssue, issue.id))
  end

  test "history query remains bounded and preserves newest attempts", %{issue: issue} do
    first = InvestigationHistory.begin_attempt(issue.id, [])
    assert :ok = InvestigationHistory.complete_attempt(
      first, %{status: :unresolved, reason: :no_two_hop_cue}, 5)

    second = InvestigationHistory.begin_attempt(issue.id, [])
    assert :ok = InvestigationHistory.complete_attempt(
      second, %{status: :failed, reason: :investigation_unavailable}, 3)

    assert [%{id: ^second}] = InvestigationHistory.for_issue(issue.id, 1)
    assert [%{id: ^second}, %{id: ^first}] =
      InvestigationHistory.for_issue(issue.id, 50)
    assert InvestigationHistory.for_issue(issue.id, -3) == []
    assert {:error, :not_found} = InvestigationHistory.explain(-1)
  end
end
