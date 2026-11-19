defmodule Core.Comprehension.DiagnosisTest do
  use ExUnit.Case, async: true

  alias Core.Comprehension.{Diagnosis, Summary}

  test "names the actual weak/fallback causes without promoting LIFG scores to understanding" do
    si = %{
      sentence: "what does the ambiguous word mean?",
      source: :user,
      session_id: "session-diagnosis",
      intent: :question,
      lifg_choices: [
        %{id: "word|noun|fallback", lemma: "word", score: 0.35,
          token_index: 0},
        %{id: "mean|verb|0", lemma: "mean", score: 0.41,
          token_index: 1}
      ],
      structural_competition: %{status: :preferred},
      sentence_meaning: %{status: :provisional},
      integration_audit: %{status: :consistent},
      lifg_feedback: %{mode: :bounded_reanalysis, applied?: false}
    }

    summary = Summary.build(si)
    assert summary.degraded?
    assert :weak_decision_rate_high in summary.reasons
    assert :fallback_rate_high in summary.reasons
    assert summary.understood == []

    diagnosis = Diagnosis.assess(Map.put(si, :comprehension, summary))
    assert diagnosis.reasons == summary.reasons
    assert diagnosis.choice_count == 2
    assert diagnosis.weak_rate == 1.0
    assert diagnosis.fallback_rate == 0.5
    assert diagnosis.verified_lexical_count == 0
    assert diagnosis.structural_status == :preferred
    assert diagnosis.sentence_meaning_status == :provisional
    assert diagnosis.integration_status == :consistent
    assert diagnosis.feedback_mode == :bounded_reanalysis
    assert diagnosis.feedback_applied == false
    assert diagnosis.repair_hint == :review_lexical_evidence
    assert diagnosis.curiosity_gap_gate == :needs_unresolved_content
    refute inspect(diagnosis) =~ "ambiguous word"
  end

  test "structurally interpreted does not imply independently verified lexical meaning" do
    si = %{
      comprehension: %{
        degraded?: false, reasons: [], understood: [],
        uncertain: [], unresolved: [], lexical_meanings: [],
        stats: %{total: 3, weak_rate: 0.0, fallback_rate: 0.0,
                 unresolved_content: 0}
      },
      structural_competition: %{status: :preferred},
      sentence_meaning: %{status: :structurally_interpreted},
      integration_audit: %{status: :consistent}
    }

    diagnosis = Diagnosis.assess(si)
    assert diagnosis.verified_lexical_count == 0
    assert diagnosis.sentence_meaning_status == :structurally_interpreted
    assert diagnosis.repair_hint == :not_indicated
    assert diagnosis.curiosity_gap_gate == :not_degraded
    assert diagnosis.feedback_mode == :not_observed
    assert diagnosis.feedback_applied == :not_observed
  end

  test "gap review requires uncertainty plus unresolved content and same-session user origin" do
    summary = %{
      degraded?: true, reasons: [:unresolved_content_tokens],
      understood: [], uncertain: ["PRIVATE uncertainty"],
      unresolved: ["PRIVATE unresolved"], lexical_meanings: [],
      stats: %{total: 2, weak_rate: 0.25, fallback_rate: 0.0,
               unresolved_content: 1}
    }
    user = %{
      source: :prod, experience_origin: :user_turn,
      session_id: "same-session", sentence: "PRIVATE utterance",
      comprehension: summary
    }

    diagnosis = Diagnosis.assess(user)
    assert diagnosis.curiosity_gap_gate == :eligible_for_review
    assert diagnosis.repair_hint == :review_unresolved_content
    assert diagnosis.unresolved_content_count == 1
    refute inspect(diagnosis) =~ "PRIVATE"

    assert Diagnosis.assess(%{user | session_id: nil}).curiosity_gap_gate == :needs_session
    assert Diagnosis.assess(%{user | experience_origin: :system}).curiosity_gap_gate ==
             :needs_user_origin
    assert Diagnosis.assess(%{user | sentence: nil}).curiosity_gap_gate == :needs_input
    assert Diagnosis.assess(%{user | comprehension: %{summary | unresolved: []}}).
             curiosity_gap_gate == :needs_unresolved_content
  end

  test "unknown metadata fails closed to bounded enum fields" do
    si = %{
      comprehension: %{
        degraded?: true, reasons: [:unrecognized_reason, :fallback_rate_high,
                                   :fallback_rate_high, "PRIVATE"],
        uncertain: ["PRIVATE"], unresolved: [], lexical_meanings: [],
        stats: %{total: 100_001, weak_rate: -1.0, fallback_rate: 40.0,
                 unresolved_content: -3}
      },
      structural_competition: %{status: :invented_status},
      sentence_meaning: %{status: :fake_verified},
      integration_audit: %{status: :untrusted},
      lifg_feedback: %{mode: :invented_mode, applied?: true,
                       applied_revisions: [%{from: "PRIVATE"}]}
    }
    diagnosis = Diagnosis.assess(si)

    assert diagnosis.reasons == [:fallback_rate_high]
    assert diagnosis.choice_count == nil
    assert diagnosis.weak_rate == nil
    assert diagnosis.fallback_rate == nil
    assert diagnosis.unresolved_content_count == nil
    assert diagnosis.structural_status == :not_observed
    assert diagnosis.sentence_meaning_status == :not_observed
    assert diagnosis.integration_status == :not_observed
    assert diagnosis.feedback_mode == :not_observed
    assert diagnosis.feedback_applied == true
    assert diagnosis.curiosity_gap_gate == :needs_unresolved_content
    refute inspect(diagnosis) =~ "PRIVATE"
  end

  test "no summary yields no new diagnosis instead of inventing a reason" do
    assert Diagnosis.assess(%{}) == %{}
    assert Diagnosis.assess(nil) == %{}
  end
end
