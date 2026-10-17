defmodule Core.LIFG.ScoreAuditTest do
  use ExUnit.Case, async: true

  alias Core.LIFG.ScoreAudit

  test "reports per-token probabilities and score margins without changing winners" do
    raw = [
      %{
        token_index: 0, chosen_id: "know|verb|1",
        scores: %{
          "know|verb|1" => 0.12,
          "know|noun|1" => 0.11,
          "know|verb|2" => 0.10,
          "know|verb|3" => 0.09
        }
      }
    ]

    final = [%{token_index: 0, id: "know|verb|2", score: 0.10}]
    tokens = [%{index: 0, phrase: "know"}]

    [entry] = ScoreAudit.summarize(raw, final, tokens, %{}, :all)

    assert entry.token == "know"
    assert entry.raw_winner == "know|verb|1"
    assert entry.final_winner == "know|verb|2"
    assert entry.reported_candidate_count == 4
    assert entry.candidate_count_complete?
    assert entry.reported_top_probability == 0.12
    assert entry.runner_up_id == "know|noun|1"
    assert_in_delta entry.reported_probability_margin, 0.01, 1.0e-10
    assert entry.score_kind == :per_token_post_softmax_probability

    assert raw == [%{token_index: 0, chosen_id: "know|verb|1",
      scores: %{
        "know|verb|1" => 0.12, "know|noun|1" => 0.11,
        "know|verb|2" => 0.10, "know|verb|3" => 0.09
      }}]
    assert final == [%{token_index: 0, id: "know|verb|2", score: 0.10}]
  end

  test "excludes virtual MWEs and marks partial score modes honestly" do
    raw = [
      %{token_index: 0, chosen_id: "what|pronoun|1",
        scores: %{"what|pronoun|1" => 0.4, "what|determiner|1" => 0.3}},
      %{token_index: 8, chosen_id: "what do you know|phrase|1",
        scores: %{"what do you know|phrase|1" => 1.0}}
    ]

    final = [%{token_index: 0, id: "what|pronoun|1"}]
    tokens = [%{index: 0, phrase: "What"}, %{index: 8, phrase: "what do you know"}]
    virtual = %{8 => %{phrase: "what do you know"}}

    [entry] = ScoreAudit.summarize(raw, final, tokens, virtual, :top2)
    assert entry.token_index == 0
    refute entry.candidate_count_complete?
    assert entry.reported_candidate_count == 2
  end

  test "POS mass and entropy expose distinct from merely numerous senses" do
    raw = [
      %{
        token_index: 0,
        chosen_id: "railgun|noun|1",
        scores: %{
          "railgun|noun|1" => 1.0 / 3,
          "railgun|noun|2" => 1.0 / 3,
          "railgun|noun|3" => 1.0 / 3
        }
      },
      %{
        token_index: 1,
        chosen_id: "do|auxiliary|0",
        scores: %{
          "do|auxiliary|0" => 0.60,
          "do|verb|1" => 0.20,
          "do|verb|2" => 0.20
        }
      }
    ]

    final = [
      %{token_index: 0, id: "railgun|noun|1"},
      %{token_index: 1, id: "do|auxiliary|0"}
    ]
    tokens = [%{index: 0, phrase: "railgun"}, %{index: 1, phrase: "do"}]

    [railgun, do_token] = ScoreAudit.summarize(raw, final, tokens, %{}, :all)

    assert railgun.pos_candidate_counts == %{"noun" => 3}
    assert_in_delta railgun.pos_probability_mass["noun"], 1.0, 1.0e-9
    assert_in_delta railgun.normalized_entropy, 1.0, 1.0e-9
    assert_in_delta railgun.uniform_baseline, 1.0 / 3, 1.0e-9
    assert_in_delta railgun.unit_interval_logit_ceiling,
      :math.exp(1.0) / (:math.exp(1.0) + 2), 1.0e-9

    assert do_token.pos_candidate_counts == %{"auxiliary" => 1, "verb" => 2}
    assert_in_delta do_token.pos_probability_mass["auxiliary"], 0.60, 1.0e-9
    assert_in_delta do_token.pos_probability_mass["verb"], 0.40, 1.0e-9
    assert do_token.normalized_entropy < 1.0

    [partial] =
      ScoreAudit.summarize(
        [hd(raw)], [hd(final)], [hd(tokens)], %{}, :top2
      )

    assert partial.normalized_entropy == nil
  end

  test "extracts word-score metadata from heterogeneous pipeline trace entries" do
    report = %{scores_mode: :all, entries: [%{token_index: 0}]}

    trace = [
      {:activated, %{rows: 137, lifg_choices: 7, shape: :activate_cells}},
      %{stage: :lifg_run, reason: :stage1_choices_available},
      :legacy_trace_event,
      %{stage: :lifg_word_score_audit, meta: report},
      %{stage: :lifg_word_score_audit, meta: %{entries: []}}
    ]

    assert ScoreAudit.trace_reports(trace) == [report]
    assert ScoreAudit.trace_reports([{:activated, %{rows: 1}}]) == []
    assert ScoreAudit.trace_reports(nil) == []
  end

  test "empty score maps remain unknown rather than implying confidence" do
    [entry] =
      ScoreAudit.summarize(
        [%{token_index: 2, chosen_id: "you|pronoun|1", scores: %{}}],
        [%{token_index: 2, id: "you|pronoun|1"}],
        [%{index: 2, phrase: "you"}], %{}, :none
      )

    assert entry.reported_candidate_count == 0
    assert entry.reported_top_probability == nil
    assert entry.reported_probability_margin == nil
    refute entry.candidate_count_complete?
  end
end
