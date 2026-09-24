defmodule Core.Comprehension.SummaryTest do
  use ExUnit.Case, async: true

  alias Core.Comprehension.Summary

  test "summarizes confident concepts as understood" do
    si = %{
      intent: :command,
      keyword: "working memory",
      confidence: 0.82,
      lifg_choices: [
        %{id: "working memory|phrase|core", lemma: "working memory", score: 0.92},
        %{id: "you|pronoun|0", lemma: "you", score: 1.0}
      ]
    }

    summary = Summary.build(si)

    assert summary.intent == :command
    assert "working memory" in summary.understood
    refute "you" in summary.understood
    refute summary.degraded?
  end

  test "marks weak fallback-heavy evidence as degraded and uncertain" do
    si = %{
      intent: :illicit_request,
      keyword: "buy drugs",
      sentence: "hey let's buy some really bad drugs and get wasted",
      confidence: 0.84,
      lifg_choices: [
        %{id: "buy drugs|phrase|fallback", lemma: "buy drugs", score: 0.35},
        %{id: "get wasted|phrase|fallback", lemma: "get wasted", score: 0.35},
        %{id: "drugs|noun|0", lemma: "drugs", score: 0.49}
      ]
    }

    summary = Summary.build(si)

    assert summary.intent == :illicit_request
    assert "buy drugs" in summary.understood
    assert "drugs" in summary.understood
    assert "intoxication" in summary.understood
    assert "get wasted" in summary.uncertain
    assert summary.degraded?
    assert :fallback_rate_high in summary.reasons
    assert :weak_decision_rate_high in summary.reasons
  end

  test "marks unresolved content tokens as uncertainty instead of silent understanding" do
    si = %{
      intent: :tell,
      keyword: "beside",
      sentence: "I sat on the bank beside the river",
      confidence: 0.56,
      tokens: [
        %{token_index: 0, norm: "i"},
        %{token_index: 1, norm: "sat"},
        %{token_index: 2, norm: "on"},
        %{token_index: 3, norm: "the"},
        %{token_index: 4, norm: "bank"},
        %{token_index: 5, norm: "beside"},
        %{token_index: 6, norm: "the"},
        %{token_index: 7, norm: "river"}
      ],
      lifg_choices: [
        %{token_index: 0, id: "i|pronoun|0", lemma: "i", score: 1.0},
        %{token_index: 2, id: "on|preposition|0", lemma: "on", score: 1.0},
        %{token_index: 3, id: "the|determiner|0", lemma: "the", score: 1.0},
        %{token_index: 6, id: "the|determiner|0", lemma: "the", score: 1.0}
      ]
    }

    summary = Summary.build(si)

    assert summary.unresolved == ["sat", "bank", "river"]
    assert "bank" in summary.uncertain
    assert "river" in summary.uncertain
    assert summary.degraded?
    assert :unresolved_content_tokens in summary.reasons
    assert summary.stats.unresolved_content == 3
  end

  test "attach writes summary and trace contract" do
    si =
      Summary.attach(%{
        sentence: "hello symbrella",
        tokens: [],
        trace: [],
        intent: :greet,
        keyword: "hello",
        confidence: 0.8,
        lifg_choices: [%{id: "hello|interjection|0", lemma: "hello", score: 0.9}]
      })

    assert %{comprehension: %{intent: :greet}} = si
    assert [%{stage: :comprehension, decision: :ok, reason: :semantic_summary} | _] = si.trace
  end
end
