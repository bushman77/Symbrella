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
