defmodule Core.Comprehension.FunctionPosMassTest do
  @moduledoc """
  P-028: a closed-class/function word is judged by the probability mass of
  its chosen POS, not by one sense among many near-identical siblings.
  Content words keep the strict sense-level rule.
  """
  use ExUnit.Case, async: true

  alias Core.Comprehension.Summary

  # 11-candidate "hey" slate shaped like the live receipt (COLLAB §16.1):
  # tied interjection senses, small noun/proper_noun/verb tail.
  defp hey_probs do
    interj = for n <- 1..8, into: %{}, do: {"hey|interjection|#{n}", 0.107}
    Map.merge(interj, %{
      "hey|noun|1" => 0.048,
      "hey|proper_noun|1" => 0.048,
      "hey|verb|1" => 0.048
    })
  end

  defp what_probs do
    pron = for n <- 1..9, into: %{}, do: {"what|pronoun|#{n}", 0.1}
    Map.merge(pron, %{"what|determiner|1" => 0.06, "what|particle|1" => 0.04})
  end

  test "tied same-POS senses of a function word with a dominant POS are not uncertain" do
    si = %{
      sentence: "hey what",
      lifg_choices: [
        %{token_index: 0, id: "hey|interjection|1", lemma: "hey", pos: "interjection",
          score: 0.107, candidate_probs: hey_probs()},
        %{token_index: 1, id: "what|pronoun|1", lemma: "what", pos: "pronoun",
          score: 0.1, candidate_probs: what_probs()}
      ]
    }

    summary = Summary.build(si)

    refute "hey" in summary.uncertain
    refute "what" in summary.uncertain
    assert summary.stats.weak == 0
    # POS mass is a selection signal, never verified dictionary meaning.
    assert summary.understood == []
  end

  test "function POS mass is the sum over same-POS candidates" do
    choice = %{id: "hey|interjection|1", pos: "interjection", candidate_probs: hey_probs()}
    assert_in_delta Summary.function_pos_mass(choice), 0.856, 1.0e-6
  end

  test "a function word whose POS does not dominate stays uncertain" do
    # "that" is a Summary low-info term, so use a non-stop-word function word.
    probs = %{
      "under|preposition|1" => 0.2, "under|preposition|2" => 0.15,
      "under|adverb|1" => 0.3, "under|adjective|1" => 0.2, "under|noun|1" => 0.15
    }

    si = %{
      sentence: "under",
      lifg_choices: [
        %{token_index: 0, id: "under|preposition|1", lemma: "under", pos: "preposition",
          score: 0.2, candidate_probs: probs}
      ]
    }

    summary = Summary.build(si)
    assert "under" in summary.uncertain
    assert summary.stats.weak == 1
  end

  test "content words keep the strict sense-level rule even with a dominant POS" do
    si = %{
      sentence: "bank",
      lifg_choices: [
        %{token_index: 0, id: "bank|noun|11", lemma: "bank", pos: "noun", score: 0.5 - 1.0e-6,
          candidate_probs: %{"bank|noun|11" => 0.5 - 1.0e-6, "bank|noun|22" => 0.5 + 1.0e-6}}
      ]
    }

    summary = Summary.build(si)
    assert "bank" in summary.uncertain
  end

  test "without a posterior the original score rule applies unchanged" do
    si = %{
      sentence: "what",
      lifg_choices: [%{token_index: 0, id: "what|pronoun|1", lemma: "what", score: 0.12}]
    }

    assert "what" in Summary.build(si).uncertain
  end

  test "fallback and relational uncertainty still override POS mass" do
    si = %{
      sentence: "hey",
      lifg_choices: [
        %{token_index: 0, id: "hey|interjection|1", lemma: "hey", pos: "interjection",
          score: 0.107, candidate_probs: hey_probs(), relational_feedback_uncertain?: true}
      ]
    }

    assert "hey" in Summary.build(si).uncertain
  end
end
