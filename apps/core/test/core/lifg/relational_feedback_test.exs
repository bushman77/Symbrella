defmodule Core.LIFG.RelationalFeedbackTest do
  use ExUnit.Case, async: true

  alias Core.LIFG.RelationalFeedback
  alias Core.SemanticInput

  defp input do
    finance = "bank|noun|finance"
    river_bank = "bank|noun|river"

    tokens = [
      %{index: 0, phrase: "river", n: 1, mw: false},
      %{index: 1, phrase: "bank", n: 1, mw: false}
    ]

    choices = [
      %{token_index: 0, id: "river|noun|1", score: 1.0},
      %{token_index: 1, id: finance, pos: "noun", score: 0.5,
        candidate_probs: %{finance => 0.5, river_bank => 0.5}}
    ]

    signatures = [
      %{id: :noun_phrase, span: {0, 1},
        roles: %{modifier: 0, head: 1},
        required_features: %{modifier: [:pos_noun], head: [:pos_noun]}}
    ]

    %SemanticInput{
      sentence: "river bank",
      tokens: tokens,
      trace: [],
      source: :test,
      lifg_choices: choices,
      semantic_choices: choices,
      sentence_signatures: signatures,
      sense_candidates: %{
        1 => [
          %{id: finance, lemma: "bank", pos: "noun",
            definition: "A financial institution."},
          %{id: river_bank, lemma: "bank", pos: "noun",
            definition: "The side of a river."}
        ]
      },
      atl_slate: %{
        constituents: [
          %{id: :noun_phrase, span: {0, 1}, status: :resolved,
            roles: %{
              modifier: %{token_index: 0, sense_id: "river|noun|1"},
              head: %{token_index: 1, sense_id: finance}
            }}
        ]
      }
    }
  end

  test "revises both lexical views and reprojects ATL from the revised sense" do
    out = RelationalFeedback.attach(input())

    assert out.lifg_feedback.applied?
    assert [%{token_index: 1, from: "bank|noun|finance",
              to: "bank|noun|river"}] = out.lifg_feedback.applied_revisions

    revised_word = Enum.find(out.lifg_choices, &(&1.token_index == 1))
    assert revised_word.id == "bank|noun|river"
    assert revised_word.relational_feedback_uncertain?
    assert revised_word.margin < 0.15
    assert Enum.find(out.semantic_choices, &(&1.token_index == 1)).id ==
             "bank|noun|river"

    summary = Core.Comprehension.Summary.build(out)
    assert "bank" in summary.uncertain
    refute "bank" in summary.understood
    assert summary.degraded?

    assert Enum.any?(out.atl_slate.constituents, fn
      %{id: :noun_phrase, status: :resolved,
        roles: %{head: %{sense_id: "bank|noun|river"}}} -> true
      _ -> false
    end)

    assert :ok = SemanticInput.validate_contract(out)
    assert [%{stage: :lifg_relational_feedback, decision: :revised} | _] = out.trace
  end

  test "observe-only mode reports evidence without changing winners or ATL" do
    si = input()
    out = RelationalFeedback.attach(si, apply?: false)

    assert out.lifg_feedback.mode == :observe_only
    assert out.lifg_feedback.recommended_changes == 1
    refute out.lifg_feedback.applied?
    assert out.lifg_choices == si.lifg_choices
    assert out.semantic_choices == si.semantic_choices
    assert out.atl_slate == si.atl_slate
  end

  test "refuses to change an MWE-owned canonical role" do
    si = input()

    shadowed =
      update_in(si.semantic_choices, fn choices ->
        Enum.map(choices, fn
          %{token_index: 1} = choice -> Map.put(choice, :kind, :mwe)
          choice -> choice
        end)
      end)

    out = RelationalFeedback.attach(shadowed)
    refute out.lifg_feedback.applied?
    assert out.semantic_choices == shadowed.semantic_choices
    assert out.lifg_choices == shadowed.lifg_choices
  end
end
