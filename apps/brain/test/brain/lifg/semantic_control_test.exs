defmodule Brain.LIFG.SemanticControlTest do
  use ExUnit.Case, async: true

  alias Brain.LIFG.SemanticControl
  alias Brain.LIFG.Stage1

  @river "bank|noun|river"
  @finance "bank|noun|finance"

  test "raises only the independently context-supported sense in a same-POS competition" do
    original = %{@river => 0.4, @finance => 0.4}
    rows = [
      %{id: @river, context_evidence: 0.29},
      %{id: @finance, context_evidence: 0.0}
    ]

    {scores, report} = SemanticControl.apply(original, rows)

    assert report.applied?
    assert report.reason == :contextual_sense_discrimination
    assert [%{id: @river, source: :original_sentence_dictionary_context}] =
             report.adjustments
    assert Enum.sort(Map.keys(scores)) == Enum.sort(Map.keys(original))
    assert scores[@river] > original[@river]
    assert scores[@river] <= original[@river] + 0.1
    assert scores[@finance] == original[@finance]
  end

  test "keeps uniform or weak semantic evidence unchanged" do
    scores = %{@river => 0.4, @finance => 0.4}

    for rows <- [
          [
            %{id: @river, context_evidence: 0.0},
            %{id: @finance, context_evidence: 0.0}
          ],
          [
            %{id: @river, context_evidence: 0.29},
            %{id: @finance, context_evidence: 0.27}
          ],
          [
            %{id: @river, context_evidence: 0.12},
            %{id: @finance, context_evidence: 0.0}
          ]
        ] do
      assert {^scores, %{applied?: false,
                         reason: :insufficient_discriminating_evidence}} =
               SemanticControl.apply(scores, rows)
    end
  end

  test "does not compare different POS classes or synthesize dictionary candidates" do
    scores = %{"bank|noun|1" => 0.5, "bank|verb|1" => 0.5}
    rows = [
      %{id: "bank|noun|1", context_evidence: 0.7},
      %{id: "bank|verb|1", context_evidence: 0.0}
    ]

    assert {^scores, %{applied?: false}} = SemanticControl.apply(scores, rows)
  end

  test "disabled control is exactly score-preserving" do
    scores = %{@river => 0.3, @finance => 0.3}
    rows = [
      %{id: @river, context_evidence: 0.5},
      %{id: @finance, context_evidence: 0.0}
    ]

    assert {^scores, %{applied?: false, reason: :disabled}} =
             SemanticControl.apply(scores, rows, enabled?: false)
  end

  test "Stage1 does not mistake a mixed particle/verb slate for a closed-class winner" do
    si = %{
      sentence: "What do you know about the river?",
      tokens: [
        %{index: 0, phrase: "know", n: 1, mw: false, span: {12, 16}}
      ],
      sense_candidates: %{
        0 => [
          %{id: "know|verb|11", lemma: "know", pos: "verb",
            definition: "To have information about a river.",
            activation: 0.5,
            features: %{lex_fit: 0.8, rel_prior: 0.5, activation: 0.5}},
          %{id: "know|verb|22", lemma: "know", pos: "verb",
            definition: "To recognize someone by sight.",
            activation: 0.5,
            features: %{lex_fit: 0.8, rel_prior: 0.5, activation: 0.5}},
          %{id: "know|particle|1", lemma: "know", pos: "particle",
            definition: "A discourse particle.",
            activation: 0.0,
            features: %{lex_fit: 0.0, rel_prior: 0.0, activation: 0.0}}
        ]
      }
    }

    assert {:ok, %{choices: [choice]}} =
             Stage1.run(si, scores: :all, preserve_sense_candidates: true,
               semantic_control?: true)

    assert choice.chosen_id == "know|verb|11"
    assert choice.semantic_control.applied?
    assert choice.semantic_control.reason == :contextual_sense_discrimination
    assert [%{id: "know|verb|11"}] = choice.semantic_control.adjustments
    assert choice.scores["know|verb|11"] > choice.scores["know|verb|22"]
    assert choice.low_confidence?

    assert {:ok, %{choices: [baseline]}} =
             Stage1.run(si, scores: :all, preserve_sense_candidates: true,
               semantic_control?: false)

    assert baseline.semantic_control.reason == :disabled
    assert baseline.semantic_control.adjustments == []
  end

  test "focus POS never boosts a competing grammatical class" do
    scores = %{
      "do|verb|1" => 0.4, "do|verb|2" => 0.4,
      "do|auxiliary|1" => 0.6
    }

    rows = [
      %{id: "do|verb|1", context_evidence: 0.8},
      %{id: "do|verb|2", context_evidence: 0.0},
      %{id: "do|auxiliary|1", context_evidence: 0.0}
    ]

    assert {^scores, %{applied?: false}} =
             SemanticControl.apply(scores, rows, focus_pos: "auxiliary")
  end

  test "Stage1 uses actual sentence dictionary evidence while retaining weak confidence" do
    si = %{
      sentence: "I stood on the bank beside the river.",
      tokens: [
        %{index: 0, phrase: "bank", n: 1, mw: false, span: {15, 19}}
      ],
      sense_candidates: %{
        0 => [
          %{id: @river, lemma: "bank", pos: "noun",
            definition: "The edge of a river.", activation: 0.5},
          %{id: @finance, lemma: "bank", pos: "noun",
            definition: "A financial institution.", activation: 0.5}
        ]
      }
    }

    assert {:ok, %{choices: [choice]}} =
             Stage1.run(si, scores: :all, preserve_sense_candidates: true,
               semantic_control?: true)

    assert choice.semantic_control.applied?
    assert [%{id: @river, evidence_gap: gap}] =
             choice.semantic_control.adjustments
    assert gap >= 0.15
    assert choice.scores[@river] > choice.scores[@finance]
    assert choice.low_confidence?
    assert Enum.sort(Map.keys(choice.scores)) == Enum.sort([@finance, @river])
  end
end
