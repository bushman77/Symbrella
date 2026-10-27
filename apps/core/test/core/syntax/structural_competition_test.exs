defmodule Core.Syntax.StructuralCompetitionTest do
  use ExUnit.Case, async: true

  alias Core.Syntax.StructuralCompetition

  test "unwitnessed injected signatures cannot win" do
    si = %Core.SemanticInput{
      sentence: "The cat is sleeping under the table.",
      source: :test,
      tokens: ~w(The cat is sleeping under the table)
        |> Enum.with_index()
        |> Enum.map(fn {word, index} -> %{index: index, phrase: word, n: 1} end),
      sentence_signatures: [
        %{
          id: :progressive_locative_candidate,
          source: :core_syntax,
          span: {0, 6},
          roles: %{subject_head: 1, predicate: 3, preposition: 4},
          evidence: [:fabricated]
        }
      ]
    }

    out = StructuralCompetition.attach(si)

    assert out.structural_competition.status == :no_complete_candidate
    assert out.structural_competition.preferred == nil
    assert Enum.any?(out.trace, &(&1.stage == :structural_competition))
    assert out.sentence_signatures == si.sentence_signatures
  end
end
