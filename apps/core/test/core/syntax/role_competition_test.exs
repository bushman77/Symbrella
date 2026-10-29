defmodule Core.Syntax.RoleCompetitionTest do
  use ExUnit.Case, async: true

  alias Core.SemanticInput
  alias Core.Syntax.RoleCompetition

  test "attaches conservative role evidence without changing original lexical or syntactic evidence" do
    tokens = [
      %{phrase: "a", index: 0, n: 1},
      %{phrase: "light", index: 1, n: 1},
      %{phrase: "suitcase", index: 2, n: 1}
    ]

    cells = [
      %{norm: "a", pos: "determiner", id: "a|determiner|1"},
      %{norm: "light", pos: "adjective", id: "light|adjective|1"},
      %{norm: "light", pos: "noun", id: "light|noun|7"},
      %{norm: "suitcase", pos: "noun", id: "suitcase|noun|1"}
    ]

    signature = %{
      id: :noun_phrase,
      source: :core_syntax, span: {0, 2},
      roles: %{determiner: 0, modifier: 1, head: 2},
      evidence: [:nominal_constituent, :pos_pattern],
      required_features: %{
        determiner: [:pos_determiner], modifier: [:pos_adjective],
        head: [:pos_noun]
      }
    }

    si = %SemanticInput{
      sentence: "a light suitcase", source: :test,
      tokens: tokens, active_cells: cells,
      sentence_signatures: [signature],
      sense_candidates: %{1 => [
        %{id: "light|adjective|1", lemma: "light"},
        %{id: "light|noun|7", lemma: "light"}
      ]}
    }

    out = RoleCompetition.attach(si)

    assert %{pos: "adjective", token_index: 1, status: :supported} =
      out.role_competition.roles[1]
    assert out.tokens == tokens
    assert out.active_cells == cells
    assert out.sentence_signatures == [signature]
    assert out.sense_candidates == si.sense_candidates
    assert hd(out.trace).stage == :role_competition
    assert hd(out.trace).meta.supported_count == 1
    assert :ok = SemanticInput.validate_contract(out)

    assert {:ok, normalized} = SemanticInput.normalize_contract(out)
    assert normalized.role_competition.roles[1].pos == "adjective"
  end

  test "passes a real signature's unresolved predicate observation to LIFG without new senses" do
    alias Core.Syntax.Signatures
    alias Core.TokenFilters

    tokens = [
      %{phrase: "The", index: 0, n: 1},
      %{phrase: "fox", index: 1, n: 1},
      %{phrase: "is", index: 2, n: 1},
      %{phrase: "flarping", index: 3, n: 1},
      %{phrase: "on", index: 4, n: 1},
      %{phrase: "the", index: 5, n: 1},
      %{phrase: "fence", index: 6, n: 1}
    ]

    cells = [
      %{norm: "the", pos: "determiner", id: "the|determiner|1"},
      %{norm: "fox", pos: "noun", id: "fox|noun|1"},
      %{norm: "is", pos: "verb", id: "is|verb|1"},
      %{norm: "on", pos: "preposition", id: "on|preposition|1"},
      %{norm: "fence", pos: "noun", id: "fence|noun|1"}
    ]

    si = %SemanticInput{
      sentence: "The fox is flarping on the fence.",
      source: :test, tokens: tokens, active_cells: cells,
      sense_candidates: %{}
    }
    |> TokenFilters.rebuild_word_ngrams(3)
    |> Signatures.attach()

    assert Enum.any?(si.sentence_signatures,
      &(&1.id == :progressive_lexical_gap_candidate))

    out = RoleCompetition.attach(si)
    assert out.role_competition.roles == %{}
    assert %{token_index: 3, status: :unresolved, role: :predicate} =
      out.role_competition.unresolved_roles[3]
    assert out.sense_candidates == %{}
    assert out.active_cells == cells
    assert out.sentence_signatures == si.sentence_signatures
    assert hd(out.trace).decision == :unresolved
    assert hd(out.trace).meta.unresolved_grammar_count == 1
    assert hd(out.trace).meta.supported_count == 0
    assert :ok = SemanticInput.validate_contract(out)
  end

  test "abstains without original structural witnesses" do
    si = %SemanticInput{sentence: "unknown structure",
      tokens: [%{phrase: "unknown", index: 0, n: 1}],
      sentence_signatures: [], active_cells: []}

    out = RoleCompetition.attach(si)
    assert out.role_competition.roles == %{}
    assert hd(out.trace).decision == :abstained
    assert :ok = SemanticInput.validate_contract(out)
  end
end
