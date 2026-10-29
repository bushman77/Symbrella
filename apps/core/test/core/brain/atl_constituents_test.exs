defmodule Core.Brain.ATLConstituentsTest do
  use ExUnit.Case, async: false

  alias Core.Brain.ATL

  test "syntax to LIFG to ATL conveys unresolved predicate without creating a semantic winner" do
    alias Core.SemanticInput
    alias Core.Syntax.{RoleCompetition, Signatures}
    alias Core.TokenFilters

    sentence = "The fox is flarping on the fence."
    cells = [
      %{id: "the|determiner|1", norm: "the", pos: "determiner"},
      %{id: "fox|noun|1", norm: "fox", pos: "noun"},
      %{id: "is|verb|1", norm: "is", pos: "verb"},
      %{id: "on|preposition|1", norm: "on", pos: "preposition"},
      %{id: "fence|noun|1", norm: "fence", pos: "noun"}
    ]
    choices = [
      %{token_index: 1, id: "fox|noun|1", lemma: "fox", score: 0.8},
      %{token_index: 4, id: "on|preposition|1", lemma: "on", score: 0.8}
    ]

    si =
      %SemanticInput{
        source: :test,
        sentence: sentence,
        active_cells: cells,
        lifg_choices: choices
      }
      |> TokenFilters.rebuild_word_ngrams(3)
      |> Signatures.attach()
      |> RoleCompetition.attach()

    assert %{status: :unresolved, role: :predicate} =
      si.role_competition.unresolved_roles[3]

    out = ATL.ingest(si, [])
    assert [%{
      role: :predicate, status: :unresolved,
      exact_dictionary_sense_verified?: false,
      event_occurrence_verified?: false,
      factual_authority?: false
    }] = out.atl_slate.unresolved_grammar
    assert out.atl_slate.unresolved_grammar_count == 1

    reduced = Brain.ATL.reduce(choices, si.tokens, si.sentence_signatures)
    assert out.atl_slate.winners == reduced.winners
    assert out.atl_slate.by_id == reduced.by_id
    assert out.atl_slate.by_norm == reduced.by_norm
    assert Map.get(out.atl_slate, :constituents, []) ==
      Map.get(reduced, :constituents, [])
    refute Map.has_key?(out.atl_slate.by_id, "flarping|verb|1")
    refute Enum.any?(Map.get(out.atl_slate, :constituents, []),
      &(&1.id == :progressive_lexical_gap_candidate))
    assert out.lifg_choices == si.lifg_choices
    assert out.role_competition == si.role_competition
    assert out.sentence_signatures == si.sentence_signatures
    assert hd(out.trace).unresolved_grammar_count == 1
    assert :ok == SemanticInput.validate_contract(out)
  end

  test "Core ATL receives resolved grammar while preserving the original semantic input" do
    si =
      %Core.SemanticInput{
        sentence: "in the garden",
        tokens: [
          %{index: 0, n: 1, phrase: "in"},
          %{index: 1, n: 1, phrase: "the"},
          %{index: 2, n: 1, phrase: "garden"}
        ],
        lifg_choices: [
          %{token_index: 0, id: "in|preposition|1", lemma: "in", score: 0.8},
          %{token_index: 1, id: "the|determiner|1", lemma: "the", score: 0.8},
          %{token_index: 2, id: "garden|noun|1", lemma: "garden", score: 0.8}
        ],
        sentence_signatures: [
          %{
            id: :prepositional_phrase,
            span: {0, 2},
            roles: %{preposition: 0, determiner: 1, head: 2, object: 2},
            required_features: %{
              preposition: [:pos_preposition],
              determiner: [:pos_determiner],
              head: [:pos_noun],
              object: [:pos_noun]
            }
          }
        ]
      }

    result = ATL.ingest(si, [])

    assert %Core.SemanticInput{} = result
    assert result.tokens == si.tokens
    assert result.lifg_choices == si.lifg_choices
    assert result.sentence_signatures == si.sentence_signatures
    assert result.atl_slate.winner_count == 3
    assert result.atl_slate.constituent_count == 1

    assert [%{
      id: :prepositional_phrase,
      status: :resolved,
      roles: %{preposition: %{sense_id: "in|preposition|1"},
               object: %{sense_id: "garden|noun|1"}}
    }] = result.atl_slate.constituents

    assert Map.has_key?(result.atl_slate.by_norm, "garden")
    assert Map.has_key?(result.atl_slate.by_id, "garden|noun|1")
  end

  test "Core ATL uses the canonical MWE choice view and does not invent child senses" do
    si =
      %Core.SemanticInput{
        sentence: "turn off",
        tokens: [
          %{index: 0, n: 1, phrase: "turn"},
          %{index: 1, n: 1, phrase: "off"}
        ],
        lifg_choices: [
          %{token_index: 0, id: "turn|verb|1"},
          %{token_index: 1, id: "off|particle|1"}
        ],
        semantic_choices: [
          %{
            token_index: 0, id: "turn off|verb|2", lemma: "turn off",
            kind: :mwe, source: :mwe_lexicon, token_span: {0, 1}
          }
        ],
        sentence_signatures: [
          %{
            id: :verb_particle_construction,
            span: {0, 1},
            roles: %{verb: 0, particle: 1},
            required_features: %{verb: [:pos_verb], particle: [:pos_particle]}
          }
        ]
      }

    result = ATL.ingest(si, [])
    assert result.atl_slate.winner_count == 1
    assert Map.has_key?(result.atl_slate.by_id, "turn off|verb|2")
    refute Map.has_key?(result.atl_slate.by_id, "off|particle|1")
    assert [%{unresolved_roles: [:particle], status: :partially_resolved}] =
             result.atl_slate.constituents
  end
end
