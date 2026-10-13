defmodule Core.Brain.ATLConstituentsTest do
  use ExUnit.Case, async: false

  alias Core.Brain.ATL

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
