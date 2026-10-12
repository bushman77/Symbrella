defmodule Brain.ATL.ConstituentsTest do
  use ExUnit.Case, async: true

  alias Brain.ATL

  defp winner(word, pos, index) do
    %{
      token_index: index,
      id: "#{word}|#{pos}|1",
      lemma: word,
      score: 0.8
    }
  end

  defp tokens(words) do
    words |> Enum.with_index() |> Enum.map(fn {w, i} -> %{index: i, phrase: w, n: 1} end)
  end

  test "an already-selected noun phrase is projected without changing winners" do
    choices = [
      winner("the", "determiner", 0),
      winner("quiet", "adjective", 1),
      winner("garden", "noun", 2)
    ]

    noun_phrase = %{
      id: :noun_phrase,
      span: {0, 2},
      roles: %{determiner: 0, modifier: 1, head: 2},
      required_features: %{
        determiner: [:pos_determiner],
        modifier: [:pos_adjective],
        head: [:pos_noun]
      }
    }

    legacy = ATL.reduce(choices, tokens(~w(the quiet garden)))
    with_grammar = ATL.reduce(choices, tokens(~w(the quiet garden)), [noun_phrase])

    assert with_grammar.by_norm == legacy.by_norm
    assert with_grammar.by_id == legacy.by_id
    assert with_grammar.winners == legacy.winners
    assert with_grammar.winner_count == 3
    assert with_grammar.constituent_count == 1

    assert [%{
      id: :noun_phrase,
      status: :resolved,
      unresolved_roles: [],
      roles: roles
    }] = with_grammar.constituents

    assert roles.head.sense_id == "garden|noun|1"
    assert roles.head.token_index == 2
    assert roles.modifier.sense_id == "quiet|adjective|1"
    assert roles.determiner.sense_id == "the|determiner|1"
  end

  test "nested noun and prepositional phrases retain shared head identity" do
    choices = [
      winner("in", "preposition", 0),
      winner("the", "determiner", 1),
      winner("quiet", "adjective", 2),
      winner("garden", "noun", 3)
    ]

    signatures = [
      %{
        id: :noun_phrase,
        span: {1, 3},
        roles: %{determiner: 1, modifier: 2, head: 3},
        required_features: %{determiner: [:pos_determiner], modifier: [:pos_adjective], head: [:pos_noun]}
      },
      %{
        id: :prepositional_phrase,
        span: {0, 3},
        roles: %{preposition: 0, determiner: 1, modifier: 2, head: 3, object: 3},
        required_features: %{
          preposition: [:pos_preposition],
          determiner: [:pos_determiner],
          modifier: [:pos_adjective],
          head: [:pos_noun],
          object: [:pos_noun]
        }
      }
    ]

    slate = ATL.reduce(choices, tokens(~w(in the quiet garden)), signatures)
    assert slate.constituent_count == 2

    [np, pp] = slate.constituents
    assert np.status == :resolved
    assert pp.status == :resolved
    assert np.roles.head.sense_id == pp.roles.object.sense_id
    assert pp.roles.preposition.sense_id == "in|preposition|1"
    assert hd(slate.by_norm["garden"]).id == "garden|noun|1"
  end

  test "adverb lexical sense resolves a grammatical particle without relabeling the sense ID" do
    choices = [
      winner("turn", "verb", 0),
      winner("the", "determiner", 1),
      winner("lights", "noun", 2),
      winner("off", "adverb", 3)
    ]

    signatures = [
      %{
        id: :verb_particle_construction, span: {0, 3},
        roles: %{verb: 0, determiner: 1, object: 2, particle: 3},
        required_features: %{
          verb: [:pos_verb], determiner: [:pos_determiner],
          object: [:pos_noun], particle: [:pos_adverb, :pos_preposition]
        }
      }
    ]

    slate = ATL.reduce(choices, tokens(~w(turn the lights off)), signatures)

    assert [%{
      status: :resolved,
      roles: %{particle: %{sense_id: "off|adverb|1", token_index: 3},
               object: %{sense_id: "lights|noun|1", token_index: 2}}
    }] = slate.constituents

    assert Map.has_key?(slate.by_id, "off|adverb|1")
    refute Map.has_key?(slate.by_id, "off|particle|1")
  end

  test "ambiguous or absent winners produce partial structure, never invented senses" do
    choices = [winner("the", "determiner", 0), winner("record", "verb", 1)]
    sig = %{
      id: :noun_phrase,
      span: {0, 1},
      roles: %{determiner: 0, head: 1},
      required_features: %{determiner: [:pos_determiner], head: [:pos_noun]}
    }

    slate = ATL.reduce(choices, tokens(~w(the record)), [sig])
    assert [%{
      status: :partially_resolved,
      unresolved_roles: [:head],
      roles: %{determiner: %{sense_id: "the|determiner|1"}}
    }] = slate.constituents

    refute Map.has_key?(hd(slate.constituents).roles, :head)
    assert Map.has_key?(slate.by_id, "record|verb|1")
  end

  test "canonical lexical MWE leaves shadowed particle unresolved" do
    choices = [
      %{
        token_index: 0, id: "turn off|verb|2", lemma: "turn off",
        token_span: {0, 1}, source: :mwe_lexicon, kind: :mwe
      },
      winner("the", "determiner", 2),
      winner("lights", "noun", 3)
    ]

    sig = %{
      id: :verb_particle_construction,
      span: {0, 1},
      roles: %{verb: 0, particle: 1},
      required_features: %{verb: [:pos_verb], particle: [:pos_particle]}
    }

    slate = ATL.reduce(choices, tokens(~w(turn off the lights)), [sig])

    assert slate.constituent_count == 1
    assert [%{
      status: :partially_resolved,
      unresolved_roles: [:particle],
      roles: %{verb: %{sense_id: "turn off|verb|2"}}
    }] = slate.constituents

    assert hd(slate.constituents).roles.verb.selection_scope == :lexical_mwe
    assert hd(slate.constituents).roles.verb.token_span == {0, 1}
    refute Map.has_key?(slate.by_norm, "turn")
    refute Map.has_key?(slate.by_norm, "off")
    assert Map.has_key?(slate.by_id, "turn off|verb|2")
  end

  test "fallback candidates and invalid spans cannot become resolved constituents" do
    choices = [%{id: "garden|noun|fallback", token_index: 1}]
    signatures = [
      %{
        id: :noun_phrase, span: {0, 1},
        roles: %{head: 1}, required_features: %{head: [:pos_noun]}
      },
      %{id: :noun_phrase, span: {0, 1}, roles: %{head: 99}},
      %{id: :unknown_signature, span: {0, 1}, roles: %{head: 1}}
    ]

    slate = ATL.reduce(choices, tokens(~w(the garden)), signatures)
    assert slate.constituent_count == 1

    assert [%{
      status: :unresolved, roles: %{}, unresolved_roles: [:head]
    }] = slate.constituents
  end

  test "legacy two-argument reduction and map reducer remain compatible" do
    choices = [winner("home", "noun", 0)]
    words = tokens(["home"])
    legacy = ATL.reduce(choices, words)
    assert legacy == ATL.reduce(choices, words, [])
    refute Map.has_key?(legacy, :constituents)
    assert legacy == ATL.reduce(%{lifg_choices: choices, tokens: words})
  end
end
