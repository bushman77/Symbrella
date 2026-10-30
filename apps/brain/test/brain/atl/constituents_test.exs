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

  test "nominalized adjective candidate keeps adjective sense and never creates noun" do
    choices = [
      winner("the", "determiner", 0),
      winner("gathered", "adjective", 1),
      winner("had", "auxiliary", 2),
      winner("grown", "verb", 3)
    ]
    signature = %{
      id: :nominalized_adjective_subject_candidate,
      span: {0, 1},
      roles: %{determiner: 0, adjective: 1},
      required_features: %{
        determiner: [:pos_determiner], adjective: [:pos_adjective]
      }
    }

    slate = ATL.reduce(choices, tokens(~w(the gathered had grown)), [signature])
    assert [%{
      id: :nominalized_adjective_subject_candidate,
      status: :resolved,
      roles: %{
        determiner: %{sense_id: "the|determiner|1"},
        adjective: %{sense_id: "gathered|adjective|1"}
      }
    }] = slate.constituents
    assert slate.winners == ATL.reduce(choices, tokens(~w(the gathered had grown))).winners
    refute Map.has_key?(slate.by_id, "gathered|noun|1")

    wrong_pos = [
      winner("the", "determiner", 0),
      winner("gathered", "verb", 1)
    ]
    assert [%{status: :partially_resolved,
      unresolved_roles: [:adjective]}] =
      ATL.reduce(wrong_pos, tokens(~w(the gathered)), [signature]).constituents

    fallback = [%{id: "the|determiner|1", token_index: 0},
      %{id: "gathered|adjective|fallback", token_index: 1}]
    assert [%{status: :partially_resolved,
      unresolved_roles: [:adjective]}] =
      ATL.reduce(fallback, tokens(~w(the gathered)), [signature]).constituents
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

  test "possessive phrase keeps an ungrounded owner while resolving the noun head" do
    choices = [
      winner("railgun", "noun", 1),
      winner("development", "noun", 2)
    ]

    signature = %{
      id: :possessive_noun_phrase,
      span: {0, 2},
      roles: %{possessor: 0, modifier: 1, head: 2},
      required_features: %{
        possessor: [:pos_noun_or_pronoun],
        modifier: [:pos_noun],
        head: [:pos_noun]
      },
      evidence: [:apostrophe_s, :nominal_head_available],
      surface_possessor: "Japan"
    }

    slate = ATL.reduce(choices, tokens(["Japan's", "railgun", "development"]), [signature])

    assert [%{
      id: :possessive_noun_phrase,
      status: :partially_resolved,
      unresolved_roles: [:possessor],
      roles: %{
        modifier: %{sense_id: "railgun|noun|1"},
        head: %{sense_id: "development|noun|1"}
      }
    }] = slate.constituents

    assert slate.winners == ATL.reduce(choices, tokens(["Japan's", "railgun", "development"])).winners
    refute Map.has_key?(slate.by_norm, "japan's")
    refute Map.has_key?(slate.by_id, "japan's|proper_noun|1")
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

  test "COLONY-14 resolves modal conditional roles only from the actual selected senses" do
    words = ~w(what would you do if you were free)
    winners = [
      winner("what", "pronoun", 0),
      winner("would", "auxiliary", 1),
      winner("you", "pronoun", 2),
      winner("do", "verb", 3),
      winner("if", "conjunction", 4),
      winner("you", "pronoun", 5),
      winner("were", "verb", 6),
      winner("free", "adjective", 7)
    ]

    roles = %{
      wh: 0, modal: 1, subject: 2, predicate: 3,
      condition_marker: 4, condition_subject: 5,
      condition_copula: 6, condition_complement: 7
    }

    required_features = %{
      wh: [:pos_pronoun],
      modal: [:pos_modal_or_auxiliary_or_verb],
      subject: [:pos_pronoun], predicate: [:pos_verb],
      condition_marker: [:explicit_if_surface],
      condition_subject: [:pos_pronoun],
      condition_copula: [:attested_copular_form],
      condition_complement: [:pos_adjective]
    }

    signature = %{
      id: :wh_modal_conditional_question, span: {0, 7},
      roles: roles, required_features: required_features
    }

    slate = ATL.reduce(winners, tokens(words), [signature])

    assert [%{
      id: :wh_modal_conditional_question,
      status: :resolved, unresolved_roles: [],
      roles: resolved
    }] = slate.constituents

    assert resolved.modal.sense_id == "would|auxiliary|1"
    assert resolved.condition_marker.sense_id == "if|conjunction|1"
    assert resolved.condition_copula.sense_id == "were|verb|1"
    assert slate.winners == ATL.reduce(winners, tokens(words)).winners
  end

  test "COLONY-14 does not override incompatible modal or copular lexical choices" do
    words = ~w(what would you do if you were free)
    winners = [
      winner("what", "pronoun", 0),
      winner("would", "noun", 1),
      winner("you", "pronoun", 2),
      winner("do", "verb", 3),
      winner("if", "noun", 4),
      winner("you", "pronoun", 5),
      winner("were", "noun", 6),
      winner("free", "noun", 7)
    ]

    signature = %{
      id: :wh_modal_conditional_question, span: {0, 7},
      roles: %{
        wh: 0, modal: 1, subject: 2, predicate: 3,
        condition_marker: 4, condition_subject: 5,
        condition_copula: 6, condition_complement: 7
      },
      required_features: %{
        wh: [:pos_pronoun], modal: [:pos_modal_or_auxiliary_or_verb],
        subject: [:pos_pronoun], predicate: [:pos_verb],
        condition_marker: [:explicit_if_surface],
        condition_subject: [:pos_pronoun],
        condition_copula: [:attested_copular_form],
        condition_complement: [:pos_adjective]
      }
    }

    slate = ATL.reduce(winners, tokens(words), [signature])
    assert [%{status: :partially_resolved, unresolved_roles: missing}] =
      slate.constituents
    assert Enum.sort(missing) ==
      [:condition_complement, :condition_copula, :condition_marker, :modal]
    assert slate.winners == ATL.reduce(winners, tokens(words)).winners
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
