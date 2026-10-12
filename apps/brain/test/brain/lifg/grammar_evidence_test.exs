defmodule Brain.LIFG.GrammarEvidenceTest do
  use ExUnit.Case, async: true

  alias Brain.LIFG.GrammarEvidence, as: GE

  @cells [
    %{id: "got|verb|1", semantic_atoms: ["tag:no-past-participle"],
      definition: "Expressing obligation; used with have."},
    %{id: "got|verb|4", semantic_atoms: ["tag:form-of", "tag:past"]},
    %{id: "got|verb|5", semantic_atoms: ["tag:form-of", "tag:past", "tag:participle"]}
  ]
  @scores %{"got|verb|1" => 0.5, "got|verb|4" => 0.5, "got|verb|5" => 0.5}

  defp context(signatures), do: GE.prepare(%{active_cells: @cells, sentence_signatures: signatures})
  defp sign(id, roles), do: %{id: id, roles: roles}

  test "weight zero preserves exact scores" do
    ctx = context([sign(:finite_past_candidate, %{verb: 1})])
    assert GE.apply(@scores, ctx, 1, 0.0) == @scores
  end

  test "finite past favors the simple-past sense, not obligation" do
    ctx = context([sign(:finite_past_candidate, %{verb: 1})])
    result = GE.apply(@scores, ctx, 1, 1.0)
    assert result["got|verb|4"] > result["got|verb|1"]
    assert result["got|verb|5"] == 0.5
  end

  test "perfect favors the past participle" do
    ctx = context([sign(:perfect_predicate, %{auxiliary: 0, verb: 1})])
    result = GE.apply(@scores, ctx, 1, 1.0)
    assert result["got|verb|5"] > result["got|verb|1"]
    assert result["got|verb|4"] == 0.5
  end

  test "larger obligation-compatible frame outweighs the perfect hypothesis" do
    ctx = context([
      sign(:perfect_predicate, %{auxiliary: 0, verb: 1}),
      sign(:have_verb_to_infinitive, %{auxiliary: 0, verb: 1, infinitive_marker: 2, complement: 3})
    ])
    result = GE.apply(@scores, ctx, 1, 1.0)
    assert result["got|verb|1"] > result["got|verb|5"]
  end

  test "infinitive complement supports a verb, without deleting the noun" do
    ctx = context([sign(:have_verb_to_infinitive, %{verb: 1, infinitive_marker: 2, complement: 3})])
    scores = %{"leave|noun|4" => 0.5, "leave|verb|1" => 0.5}
    result = GE.apply(scores, ctx, 3, 1.0)
    assert result["leave|verb|1"] > result["leave|noun|4"]
    assert Map.keys(result) |> Enum.sort() == Map.keys(scores) |> Enum.sort()
  end

  test "infinitive roles beat misleading noun and preposition priors" do
    ctx =
      context([
        sign(:have_verb_to_infinitive,
          %{auxiliary: 0, verb: 1, infinitive_marker: 2, complement: 3})
      ])

    complement = GE.apply(%{"leave|noun|4" => 0.65, "leave|verb|1" => 0.25}, ctx, 3, 1.0)
    assert complement["leave|verb|1"] > complement["leave|noun|4"]

    marker = GE.apply(%{"to|preposition|1" => 0.68, "to|particle|1" => 0.25}, ctx, 2, 1.0)
    assert marker["to|particle|1"] > marker["to|preposition|1"]
  end

  test "noun phrase roles support compatible existing POS senses without removing alternatives" do
    signature = %{
      id: :noun_phrase,
      span: {0, 2},
      roles: %{determiner: 0, modifier: 1, head: 2},
      required_features: %{
        determiner: [:pos_determiner],
        modifier: [:pos_adjective],
        head: [:pos_noun]
      }
    }
    ctx = context([signature])
    scores = %{"garden|noun|1" => 0.5, "garden|verb|1" => 0.5}

    updated = GE.apply(scores, ctx, 2, 1.0)
    assert_in_delta updated["garden|noun|1"], 0.68, 1.0e-8
    assert updated["garden|verb|1"] == 0.5
    assert Map.keys(updated) |> Enum.sort() == Map.keys(scores) |> Enum.sort()
    assert GE.apply(scores, ctx, 2, 0.0) == scores
    assert GE.apply(scores, ctx, 99, 1.0) == scores
  end

  test "prepositional constituents favor compatible preposition and nominal senses" do
    signature = %{
      id: :prepositional_phrase,
      span: {0, 2},
      roles: %{preposition: 0, determiner: 1, object: 2, head: 2},
      required_features: %{
        preposition: [:pos_preposition],
        determiner: [:pos_determiner],
        object: [:pos_noun],
        head: [:pos_noun]
      }
    }
    ctx = context([signature])

    preposition = GE.apply(%{"in|preposition|1" => 0.5, "in|adverb|1" => 0.5}, ctx, 0, 1.0)
    assert preposition["in|preposition|1"] > preposition["in|adverb|1"]

    head = GE.apply(%{"house|noun|1" => 0.5, "house|verb|1" => 0.5}, ctx, 2, 1.0)
    assert head["house|noun|1"] > head["house|verb|1"]
  end

  test "separated particle roles favor particle at its original index" do
    signature = %{
      id: :verb_particle_construction,
      span: {0, 3},
      roles: %{verb: 0, determiner: 1, object: 2, particle: 3},
      required_features: %{
        verb: [:pos_verb],
        determiner: [:pos_determiner],
        object: [:pos_noun_or_pronoun],
        particle: [:pos_particle]
      }
    }

    ctx = context([signature])
    scores = %{"off|preposition|1" => 0.5, "off|particle|1" => 0.5}
    result = GE.apply(scores, ctx, 3, 1.0)
    assert result["off|particle|1"] > result["off|preposition|1"]
    assert GE.apply(scores, ctx, 1, 1.0) == scores
  end

  test "structural grammar has bounded weighting and will not boost a synthetic fallback" do
    signature = %{
      id: :noun_phrase,
      roles: %{head: 1},
      required_features: %{head: [:pos_noun]}
    }

    ctx = context([signature])
    scores = %{"garden|noun|1" => 0.5, "garden|noun|fallback" => 0.5}
    result = GE.apply(scores, ctx, 1, 0.25)

    assert_in_delta result["garden|noun|1"], 0.545, 1.0e-8
    assert result["garden|noun|fallback"] == 0.5
    assert GE.apply(scores, ctx, 1, 0.0) == scores
  end

  test "POS construction cannot fabricate missing or incompatible lexical candidates" do
    ctx = context([
      %{id: :noun_phrase, span: {0, 1}, roles: %{head: 1},
        required_features: %{head: [:pos_noun]}}
    ])

    assert GE.apply(%{}, ctx, 1, 1.0) == %{}
    assert GE.apply(%{"garden|verb|1" => 0.4}, ctx, 1, 1.0) ==
             %{"garden|verb|1" => 0.4}
  end

  test "unrelated token index is unchanged" do
    ctx = context([sign(:finite_past_candidate, %{verb: 1})])
    assert GE.apply(@scores, ctx, 99, 1.0) == @scores
  end

  test "broad have-verb-to pattern alone is not treated as obligation" do
    ctx = GE.prepare(%{
      active_cells: [%{id: "tried|verb|1", semantic_atoms: ["tag:past", "tag:participle"], definition: "Past form of try."}],
      sentence_signatures: [sign(:have_verb_to_infinitive, %{verb: 1, complement: 3})]
    })
    assert GE.apply(%{"tried|verb|1" => 0.5}, ctx, 1, 1.0) == %{"tried|verb|1" => 0.5}
  end
end
