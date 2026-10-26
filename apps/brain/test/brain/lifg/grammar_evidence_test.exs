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

  test "complete bounded past transitive boosts only an existing verb candidate" do
    sig = %{
      id: :bounded_regular_past_transitive,
      source: :core_syntax,
      span: {0, 4},
      roles: %{
        subject_determiner: 0, subject_head: 1, predicate: 2,
        object_determiner: 3, object_head: 4
      },
      evidence: [
        :contiguous_surface, :attested_nominal_subject,
        :attested_dictionary_verb, :bounded_past_surface,
        :attested_nominal_object
      ]
    }

    cells = [
      %{id: "verified|noun|1", norm: "verified", pos: "noun"},
      %{id: "verified|verb|3", norm: "verified", pos: "verb"}
    ]

    scores = %{"verified|noun|1" => 0.42, "verified|verb|3" => 0.20}
    ctx = GE.prepare(%{active_cells: cells, sentence_signatures: [sig]})
    result = GE.apply_bounded_past_transitive(scores, ctx, 2)

    assert result["verified|verb|3"] > result["verified|noun|1"]
    assert result["verified|noun|1"] == 0.42
    assert Map.keys(result) |> Enum.sort() == Map.keys(scores) |> Enum.sort()
    assert GE.apply_bounded_past_transitive(scores, ctx, 3) == scores

    incomplete = %{sig | evidence: List.delete(sig.evidence, :attested_nominal_object)}
    incomplete_ctx = GE.prepare(%{active_cells: cells, sentence_signatures: [incomplete]})
    assert GE.apply_bounded_past_transitive(scores, incomplete_ctx, 2) == scores

    missing_verb = GE.prepare(%{active_cells: [hd(cells)], sentence_signatures: [sig]})
    assert GE.apply_bounded_past_transitive(scores, missing_verb, 2) == scores
  end

  test "complete copular relative clause boosts only an attested noun complement" do
    sig = %{
      id: :copular_relative_classification,
      source: :core_syntax,
      span: {0, 6},
      roles: %{
        subject_head: 0, copula: 1, complement_head: 2,
        relative_marker: 3, relative_verb: 4,
        object_determiner: 5, object_head: 6
      },
      evidence: [
        :contiguous_surface, :attested_subject_noun,
        :attested_copular_be, :attested_complement_noun,
        :explicit_relative_clause, :attested_relative_verb,
        :attested_object_noun
      ]
    }

    ctx =
      GE.prepare(%{
        active_cells: [
          %{id: "lines|noun|1", pos: "noun"},
          %{id: "lines|proper_noun|1", pos: "proper_noun"}
        ],
        sentence_signatures: [sig]
      })

    scores = %{"lines|noun|1" => 0.20, "lines|proper_noun|1" => 0.35}
    boosted = GE.apply_copular_relative(scores, ctx, 2)

    assert boosted["lines|noun|1"] > boosted["lines|proper_noun|1"]
    assert boosted["lines|proper_noun|1"] == 0.35
    assert Map.keys(boosted) |> Enum.sort() == Map.keys(scores) |> Enum.sort()
    assert GE.apply_copular_relative(scores, ctx, 0) == scores

    missing_evidence =
      %{sig | evidence: List.delete(sig.evidence, :attested_relative_verb)}
    assert GE.apply_copular_relative(scores, GE.prepare(%{
      active_cells: [%{id: "lines|noun|1", pos: "noun"}],
      sentence_signatures: [missing_evidence]
    }), 2) == scores

    assert GE.apply_copular_relative(scores, GE.prepare(%{
      active_cells: [%{id: "lines|proper_noun|1", pos: "proper_noun"}],
      sentence_signatures: [sig]
    }), 2) == scores
  end

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

  test "complete WH auxiliary question predicate outranks an overlapping noun-phrase hypothesis" do
    sentence_frame = %{
      id: :wh_aux_topic_question,
      span: {0, 7},
      roles: %{wh: 0, auxiliary: 1, subject: 2, predicate: 3, preposition: 4},
      required_features: %{predicate: [:pos_verb]},
      evidence: [
        :wh_fronted, :auxiliary_inversion, :lexical_verb_available,
        :explicit_prepositional_topic, :question_punctuation
      ]
    }

    # A short "you know" noun phrase is plausible in isolation, but does not
    # outrank the complete "what do you know about X?" question.
    overlapping_noun = %{
      id: :noun_phrase,
      span: {2, 3},
      roles: %{modifier: 2, head: 3},
      required_features: %{modifier: [:pos_noun], head: [:pos_noun]}
    }

    ctx = context([overlapping_noun, sentence_frame])
    scores = %{
      "know|noun|1" => 0.56,
      "know|verb|1" => 0.48,
      "know|verb|2" => 0.47
    }

    assert GE.apply(scores, ctx, 3, 0.0) == scores

    result = GE.apply(scores, ctx, 3, 1.0)
    assert result["know|verb|1"] > result["know|noun|1"]
    assert result["know|verb|2"] > result["know|noun|1"]
    assert Map.keys(result) |> Enum.sort() == Map.keys(scores) |> Enum.sort()

    # The question is only a POS constraint, not evidence that any particular
    # verb sense is correct. Preserve the relative ordering of verb senses.
    assert_in_delta result["know|verb|1"] - result["know|verb|2"], 0.01, 1.0e-8
    assert GE.apply(scores, ctx, 99, 1.0) == scores
  end

  test "incomplete question frames cannot inflate predicate senses" do
    noun = %{
      id: :noun_phrase, span: {2, 3}, roles: %{head: 3},
      required_features: %{head: [:pos_noun]}
    }

    incomplete = %{
      id: :wh_aux_topic_question, span: {0, 7},
      roles: %{wh: 0, auxiliary: 1, subject: 2, predicate: 3, preposition: 4},
      required_features: %{predicate: [:pos_verb]},
      evidence: [:wh_fronted, :lexical_verb_available]
    }

    scores = %{"know|noun|1" => 0.5, "know|verb|1" => 0.5}
    output = GE.apply(scores, context([noun, incomplete]), 3, 1.0)
    assert output["know|noun|1"] > output["know|verb|1"]

    fallback = %{"know|verb|fallback" => 0.5}
    frame = %{incomplete | evidence: [
      :wh_fronted, :auxiliary_inversion, :lexical_verb_available,
      :explicit_prepositional_topic, :question_punctuation
    ]}
    assert GE.apply(fallback, context([frame]), 3, 1.0) == fallback
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

  test "lexical adverb particle beats competing weak noun phrase evidence" do
    signatures = [
      %{
        id: :noun_phrase, span: {2, 3},
        roles: %{modifier: 2, head: 3},
        required_features: %{modifier: [:pos_adjective], head: [:pos_noun]}
      },
      %{
        id: :verb_particle_construction, span: {0, 3},
        roles: %{verb: 0, determiner: 1, object: 2, particle: 3},
        required_features: %{
          verb: [:pos_verb], determiner: [:pos_determiner],
          object: [:pos_noun], particle: [:pos_adverb, :pos_preposition]
        }
      }
    ]

    ctx = context(signatures)
    candidates = %{
      "off|adverb|1" => 0.5,
      "off|preposition|1" => 0.5,
      "off|noun|1" => 0.5
    }

    result = GE.apply(candidates, ctx, 3, 1.0)
    assert result["off|adverb|1"] > result["off|noun|1"]
    assert result["off|preposition|1"] > result["off|noun|1"]
    assert Map.keys(result) |> Enum.sort() == Map.keys(candidates) |> Enum.sort()
    assert GE.apply(candidates, ctx, 3, 0.0) == candidates
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
  test "verified finite-present clause prefers the attested verb, not a plural noun" do
    evidence = ["tag:form-of", "tag:indicative", "tag:present",
                "tag:singular", "tag:third-person"]
    cells = [
      %{id: "draws|noun|1", semantic_atoms: ["tag:form-of", "tag:plural"]},
      %{id: "draws|noun|2", semantic_atoms: ["tag:plural", "tag:plural-only"]},
      %{id: "draws|verb|1", semantic_atoms: evidence}
    ]

    signature = %{
      id: :finite_present_svo_candidate,
      roles: %{subject_head: 1, predicate: 2, object_head: 4},
      evidence: [:contiguous_surface, :attested_nominal_subject,
                 :attested_nominal_object, :attested_finite_present_verb]
    }

    scores = %{"draws|noun|1" => 0.57, "draws|noun|2" => 0.56,
               "draws|verb|1" => 0.40}
    context = GE.prepare(%{active_cells: cells, sentence_signatures: [signature]})
    updated = GE.apply_finite_present(scores, context, 2)
    assert updated["draws|verb|1"] > updated["draws|noun|1"]
    assert updated["draws|noun|1"] == scores["draws|noun|1"]
    assert updated["draws|noun|2"] == scores["draws|noun|2"]
    assert GE.apply_finite_present(scores, context, 1) == scores
    assert GE.apply_finite_present(scores, GE.prepare(%{active_cells: cells}), 2) == scores

    no_attested_verb = GE.prepare(%{active_cells: Enum.drop(cells, -1),
                                   sentence_signatures: [signature]})
    assert GE.apply_finite_present(scores, no_attested_verb, 2) == scores

    incomplete = put_in(signature, [:evidence], [:attested_finite_present_verb])
    assert GE.apply_finite_present(scores,
             GE.prepare(%{active_cells: cells, sentence_signatures: [incomplete]}), 2) == scores
  end


  test "POS grammar evidence does not saturate away distinctions between noun senses" do
    signature = %{
      id: :noun_phrase,
      roles: %{head: 2},
      required_features: %{head: [:pos_noun]}
    }

    financial = "bank|noun|1"
    gambling = "bank|noun|5"
    verb = "bank|verb|1"

    initial = %{financial => 0.933, gambling => 0.861, verb => 0.83}
    ctx = context([signature])
    boosted = GE.apply(initial, ctx, 2, 1.0)

    assert boosted[financial] > 1.0
    assert boosted[gambling] > 1.0
    assert boosted[financial] > boosted[gambling]
    assert_in_delta boosted[financial] - boosted[gambling], 0.072, 1.0e-8
    assert boosted[verb] == initial[verb]
  end

end
