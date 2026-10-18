defmodule Core.Brain.SemanticSenseValidatorTest do
  use ExUnit.Case, async: false

  alias Core.Brain.SemanticSenseValidator
  alias Core.Semantic.{EpisodicProjection, SentenceMeaning}

  @svo_evidence [:contiguous_surface, :attested_nominal_subject,
                 :attested_nominal_object, :attested_finite_present_verb]
  @verb_tags ["tag:form-of", "tag:indicative", "tag:present",
              "tag:singular", "tag:third-person"]

  defp statement(surface, base, actor, patient) do
    actor_id = "#{actor}|noun|1"
    patient_id = "#{patient}|noun|1"
    verb_id = "#{surface}|verb|1"

    choices = [
      %{token_index: 1, id: actor_id, lemma: actor},
      %{token_index: 2, id: verb_id, lemma: surface},
      %{token_index: 4, id: patient_id, lemma: patient}
    ]

    %{
      source: :user,
      sentence: "The #{actor} #{surface} a #{patient}.",
      session_id: "semantic-sense-validator-fixture",
      tokens: [
        %{index: 0, phrase: "The", n: 1},
        %{index: 1, phrase: actor, n: 1},
        %{index: 2, phrase: surface, n: 1},
        %{index: 3, phrase: "a", n: 1},
        %{index: 4, phrase: patient <> ".", n: 1}
      ],
      sentence_signatures: [
        %{id: :finite_present_svo_candidate, source: :core_syntax,
          span: {0, 4}, evidence: @svo_evidence,
          roles: %{subject_determiner: 0, subject_head: 1,
                   predicate: 2, object_determiner: 3, object_head: 4}}
      ],
      active_cells: [
        %{id: actor_id, norm: actor, pos: "noun"},
        %{id: verb_id, norm: surface, pos: "verb",
          semantic_atoms: @verb_tags,
          definition: "An inflected form of #{base}."},
        %{id: patient_id, norm: patient, pos: "noun"}
      ],
      lifg_choices: choices,
      semantic_choices: choices,
      atl_slate: %{winners: choices},
      trace: []
    }
    |> EpisodicProjection.attach()
    |> SentenceMeaning.attach()
  end

  defp sense(base, n, gloss, grammar \\ ["transitive"], atoms \\ []) do
    %{id: "#{base}|verb|#{n}", norm: base, pos: "verb",
      definition: gloss, gram_function: grammar,
      semantic_atoms: atoms, synonyms: [], antonyms: []}
  end

  defp throw_senses do
    [
      sense("throw", 1,
        "To hurl; to release an object with force so that it moves through the air."),
      sense("throw", 5,
        "(transitive) To deliver (the ball) illegally by straightening the bowling arm during delivery."),
      sense("throw", 6,
        "To send an error to an exception-handling mechanism.")
    ]
  end

  defp draw_senses do
    [
      sense("draw", 1, "To pull a heavy rope through a door."),
      sense("draw", 2, "To produce a picture by tracing lines."),
      sense("draw", 3, "To take water from a well.")
    ]
  end

  test "a misleading ball-only lexical hit cannot validate the specialized bowling sense" do
    si = statement("throws", "throw", "child", "ball")
    snapshot = si

    report = SemanticSenseValidator.evaluate(si,
      fetch_base_senses: fn "throw" -> throw_senses() end)

    assert report.status == :abstained
    assert report.baseline_id == "throw|verb|5"
    assert report.proposed_id == nil
    assert report.reason == :insufficient_lexical_evidence

    assert Enum.any?(report.exclusions, fn excluded ->
      excluded.id == "throw|verb|5" and
        {:missing_restriction_witness, :cricket_bowling} in excluded.exclusions
    end)

    refute report.sense_meaning_verified?
    refute report.synaptic_weight_transferred?
    refute report.applied_to_lifg?
    refute report.brain_cells_stimulated?
    assert si == snapshot
  end

  test "the exact picture context can retain a provisional drawing hypothesis" do
    si = statement("draws", "draw", "artist", "picture")

    report = SemanticSenseValidator.evaluate(si,
      fetch_base_senses: fn "draw" -> draw_senses() end)

    assert report.status == :provisional_hypothesis
    assert report.baseline_id == "draw|verb|2"
    assert report.proposed_id == "draw|verb|2"
    assert report.validator_score_gap > 0.10
    assert report.excluded_count == 0
    assert report.sense_count == 3
    refute report.sense_meaning_verified?
  end

  test "intransitive-only dictionary labels are excluded from an overt-object SVO" do
    senses = [
      sense("write", 1, "To form letters and words on a page.", ["transitive"]),
      sense("write", 2, "To be the author of a letter.", ["transitive"]),
      sense("write", 6, "(intransitive) To be an author.", ["intransitive"]),
      sense("write", 7, "To communicate by writing.", ["ambitransitive"])
    ]

    si = statement("writes", "write", "author", "letter")
    report = SemanticSenseValidator.evaluate(si,
      fetch_base_senses: fn "write" -> senses end)

    assert report.status in [:abstained, :provisional_hypothesis]
    assert report.excluded_count == 1
    assert Enum.any?(report.exclusions, fn excluded ->
      excluded.id == "write|verb|6" and
        :intransitive_with_object in excluded.exclusions
    end)
    refute Enum.any?(report.eligible_top, &(&1.id == "write|verb|6"))
    assert Enum.any?(report.eligible_top, &(&1.id == "write|verb|7"))
  end

  test "contradictory dictionary valency tags fail closed instead of granting compatibility" do
    conflicting = [
      sense("write", 1, "(intransitive) To be an author.", ["transitive"]),
      sense("write", 2, "To write a letter to someone.", ["transitive"]),
      sense("write", 3, "To record information by typing.", ["transitive"])
    ]

    si = statement("writes", "write", "author", "letter")
    report = SemanticSenseValidator.evaluate(si,
      fetch_base_senses: fn _ -> conflicting end)

    assert report.excluded_count == 1
    assert Enum.any?(report.exclusions, fn row ->
      row.id == "write|verb|1" and :conflicting_dictionary_grammar in row.exclusions
    end)
    refute Enum.any?(report.eligible_top, &(&1.id == "write|verb|1"))
  end

  test "radar detection gloss cannot borrow portrait evidence without radar context" do
    senses = [
      sense("paint", 1, "To apply paint to a wall."),
      sense("paint", 5, "To create a portrait with paints."),
      sense("paint", 10, "(slang, transitive) To detect something with radar.")
    ]

    si = statement("paints", "paint", "artist", "portrait")
    report = SemanticSenseValidator.evaluate(si,
      fetch_base_senses: fn "paint" -> senses end)

    assert report.status == :provisional_hypothesis
    assert report.proposed_id == "paint|verb|5"
    assert Enum.any?(report.exclusions, fn row ->
      row.id == "paint|verb|10" and
        {:missing_restriction_witness, :radar} in row.exclusions
    end)
    refute report.sense_meaning_verified?
    refute report.applied_to_lifg?
  end

  test "explicit specialist cue removes the missing-context exclusion but does not verify truth" do
    si = statement("throws", "throw", "bowler", "ball")
    report = SemanticSenseValidator.evaluate(si,
      fetch_base_senses: fn "throw" -> throw_senses() end)

    refute Enum.any?(report.exclusions, &(&1.id == "throw|verb|5"))
    assert Enum.any?(report.eligible_top, &(&1.id == "throw|verb|5"))
    refute report.sense_meaning_verified?
  end

  test "no trusted morphology witness or no validated SVO means no dictionary lookup" do
    original = statement("draws", "draw", "artist", "picture")

    cases = [
      %{original | source: :assistant},
      %{original | atl_slate: %{winners: []}},
      %{original | sentence: "The artist might draw a picture."},
      Map.update!(original, :active_cells, fn cells ->
        Enum.map(cells, fn
          %{id: "draws|verb|1"} = cell -> %{cell | semantic_atoms: []}
          cell -> cell
        end)
      end)
    ]

    for si <- cases do
      assert %{status: :skipped} = SemanticSenseValidator.evaluate(si,
        fetch_base_senses: fn _ -> raise "untrusted source queried dictionary" end)
    end
  end

  test "caller-supplied holdout labels count wrong-to-abstention, not a correct flip" do
    si = statement("throws", "throw", "child", "ball")
    opts = [fetch_base_senses: fn _ -> throw_senses() end,
      gold_id: "throw|verb|1", split: :holdout,
      label_source: :human_curated]

    report = SemanticSenseValidator.evaluate(si, opts)
    assert report.label_status == :caller_labeled
    assert report.gold_id == "throw|verb|1"
    assert report.baseline_correct? == false
    assert report.validator_correct? == false

    summary = SemanticSenseValidator.aggregate([report])
    assert summary.labeled == 1
    assert summary.wrong_to_right == 0
    assert summary.wrong_to_abstention == 1
    assert summary.validator_correct == 0
    refute summary.independently_verified_labels?
    refute summary.live_lifg_modified?
  end

  test "untrusted, nonholdout, or absent gold sense IDs do not enter metrics" do
    si = statement("draws", "draw", "artist", "picture")
    loader = fn _ -> draw_senses() end

    for options <- [
          [gold_id: "draw|verb|2"],
          [gold_id: "draw|verb|2", split: :train,
           label_source: :human_curated],
          [gold_id: "draw|verb|999", split: :holdout,
           label_source: :human_curated]
        ] do
      assert %{status: :skipped, reason: :invalid_or_untrusted_gold_label} =
               SemanticSenseValidator.evaluate(si,
                 Keyword.put(options, :fetch_base_senses, loader))
    end
  end
end
