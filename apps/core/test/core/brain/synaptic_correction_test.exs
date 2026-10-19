defmodule Core.Brain.SynapticCorrectionTest do
  use ExUnit.Case, async: false

  alias Brain.Synapses.Shadow
  alias Core.Brain.{SynapticCorrection, SynapticExperience}
  alias Core.Semantic.{EpisodicProjection, SentenceMeaning}

  @svo_evidence [:contiguous_surface, :attested_nominal_subject,
                 :attested_nominal_object, :attested_finite_present_verb]
  @verb_tags ["tag:form-of", "tag:indicative", "tag:present",
              "tag:singular", "tag:third-person"]

  defp statement(actor, patient) do
    actor_id = "#{actor}|noun|1"
    patient_id = "#{patient}|noun|1"
    selections = [
      %{token_index: 1, id: actor_id, lemma: actor},
      %{token_index: 2, id: "draws|verb|1", lemma: "draws"},
      %{token_index: 4, id: patient_id, lemma: patient}
    ]

    %{
      source: :user,
      sentence: "The #{actor} draws a #{patient}.",
      session_id: "correction-#{System.unique_integer([:positive, :monotonic])}",
      tokens: [
        %{index: 0, phrase: "The", n: 1},
        %{index: 1, phrase: actor, n: 1},
        %{index: 2, phrase: "draws", n: 1},
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
        %{id: patient_id, norm: patient, pos: "noun"},
        %{id: "draws|verb|1", norm: "draws", pos: "verb",
          semantic_atoms: @verb_tags, definition: "An inflected form of draw."}
      ],
      lifg_choices: selections,
      semantic_choices: selections,
      atl_slate: %{winners: selections},
      trace: []
    }
    |> EpisodicProjection.attach()
    |> SentenceMeaning.attach()
  end

  test "explicit correction identifies both complete clauses but not their truth" do
    assert {:ok, %{
      prior: "the artist draws a picture.",
      replacement: "the artist draws a diagram.",
      evidence_source: :explicit_patient_negation
    }} = SynapticCorrection.parse(
      "Actually, the artist draws a diagram, not a picture."
    )

    assert {:ok, _} = SynapticCorrection.parse(
      "ACTUALLY, the artist draws a diagram, NOT a picture"
    )
  end

  test "rejects ambiguous, hypothetical and loosely related statements" do
    for text <- [
      "The artist draws a diagram.",
      "The artist draws a diagram and a picture.",
      "Actually, the artist draws a picture, not a picture.",
      "Actually, the artist might draw a diagram, not a picture.",
      "Actually, the artist draws a diagram, not a picture or something.",
      "Actually, the artist draws a diagram, not a picture?",
      "Actually, the artist draws a diagram; not a picture.",
      "Actually, the artist draws a diagram, not a picture. Then something else.",
      "The artist draws a diagram, not a picture.",
      String.duplicate("x", 300)
    ] do
      assert SynapticCorrection.parse(text) == :not_a_supported_correction
    end
  end

  test "recorded prior yields read-only review proposal with matching actor and verb" do
    prior = statement("engraver", "seal")
    replacement = statement("engraver", "stamp")

    assert %{decision: :recorded} = hd(SynapticExperience.attach(prior).trace)
    before = Shadow.snapshot()
    original_weight = Shadow.edge("draws|verb|1", "seal|noun|1", :patient)

    assert %{status: :review_proposed,
      actor_id: "engraver|noun|1", predicate_id: "draws|verb|1",
      previous_patient_id: "seal|noun|1", proposed_patient_id: "stamp|noun|1",
      previous_review: :unreviewed,
      applied?: false, affects_lifg?: false, externally_verified?: false
    } = SynapticCorrection.evaluate(prior, replacement)

    assert Shadow.snapshot() == before
    assert Shadow.edge("draws|verb|1", "seal|noun|1", :patient) == original_weight
    assert Shadow.edge("draws|verb|1", "stamp|noun|1", :patient) == nil
    assert %{status: :review_proposed} =
             SynapticCorrection.evaluate(prior, replacement)
    assert Shadow.snapshot() == before
  end

  test "different actors or unknown original experience cannot propose correction" do
    prior = statement("sculptor", "bust")
    replacement = statement("sculptor", "statue")
    unrelated = statement("musician", "statue")

    assert %{status: :unknown_prior_experience} =
             SynapticCorrection.evaluate(prior, replacement)

    assert %{decision: :recorded} = hd(SynapticExperience.attach(prior).trace)

    assert %{status: :incompatible_or_ambiguous} =
             SynapticCorrection.evaluate(prior, unrelated)
  end

  test "a confirmed experience is not automatically retracted by a correction" do
    prior = statement("painter", "panel")
    new_event = statement("painter", "mural")
    assert %{decision: :recorded} = hd(SynapticExperience.attach(prior).trace)
    assert {:ok, :confirmed} =
             SynapticExperience.review(prior, :confirm, "reviewer")

    before = Shadow.edge("draws|verb|1", "panel|noun|1", :patient)
    assert %{status: :review_proposed, previous_review: :confirmed,
             applied?: false} =
             SynapticCorrection.evaluate(prior, new_event)
    assert Shadow.edge("draws|verb|1", "panel|noun|1", :patient) == before
  end

  test "rejected and separately recorded events are never rewritten by proposals" do
    prior = statement("surveyor", "chart")
    replacement = statement("surveyor", "diagram")
    assert %{decision: :recorded} = hd(SynapticExperience.attach(prior).trace)
    assert {:ok, :rejected} =
             SynapticExperience.review(prior, :reject, "reviewer")
    assert %{status: :prior_already_rejected} =
             SynapticCorrection.evaluate(prior, replacement)

    another = statement("drafter", "map")
    other_event = statement("drafter", "plan")
    assert %{decision: :recorded} = hd(SynapticExperience.attach(another).trace)
    assert %{decision: :recorded} = hd(SynapticExperience.attach(other_event).trace)

    before = Shadow.snapshot()
    assert %{status: :replacement_already_reported} =
             SynapticCorrection.evaluate(another, other_event)
    assert Shadow.snapshot() == before
  end

  test "forged ATL or narrative cannot become reviewable synaptic evidence" do
    prior = statement("designer", "poster")
    replacement = statement("designer", "banner")
    assert %{decision: :recorded} = hd(SynapticExperience.attach(prior).trace)

    assert %{status: :independent_structure_not_established} =
             SynapticCorrection.evaluate(%{prior | atl_slate: %{winners: []}}, replacement)

    assert %{status: :independent_structure_not_established} =
             SynapticCorrection.evaluate(
               %{prior | sentence: "The designer never draws a poster."}, replacement)

    assert %{status: :independent_structure_not_established} =
             SynapticCorrection.evaluate(prior,
               %{replacement | episodic_candidates: []})
  end

  test "an unmarked production correction cannot trigger nested interpretation" do
    si = %{
      source: :prod,
      sentence: "Actually, the artist draws a diagram, not a picture.",
      trace: []
    }

    assert SynapticCorrection.attach(si) == si
    model_origin = Map.put(si, :experience_origin, :model_output)
    assert SynapticCorrection.attach(model_origin) == model_origin
    assert SynapticCorrection.attach(
      Map.put(si, :experience_origin, :user_turn), synapse_correction?: false
    ) == Map.put(si, :experience_origin, :user_turn)
  end
end
