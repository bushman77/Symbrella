defmodule Core.Brain.SynapticExperienceTest do
  use ExUnit.Case, async: false

  alias Core.Brain.SynapticExperience
  alias Core.Semantic.{EpisodicProjection, SentenceMeaning}
  alias Brain.Synapses.Shadow

  @evidence [:contiguous_surface, :attested_nominal_subject,
             :attested_nominal_object, :attested_finite_present_verb]
  @verb_tags ["tag:form-of", "tag:indicative", "tag:present",
              "tag:singular", "tag:third-person"]

  # Independently supplied lexical/grammar evidence for an isolated test DB.
  # No invented ID is written to the dictionary or selected as a fact.
  defp experience(actor \\ "artist", patient \\ "picture") do
    sentence = "The #{actor} draws a #{patient}."
    actor_id = "#{actor}|noun|1"
    patient_id = "#{patient}|noun|1"

    choices = [
      %{id: actor_id, token_index: 1, lemma: actor},
      %{id: "draws|verb|1", token_index: 2, lemma: "draws"},
      %{id: patient_id, token_index: 4, lemma: patient}
    ]

    %{
      sentence: sentence,
      source: :user,
      session_id: "synapse-test-#{System.unique_integer([:positive])}",
      tokens: [
        %{index: 0, phrase: "The", n: 1},
        %{index: 1, phrase: actor, n: 1},
        %{index: 2, phrase: "draws", n: 1},
        %{index: 3, phrase: "a", n: 1},
        %{index: 4, phrase: patient <> ".", n: 1}
      ],
      sentence_signatures: [
        %{id: :finite_present_svo_candidate, source: :core_syntax,
          span: {0, 4}, evidence: @evidence,
          roles: %{subject_determiner: 0, subject_head: 1,
                   predicate: 2, object_determiner: 3, object_head: 4}}
      ],
      active_cells: [
        %{id: actor_id, norm: actor, pos: "noun"},
        %{id: "draws|verb|1", norm: "draws", pos: "verb",
          semantic_atoms: @verb_tags, definition: "An inflected form of draw."},
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

  test "revalidated user experience gives typed provisional edges, not verified senses" do
    si = experience()

    assert si.sentence_meaning.status == :structurally_interpreted
    assert {:ok, %{
             provenance: :user_reported,
             evidence: evidence,
             edges: [
               {"draws|verb|1", "artist|noun|1", :actor},
               {"draws|verb|1", "picture|noun|1", :patient}
             ]
           }} = SynapticExperience.extract(si)
    assert is_binary(evidence)

    before = Shadow.snapshot()
    out = SynapticExperience.attach(si)
    assert %{stage: :synaptic_experience, decision: :recorded,
             meta: %{applied_to_lifg?: false, learned_senses_verified?: false}} =
             hd(out.trace)
    assert Shadow.snapshot().experiences == before.experiences + 1

    assert %{weight: a, experiences: count, lexical_sense_verified?: false,
             deployment: :shadow_only} =
             Shadow.edge("draws|verb|1", "picture|noun|1", :patient)
    assert a > 0 and a <= 0.08
    assert count >= 1
    # Shadow bookkeeping must not mutate the comprehension or lexical slates.
    assert out.active_cells == si.active_cells
    assert out.lifg_choices == si.lifg_choices
    assert out.semantic_choices == si.semantic_choices
  end

  test "replay in another session cannot reinforce the same observation" do
    si = experience("sketcher", "portrait")
    assert %{stage: :synaptic_experience, decision: :recorded} =
             hd(SynapticExperience.attach(si).trace)

    first = Shadow.edge("draws|verb|1", "portrait|noun|1", :patient)
    repeated = %{si | session_id: "another-browser-session"}
    assert %{stage: :synaptic_experience, decision: :duplicate} =
             hd(SynapticExperience.attach(repeated).trace)

    assert Shadow.edge("draws|verb|1", "portrait|noun|1", :patient) == first
  end

  test "distinct contextual experiences reinforce only shared connections" do
    first = experience("drafter", "mural")
    second = experience("drafter", "diagram")

    assert %{decision: :recorded} = hd(SynapticExperience.attach(first).trace)
    initial_actor = Shadow.edge("draws|verb|1", "drafter|noun|1", :actor)
    assert %{decision: :recorded} = hd(SynapticExperience.attach(second).trace)

    later_actor = Shadow.edge("draws|verb|1", "drafter|noun|1", :actor)
    assert later_actor.experiences == initial_actor.experiences + 1
    assert later_actor.weight > initial_actor.weight

    assert %{experiences: 1} =
             Shadow.edge("draws|verb|1", "diagram|noun|1", :patient)
    assert %{experiences: 1} =
             Shadow.edge("draws|verb|1", "mural|noun|1", :patient)
  end

  test "assistant text, questions, and forged event or ATL are ineligible" do
    si = experience("artisan", "canvas")
    assert SynapticExperience.extract(%{si | source: :assistant}) == :ineligible
    assert SynapticExperience.extract(%{si | sentence: "What did the artisan draw?"}) ==
             :ineligible
    assert SynapticExperience.extract(%{si | atl_slate: %{winners: []}}) == :ineligible
    assert SynapticExperience.extract(%{si | episodic_candidates: []}) == :ineligible

    [event] = si.episodic_candidates
    assert SynapticExperience.extract(%{si |
      episodic_candidates: [%{event | value: "a fake value"}]
    }) == :ineligible
  end

  test "unmarked production calls and assistant-derived input never learn" do
    si = experience("illustrator", "poster")
    production = %{si | source: :prod}
    assert SynapticExperience.extract(production) == :ineligible
    assert {:ok, _observation} =
             SynapticExperience.extract(Map.put(production, :experience_origin, :user_turn))
    assert SynapticExperience.extract(Map.put(production, :experience_origin, :model_output)) ==
             :ineligible
    assert SynapticExperience.extract(%{si | source: :assistant}) == :ineligible
  end

  test "turning off shadow collection has no synaptic effect" do
    si = experience("designer", "plan")
    before = Shadow.snapshot()
    out = SynapticExperience.attach(si, synapse_shadow?: false)
    assert %{stage: :synaptic_experience, decision: :disabled} = hd(out.trace)
    assert Shadow.snapshot() == before
  end

  test "shadow engine clamps experience-driven weights and validates edges" do
    from = "sketches|verb|1"
    to_actor = "traveler|noun|1"
    to_patient = "atlas|noun|1"
    edges = [{from, to_actor, :actor}, {from, to_patient, :patient}]

    for i <- 1..20 do
      assert {:ok, :recorded} = Shadow.observe(%{
        provenance: :user_reported,
        evidence: "distinct-support-#{i}",
        edges: edges
      })
    end

    assert %{weight: weight, experiences: 20, deployment: :shadow_only} =
             Shadow.edge(from, to_patient, :patient)
    assert_in_delta weight, 0.08, 1.0e-8

    assert {:error, :invalid_observation} =
             Shadow.observe(%{
               provenance: :user_reported,
               evidence: "bad-edge",
               edges: [{from, "atlas|noun|1", :some_invented_relation}]
             })
    assert Shadow.snapshot().live_priming? == false
    assert Shadow.snapshot().persisted? == false
  end

  test "reviewed rejection removes an experience without allowing replay to restore it" do
    si = experience("engraver", "plaque")
    assert %{decision: :recorded} = hd(SynapticExperience.attach(si).trace)
    assert %{weight: w} = Shadow.edge("draws|verb|1", "plaque|noun|1", :patient)
    assert_in_delta w, 0.01, 1.0e-8

    assert {:ok, :rejected} = SynapticExperience.review(si, :reject, "local-reviewer")
    assert Shadow.edge("draws|verb|1", "plaque|noun|1", :patient) == nil
    assert Shadow.edge("draws|verb|1", "engraver|noun|1", :actor) == nil

    assert %{decision: :duplicate} =
             hd(SynapticExperience.attach(%{si | session_id: "replay"}).trace)
    assert Shadow.edge("draws|verb|1", "plaque|noun|1", :patient) == nil

    assert {:ok, :unchanged} = SynapticExperience.review(si, :reject, "local-reviewer")

    # A new explicit review may reverse a mistaken rejection; duplicate
    # statements themselves can never undo the rejection.
    assert {:ok, :confirmed} =
             SynapticExperience.review(si, :confirm, "local-reviewer")
    assert %{weight: confirmed_weight, experiences: 1,
             reviewed_experiences: 1, lexical_sense_verified?: false} =
             Shadow.edge("draws|verb|1", "plaque|noun|1", :patient)
    assert_in_delta confirmed_weight, 0.03, 1.0e-8
  end

  test "rejecting one experience preserves reinforcement from a different one" do
    first = experience("illustrator", "banner")
    second = experience("illustrator", "pamphlet")

    assert %{decision: :recorded} = hd(SynapticExperience.attach(first).trace)
    assert %{decision: :recorded} = hd(SynapticExperience.attach(second).trace)

    assert %{weight: combined, experiences: 2} =
             Shadow.edge("draws|verb|1", "illustrator|noun|1", :actor)
    assert_in_delta combined, 0.02, 1.0e-8

    assert {:ok, :rejected} = SynapticExperience.review(first, :reject, "local-reviewer")

    assert %{weight: remaining, experiences: 1} =
             Shadow.edge("draws|verb|1", "illustrator|noun|1", :actor)
    assert_in_delta remaining, 0.01, 1.0e-8

    assert Shadow.edge("draws|verb|1", "banner|noun|1", :patient) == nil
    assert %{weight: 0.01} =
             Shadow.edge("draws|verb|1", "pamphlet|noun|1", :patient)
  end

  test "human confirmation adjusts contribution once, not on repeated reviews" do
    si = experience("cartographer", "chart")
    assert %{decision: :recorded} = hd(SynapticExperience.attach(si).trace)
    assert {:ok, :confirmed} = SynapticExperience.review(si, :confirm, "human-curator")
    initial = Shadow.edge("draws|verb|1", "chart|noun|1", :patient)
    assert_in_delta initial.weight, 0.03, 1.0e-8
    assert {:ok, :unchanged} = SynapticExperience.review(si, :confirm, "human-curator")
    assert Shadow.edge("draws|verb|1", "chart|noun|1", :patient) == initial
    assert initial.lexical_sense_verified? == false
  end

  test "unreviewed, unknown and forged correction requests never change weights" do
    si = experience("surveyor", "map")
    assert {:error, :unknown_experience} =
             SynapticExperience.review(si, :reject, "local-reviewer")
    assert {:error, :invalid_review} =
             SynapticExperience.review(si, :reject, "")
    assert {:error, :invalid_review} =
             SynapticExperience.review(si, :reject, :not_a_human_identity)

    assert %{decision: :recorded} = hd(SynapticExperience.attach(si).trace)
    before = Shadow.edge("draws|verb|1", "map|noun|1", :patient)
    [event] = si.episodic_candidates

    forged = %{si | episodic_candidates: [%{event | value: "different"}]}
    assert {:error, :ineligible} =
             SynapticExperience.review(forged, :reject, "local-reviewer")
    assert Shadow.edge("draws|verb|1", "map|noun|1", :patient) == before
  end

  test "shadow preview compares only existing lexical candidates and never rewrites LIFG" do
    si = experience("painter", "fresco")
    assert %{decision: :recorded} = hd(SynapticExperience.attach(si).trace)

    rows = [
      %{id: "draws|verb|1", token_index: 2, norm: "draws"},
      %{id: "draws|verb|2", token_index: 2, norm: "draws"}
    ]

    complete = Map.put(si, :sense_candidates, %{2 => rows})
    before = Shadow.snapshot()

    assert %{status: :shadow_only, affects_lifg?: false,
             senses_verified?: false,
             candidates: [
               %{id: "draws|verb|1", shadow_support: support},
               %{id: "draws|verb|2", shadow_support: other_support}
             ]} =
             SynapticExperience.preview(complete, ["draws|verb|1", "draws|verb|2"])

    assert_in_delta support, 0.01, 1.0e-8
    assert_in_delta other_support, 0.0, 1.0e-8
    assert Shadow.snapshot() == before

    assert %{status: :ineligible_or_incomplete_slate} =
             SynapticExperience.preview(complete, ["not-in-dictionary|verb|1"])
    assert %{status: :ineligible_or_incomplete_slate} =
             SynapticExperience.preview(si, ["draws|verb|1"])
    assert si.lifg_choices == complete.lifg_choices
  end

end
