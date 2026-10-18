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
end
