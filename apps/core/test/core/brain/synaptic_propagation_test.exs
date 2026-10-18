defmodule Core.Brain.SynapticPropagationTest do
  use ExUnit.Case, async: false

  alias Brain.Synapses.Shadow
  alias Core.Brain.{SynapticExperience, SynapticPropagation}
  alias Core.Semantic.{EpisodicProjection, SentenceMeaning}

  @svo_evidence [:contiguous_surface, :attested_nominal_subject,
                 :attested_nominal_object, :attested_finite_present_verb]
  @verb_tags ["tag:form-of", "tag:indicative", "tag:present",
              "tag:singular", "tag:third-person"]

  setup do
    {:ok, server} = Shadow.start_link(name: nil, durable?: false)
    %{server: server}
  end

  defp statement(actor, patient) do
    actor_id = "#{actor}|noun|1"
    patient_id = "#{patient}|noun|1"
    choices = [
      %{token_index: 1, id: actor_id, lemma: actor},
      %{token_index: 2, id: "draws|verb|1", lemma: "draws"},
      %{token_index: 4, id: patient_id, lemma: patient}
    ]

    %{
      sentence: "The #{actor} draws a #{patient}.",
      source: :user,
      session_id: "propagation-test-#{System.unique_integer([:positive])}",
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
        %{id: "draws|verb|1", norm: "draws", pos: "verb",
          semantic_atoms: @verb_tags, definition: "An inflected form of draw."},
        %{id: patient_id, norm: patient, pos: "noun"}
      ],
      lifg_choices: choices,
      semantic_choices: choices,
      atl_slate: %{winners: choices},
      sense_candidates: %{2 => [
        %{id: "draws|verb|2", norm: "draws", pos: "verb"},
        %{id: "draws|verb|1", norm: "draws", pos: "verb"},
        %{id: "artist|noun|1", norm: "artist", pos: "noun"}
      ]},
      trace: []
    }
    |> EpisodicProjection.attach()
    |> SentenceMeaning.attach()
  end

  test "existing exact verb slate receives only earlier patient-conditioned evidence",
       %{server: server} do
    si = statement("artist", "picture")
    before = SynapticPropagation.compare(si, synapse_shadow_server: server)

    assert before.status == :shadow_comparison
    assert before.propagation_on.supported_count == 0
    assert before.propagation_off.supported_count == 0
    assert before.candidates_checked == 2
    assert before.applied_to_lifg? == false

    {:ok, observed} = SynapticExperience.extract(si)
    assert {:ok, :recorded} = Shadow.observe(observed, server)

    after_learning = SynapticPropagation.compare(si, synapse_shadow_server: server)
    assert after_learning.status == :shadow_comparison
    assert after_learning.propagation_on.supported_count == 1
    assert after_learning.propagation_off.supported_count == 0

    [unlearned, learned] = after_learning.propagation_on.candidates
    assert unlearned.id == "draws|verb|2"
    assert unlearned.shadow_signal == 0.0
    assert learned.id == "draws|verb|1"
    assert_in_delta learned.shadow_signal, 0.0025, 1.0e-10
    assert learned.exact_sense_verified? == false

    another_patient = statement("artist", "water")
    refute SynapticPropagation.compare(another_patient,
      synapse_shadow_server: server).propagation_on.supported_count > 0

    # A read-only trace cannot change the source lexical selection.
    before_state = Shadow.snapshot(server)
    attached = SynapticPropagation.attach(si, synapse_shadow_server: server)
    assert %{stage: :synaptic_propagation, decision: :shadow_comparison,
             meta: %{applied_to_lifg?: false,
                     evaluated_before_experience_admission?: true}} =
             hd(attached.trace)
    assert attached.lifg_choices == si.lifg_choices
    assert attached.sense_candidates == si.sense_candidates
    assert Shadow.snapshot(server) == before_state
  end

  test "forged, assistant, missing-slate or unmarked production data cannot propagate",
       %{server: server} do
    si = statement("painter", "fresco")
    assert SynapticPropagation.compare(%{si | source: :assistant},
      synapse_shadow_server: server).status == :ineligible
    assert SynapticPropagation.compare(%{si | sentence: "The painter never draws a fresco."},
      synapse_shadow_server: server).status == :ineligible
    assert SynapticPropagation.compare(%{si | atl_slate: %{winners: []}},
      synapse_shadow_server: server).status == :ineligible
    assert SynapticPropagation.compare(%{si | sense_candidates: %{}},
      synapse_shadow_server: server).status == :ineligible
    assert SynapticPropagation.compare(%{si | source: :prod},
      synapse_shadow_server: server).status == :ineligible

    allowed = %{si | source: :prod, experience_origin: :user_turn}
    assert SynapticPropagation.compare(allowed,
      synapse_shadow_server: server).status == :shadow_comparison
    assert SynapticPropagation.attach(si, synapse_shadow?: false) == si
    assert SynapticPropagation.attach(si, synapse_propagation?: false) == si
  end

  test "review changes signal without changing current selected dictionary sense",
       %{server: server} do
    si = statement("artist", "diagram")
    {:ok, observed} = SynapticExperience.extract(si)
    assert {:ok, :recorded} = Shadow.observe(observed, server)
    initial = SynapticPropagation.compare(si, synapse_shadow_server: server)
    initial_signal = Enum.find(initial.propagation_on.candidates,
      &(&1.id == "draws|verb|1")).shadow_signal
    assert_in_delta initial_signal, 0.0025, 1.0e-10

    review = %{evidence: observed.evidence, provenance: :human_review,
               reviewer: "developer", decision: :confirm}
    assert {:ok, :confirmed} = Shadow.review(review, server)

    confirmed = SynapticPropagation.compare(si, synapse_shadow_server: server)
    confirmed_signal = Enum.find(confirmed.propagation_on.candidates,
      &(&1.id == "draws|verb|1")).shadow_signal
    assert_in_delta confirmed_signal, 0.03, 1.0e-10

    assert {:ok, :rejected} =
             Shadow.review(%{review | decision: :reject}, server)
    rejected = SynapticPropagation.compare(si, synapse_shadow_server: server)
    assert rejected.propagation_on.supported_count == 0
    assert si.lifg_choices == statement("artist", "diagram").lifg_choices
  end
end
