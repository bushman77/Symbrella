defmodule Core.Brain.SynapticFeedbackTest do
  use ExUnit.Case, async: false

  alias Brain.Synapses.Shadow
  alias Core.Brain.{SynapticExperience, SynapticFeedback}
  alias Core.Semantic.{EpisodicProjection, SentenceMeaning}

  @svo_evidence [:contiguous_surface, :attested_nominal_subject,
    :attested_nominal_object, :attested_finite_present_verb]
  @verb_tags ["tag:form-of", "tag:indicative", "tag:present",
    "tag:singular", "tag:third-person"]

  setup do
    {:ok, server} = Shadow.start_link(name: nil, durable?: false)
    old_flag = Application.get_env(:core, :synaptic_feedback_live_enabled?, false)
    Application.put_env(:core, :synaptic_feedback_live_enabled?, false)
    on_exit(fn ->
      Application.put_env(:core, :synaptic_feedback_live_enabled?, old_flag)
    end)
    %{server: server}
  end

  defp interpreted(actor, patient, winner \\ "draws|verb|1") do
    actor_id = "#{actor}|noun|1"
    patient_id = "#{patient}|noun|1"
    scores = %{"draws|verb|1" => 0.501, "draws|verb|2" => 0.499}
    choices = [
      %{token_index: 1, id: actor_id, lemma: actor, pos: "noun"},
      %{token_index: 2, id: winner, lemma: "draws", pos: "verb",
        score: Map.fetch!(scores, winner), candidate_probs: scores},
      %{token_index: 4, id: patient_id, lemma: patient, pos: "noun"}
    ]
    %{
      sentence: "The #{actor} draws a #{patient}.",
      source: :user, session_id: "feedback-#{System.unique_integer([:positive])}",
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
          semantic_atoms: @verb_tags, definition: "Present of draw."},
        %{id: "draws|verb|2", norm: "draws", pos: "verb",
          semantic_atoms: @verb_tags, definition: "Alternate present of draw."},
        %{id: patient_id, norm: patient, pos: "noun"}
      ],
      lifg_choices: choices,
      semantic_choices: choices,
      atl_slate: %{winners: choices},
      sense_candidates: %{2 => [
        %{id: "draws|verb|1", norm: "draws", pos: "verb",
          definition: "Present of draw."},
        %{id: "draws|verb|2", norm: "draws", pos: "verb",
          definition: "Alternate present of draw."}
      ]},
      trace: []
    }
    |> EpisodicProjection.attach()
    |> SentenceMeaning.attach()
  end

  defp reviewed_training(server) do
    train = interpreted("artist", "picture", "draws|verb|2")
    {:ok, observed} = SynapticExperience.extract(train)
    assert {:ok, :recorded} = Shadow.observe(observed, server)
    assert {:ok, :confirmed} = Shadow.review(%{
      provenance: :human_review, decision: :confirm,
      evidence: observed.evidence, reviewer: "independent-curator"
    }, server)
  end

  defp holdout do
    interpreted("painter", "picture")
    |> Map.put(:source, :prod)
    |> Map.put(:experience_origin, :user_turn)
  end

  test "default live gate only reports A/B without altering decisions", %{server: server} do
    reviewed_training(server)
    si = holdout()
    state = Shadow.snapshot(server)

    result = SynapticFeedback.attach(si,
      synapse_shadow_server: server, synaptic_feedback_live?: true)

    assert result.lifg_choices == si.lifg_choices
    assert result.semantic_choices == si.semantic_choices
    assert result.atl_slate == si.atl_slate
    assert Shadow.snapshot(server) == state
    assert %{stage: :synaptic_feedback, decision: :observed,
      meta: %{changed_decision?: true, applied_to_lifg?: false,
        guard: :live_gate_closed}} = hd(result.trace)
  end

  test "live opt-in requires reviewed support and independently reprojects SVO",
       %{server: server} do
    reviewed_training(server)
    Application.put_env(:core, :synaptic_feedback_live_enabled?, true)
    si = holdout()
    previous = Shadow.snapshot(server)

    result = SynapticFeedback.attach(si,
      synapse_shadow_server: server, synaptic_feedback_live?: true)

    assert %{stage: :synaptic_feedback, decision: :applied,
      meta: %{applied_to_lifg?: true,
        change: %{from: "draws|verb|1", to: "draws|verb|2"}}} =
        hd(result.trace)
    assert Enum.find(result.lifg_choices, &(&1.token_index == 2)).id ==
      "draws|verb|2"
    assert Enum.find(result.semantic_choices, &(&1.token_index == 2)).id ==
      "draws|verb|2"
    assert result.sentence_meaning.status == :structurally_interpreted
    assert SynapticExperience.extract(result) != :ineligible
    assert Shadow.snapshot(server) == previous

    audited = SynapticExperience.attach(result)
    assert %{stage: :synaptic_experience,
      decision: :skipped_feedback_selected} = hd(audited.trace)
    assert Shadow.snapshot(server) == previous
  end

  test "unreviewed experience remains shadow only even with both opt-ins",
       %{server: server} do
    train = interpreted("artist", "picture", "draws|verb|2")
    {:ok, observed} = SynapticExperience.extract(train)
    assert {:ok, :recorded} = Shadow.observe(observed, server)
    Application.put_env(:core, :synaptic_feedback_live_enabled?, true)
    si = holdout()

    result = SynapticFeedback.attach(si,
      synapse_shadow_server: server, synaptic_feedback_live?: true)

    assert result.lifg_choices == si.lifg_choices
    assert hd(result.trace).decision == :guarded
  end

  test "refuses wrong POS, missing complete slate and exact training replay",
       %{server: server} do
    reviewed_training(server)
    Application.put_env(:core, :synaptic_feedback_live_enabled?, true)
    si = holdout()
    opts = [synapse_shadow_server: server, synaptic_feedback_live?: true]

    mixed = put_in(si.sense_candidates[2], [
      %{id: "draws|verb|1"}, %{id: "draws|noun|2"}
    ])
    assert SynapticFeedback.attach(mixed, opts).lifg_choices == si.lifg_choices

    incomplete = update_in(si.lifg_choices, fn choices ->
      Enum.map(choices, fn
        %{token_index: 2} = c ->
          %{c | candidate_probs: %{"draws|verb|1" => 1.0}}
        c -> c
      end)
    end)
    assert SynapticFeedback.attach(incomplete, opts).lifg_choices ==
      incomplete.lifg_choices

    seen = interpreted("artist", "picture", "draws|verb|2")
      |> Map.put(:source, :prod)
      |> Map.put(:experience_origin, :user_turn)
    assert %{stage: :synaptic_feedback, decision: :skipped,
      reason: :seen_in_training_receipts} =
        hd(SynapticFeedback.attach(seen, opts).trace)
  end

  test "cannot override an MWE-owned semantic choice", %{server: server} do
    reviewed_training(server)
    Application.put_env(:core, :synaptic_feedback_live_enabled?, true)
    si = holdout()
    shadowed = update_in(si.semantic_choices, fn choices ->
      Enum.map(choices, fn
        %{token_index: 2} = c -> Map.put(c, :kind, :mwe)
        c -> c
      end)
    end)

    result = SynapticFeedback.attach(shadowed,
      synapse_shadow_server: server, synaptic_feedback_live?: true)
    assert result.lifg_choices == shadowed.lifg_choices
    assert hd(result.trace).decision == :guarded
  end
end
