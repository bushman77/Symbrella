defmodule Core.Brain.SynapticDisambiguationTest do
  use ExUnit.Case, async: false

  alias Brain.Synapses.Shadow
  alias Core.Brain.{SynapticDisambiguation, SynapticExperience}
  alias Core.Semantic.{EpisodicProjection, SentenceMeaning}

  @svo_evidence [:contiguous_surface, :attested_nominal_subject,
                 :attested_nominal_object, :attested_finite_present_verb]
  @verb_tags ["tag:form-of", "tag:indicative", "tag:present",
              "tag:singular", "tag:third-person"]

  setup do
    {:ok, server} = Shadow.start_link(name: nil, durable?: false)
    %{server: server}
  end

  # Test-only manually constructed probability slate: these scores and IDs
  # are NOT claims about the true dictionary senses of "draws".
  defp interpreted(actor, patient, probabilities \\ %{ "draws|verb|1" => 0.499,
                                                       "draws|verb|2" => 0.501 }) do
    actor_id = "#{actor}|noun|1"
    patient_id = "#{patient}|noun|1"
    choices = [
      %{token_index: 1, id: actor_id, lemma: actor},
      %{token_index: 2, id: "draws|verb|1", lemma: "draws",
        candidate_probs: probabilities},
      %{token_index: 4, id: patient_id, lemma: patient}
    ]

    %{
      sentence: "The #{actor} draws a #{patient}.",
      source: :user,
      session_id: "labeled-ab-#{System.unique_integer([:positive])}",
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
        %{id: "draws|verb|1", norm: "draws", pos: "verb"},
        %{id: "draws|verb|2", norm: "draws", pos: "verb"}
      ]},
      trace: []
    }
    |> EpisodicProjection.attach()
    |> SentenceMeaning.attach()
  end

  defp learn(si, server) do
    {:ok, observed} = SynapticExperience.extract(si)
    assert {:ok, :recorded} = Shadow.observe(observed, server)
    observed
  end

  test "requires true scored competition and never confuses selection with gold",
       %{server: server} do
    si = interpreted("artist", "picture")
    opts = [synapse_shadow_server: server]

    before = SynapticDisambiguation.evaluate(si, opts)
    assert before.status == :unlabeled_diagnostic
    assert before.candidate_count == 2
    assert before.verb_sense_count == 2
    assert before.baseline.decision == :abstain
    assert before.counterfactual.decision == :abstain
    assert before.baseline.correct? == nil
    assert before.selected_lifg_id == "draws|verb|1"
    assert before.label_verified_by_system? == false
    assert before.applied_to_lifg? == false

    _ = learn(interpreted("painter", "picture"), server)

    after_learning = SynapticDisambiguation.evaluate(si, opts)
    assert after_learning.status == :unlabeled_diagnostic
    assert after_learning.total_signal > 0.0
    assert after_learning.baseline.decision == :abstain
    assert after_learning.counterfactual.decision == :abstain

    summary = SynapticDisambiguation.aggregate([before, after_learning])
    assert summary.eligible == 2
    assert summary.labeled == 0
    assert summary.measured_accuracy? == false
    assert summary.externally_verified_gold? == false
    assert summary.live_lifg_modified? == false
  end

  test "held-out curated gold can show abstention-to-right, without LIFG changes",
       %{server: server} do
    training = interpreted("artist", "diagram")
    holdout = interpreted("painter", "diagram")
    learned = learn(training, server)
    review = %{provenance: :human_review, evidence: learned.evidence,
               reviewer: "test-curator", decision: :confirm}
    assert {:ok, :confirmed} = Shadow.review(review, server)

    before = Shadow.snapshot(server)
    opts = [synapse_shadow_server: server,
      split: :holdout, label_source: :human_curated, gold_id: "draws|verb|1"]
    report = SynapticDisambiguation.evaluate(holdout, opts)

    assert report.status == :labeled_diagnostic
    assert report.baseline.decision == :abstain
    assert report.counterfactual.decision == "draws|verb|1"
    assert report.flip_class == :abstention_to_right
    assert report.counterfactual.correct? == true
    assert report.evidence_leakage_checked? == true
    assert report.trained_on_evaluation_example? == false
    assert Shadow.snapshot(server) == before

    aggregate = SynapticDisambiguation.aggregate([report])
    assert aggregate.baseline_abstentions == 1
    assert aggregate.abstention_to_right == 1
    assert aggregate.counterfactual_correct == 1
    assert aggregate.wrong_to_right == 0

    # A separate explicit label can expose a harmful signal.
    wrong = SynapticDisambiguation.evaluate(holdout,
      Keyword.put(opts, :gold_id, "draws|verb|2"))
    assert wrong.status == :labeled_diagnostic
    assert wrong.flip_class == :abstention_to_wrong
    assert wrong.counterfactual.correct? == false
  end

  test "held-out score flips can be harmful, even when a candidate gains weight",
       %{server: server} do
    training = interpreted("artist", "picture")
    heldout = interpreted("painter", "picture", %{
      "draws|verb|1" => 0.49, "draws|verb|2" => 0.51
    })
    observed = learn(training, server)
    assert {:ok, :confirmed} =
             Shadow.review(%{provenance: :human_review,
               evidence: observed.evidence, decision: :confirm,
               reviewer: "test-curator"}, server)

    opts = [synapse_shadow_server: server, split: :holdout,
      label_source: :human_curated, gold_id: "draws|verb|2",
      abstain_margin: 0.0]
    report = SynapticDisambiguation.evaluate(heldout, opts)

    assert report.status == :labeled_diagnostic
    assert report.baseline.decision == "draws|verb|2"
    assert report.counterfactual.decision == "draws|verb|1"
    assert report.flip_class == :right_to_wrong

    summary = SynapticDisambiguation.aggregate([report])
    assert summary.right_to_wrong == 1
    assert summary.baseline_correct == 1
    assert summary.counterfactual_correct == 0
  end

  test "refuses exact training replay, partial scores and single-sense slates",
       %{server: server} do
    seen = interpreted("artist", "poster")
    learn(seen, server)

    assert %{status: :skipped, reason: :seen_in_training_receipts} =
             SynapticDisambiguation.evaluate(seen, synapse_shadow_server: server)

    fresh = interpreted("painter", "poster")
    with_partial = Map.put(fresh, :lifg_choices, Enum.map(fresh.lifg_choices, fn
      %{token_index: 2} = ch ->
        %{ch | candidate_probs: %{"draws|verb|1" => 1.0}}
      row -> row
    end))
    assert %{status: :skipped, reason: :incomplete_or_invalid_score_slate} =
             SynapticDisambiguation.evaluate(with_partial, synapse_shadow_server: server)

    with_unscaled = Map.put(fresh, :lifg_choices, Enum.map(fresh.lifg_choices, fn
      %{token_index: 2} = ch ->
        %{ch | candidate_probs: %{"draws|verb|1" => 0.7,
                                  "draws|verb|2" => 0.7}}
      row -> row
    end))
    assert %{status: :skipped, reason: :incomplete_or_invalid_score_slate} =
             SynapticDisambiguation.evaluate(with_unscaled, synapse_shadow_server: server)

    single_sense = %{fresh | sense_candidates: %{2 => [%{id: "draws|verb|1"}]},
      lifg_choices: Enum.map(fresh.lifg_choices, fn
        %{token_index: 2} = ch -> %{ch | candidate_probs: %{"draws|verb|1" => 1.0}}
        row -> row
      end)}
    assert %{status: :skipped} =
             SynapticDisambiguation.evaluate(single_sense, synapse_shadow_server: server)
  end

  test "opt-in wide score evaluation covers 138 exact candidate IDs, not a truncated top two",
       %{server: server} do
    ids = Enum.map(1..138, fn n -> "draws|verb|#{n}" end)
    probability = 1.0 / length(ids)
    scores = Map.new(ids, fn id -> {id, probability} end)
    rows = Enum.map(ids, &%{id: &1, pos: "verb"})

    si = interpreted("painter", "picture")
    si =
      si
      |> Map.put(:sense_candidates, %{2 => rows})
      |> Map.put(:lifg_choices, Enum.map(si.lifg_choices, fn
        %{token_index: 2} = ch -> %{ch | candidate_probs: scores}
        row -> row
      end))

    result = SynapticDisambiguation.evaluate(si, synapse_shadow_server: server)

    assert result.status == :unlabeled_diagnostic
    assert result.candidate_count == 138
    assert result.verb_sense_count == 138
    assert result.baseline.decision == :abstain
    assert result.counterfactual.decision == :abstain
    assert result.applied_to_lifg? == false
  end

  test "rejects guessed gold without explicit holdout provenance", %{server: server} do
    si = interpreted("artist", "banner")
    assert %{status: :skipped, reason: :untrusted_or_out_of_slate_gold} =
             SynapticDisambiguation.evaluate(si, synapse_shadow_server: server,
               gold_id: "draws|verb|1")
    assert %{status: :skipped, reason: :untrusted_or_out_of_slate_gold} =
             SynapticDisambiguation.evaluate(si, synapse_shadow_server: server,
               gold_id: "draws|verb|99", split: :holdout,
               label_source: :human_curated)
    assert %{status: :skipped, reason: :untrusted_or_out_of_slate_gold} =
             SynapticDisambiguation.evaluate(si, synapse_shadow_server: server,
               gold_id: "draws|verb|1", split: :train,
               label_source: :human_curated)
  end
end
