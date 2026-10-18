defmodule Core.Brain.RoleSemanticContextTest do
  use ExUnit.Case, async: false

  alias Core.Brain.{RoleSemanticContext, SemanticSenseValidator}
  alias Core.Semantic.{EpisodicProjection, SentenceMeaning}

  @svo_evidence [:contiguous_surface, :attested_nominal_subject,
                 :attested_nominal_object, :attested_finite_present_verb]
  @verb_tags ["tag:form-of", "tag:indicative", "tag:present",
              "tag:singular", "tag:third-person"]

  defp si(actor, verb, base, patient, actor_gloss, patient_gloss) do
    actor_id = "#{actor}|noun|1"
    patient_id = "#{patient}|noun|1"
    verb_id = "#{verb}|verb|1"

    choices = [
      %{token_index: 1, id: actor_id, lemma: actor,
        definition: actor_gloss,
        candidate_probs: %{actor_id => 1.0}},
      %{token_index: 2, id: verb_id, lemma: verb},
      %{token_index: 4, id: patient_id, lemma: patient,
        definition: patient_gloss,
        candidate_probs: %{patient_id => 1.0}}
    ]

    %{
      source: :user,
      session_id: "role-context-test",
      sentence: "The #{actor} #{verb} a #{patient}.",
      tokens: [
        %{index: 0, phrase: "The", n: 1},
        %{index: 1, phrase: actor, n: 1},
        %{index: 2, phrase: verb, n: 1},
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
        %{id: actor_id, norm: actor, pos: "noun", definition: actor_gloss},
        %{id: patient_id, norm: patient, pos: "noun", definition: patient_gloss},
        %{id: verb_id, norm: verb, pos: "verb",
          definition: "An inflected form of #{base}.",
          semantic_atoms: @verb_tags}
      ],
      lifg_choices: choices,
      semantic_choices: choices,
      atl_slate: %{winners: choices},
      trace: []
    }
    |> EpisodicProjection.attach()
    |> SentenceMeaning.attach()
  end

  defp sense(base, index, definition, tags \\ ["transitive"]) do
    %{id: "#{base}|verb|#{index}", norm: base, pos: "verb",
      definition: definition, gram_function: tags, synonyms: [],
      semantic_atoms: [], antonyms: []}
  end

  defp run(si, senses, opts \\ []) do
    evaluator_opts = Keyword.merge(
      [fetch_base_senses: fn _ -> senses end], opts)

    {SemanticSenseValidator.evaluate(si, Keyword.put(evaluator_opts, :role_context?, false)),
     SemanticSenseValidator.evaluate(si, Keyword.put(evaluator_opts, :role_context?, true))}
  end

  test "meal's selected noun sense weakly supports cooking food, not every cook meaning" do
    input = si("baker", "cooks", "cook", "meal",
      "A person who bakes bread.", "Food prepared and eaten at one sitting.")
    senses = [
      sense("cook", 1, "To prepare food for eating by heating ingredients."),
      sense("cook", 2, "To smelt metals in a furnace."),
      sense("cook", 3, "To defeat an opponent in a game.")
    ]

    snapshot = input
    {off, on} = run(input, senses)

    assert off.status == :abstained
    assert off.proposed_id == nil
    assert on.status == :provisional_hypothesis
    assert on.proposed_id == "cook|verb|1"
    assert on.proposed_with_patient_evidence?
    assert on.role_expansion_status == :grounded
    assert [%{id: "cook|verb|1", lexical_score: direct_score,
      patient_support: patient_support, role_support: combined} | _] = on.eligible_top
    assert_in_delta direct_score, 0.0, 1.0e-9
    assert patient_support >= 0.08
    assert combined <= 0.13
    assert input == snapshot
    refute on.applied_to_lifg?
    refute on.synaptic_weight_transferred?
    refute on.sense_meaning_verified?
  end

  test "verified selected ship noun sense supplies vessel, never generic word association" do
    input = si("captain", "steers", "steer", "ship",
      "A person commanding a vessel.", "A large seagoing vessel for cargo.")
    senses = [
      sense("steer", 2, "To guide the course of a vessel."),
      sense("steer", 4, "To direct a herd of animals."),
      sense("steer", 5, "To influence the actions of a person.")
    ]

    {off, on} = run(input, senses)
    assert off.status == :abstained
    assert on.proposed_id == "steer|verb|2"
    assert on.validator_score_gap >= 0.1
    assert on.role_expansion_enabled?
    assert Enum.any?(on.eligible_top, fn candidate ->
      candidate.id == "steer|verb|2" and
        Enum.any?(candidate.one_hop_witnesses, &("vessel" in &1.matched))
    end)
  end

  test "plant and tree relation can be proposed without inventing a new dictionary sense" do
    input = si("gardener", "plants", "plant", "tree",
      "A person who tends gardens.", "A tall perennial woody plant.")
    senses = [
      sense("plant", 1, "To place a plant in soil to grow."),
      sense("plant", 3, "To hide evidence of a crime."),
      sense("plant", 6, "To generate an idea in a person's mind.")
    ]

    {_off, on} = run(input, senses)
    assert on.proposed_id == "plant|verb|1"
    refute on.sense_meaning_verified?
    assert on.sense_count == 3
  end

  test "specialist cricket exclusion cannot be bypassed by a selected ball gloss" do
    input = si("child", "throws", "throw", "ball",
      "A young person.", "A round object used in games such as cricket.")
    senses = [
      sense("throw", 1, "To hurl an object so it travels through the air."),
      sense("throw", 5, "(transitive) To deliver the ball by straightening the bowling arm."),
      sense("throw", 6, "To send an error to a computer exception handler.")
    ]

    {_off, on} = run(input, senses)
    assert on.status == :abstained
    assert on.proposed_id == nil
    assert Enum.any?(on.exclusions, fn row ->
      row.id == "throw|verb|5" and
        {:missing_restriction_witness, :cricket_bowling} in row.exclusions
    end)
    refute Enum.any?(on.eligible_top, &(&1.id == "throw|verb|5"))
  end

  test "ambiguous selected noun sense fails closed even with a useful gloss" do
    original = si("baker", "cooks", "cook", "meal",
      "A baker.", "Food prepared and eaten.")
    changed = Map.update!(original, :lifg_choices, fn choices ->
      Enum.map(choices, fn
        %{token_index: 4} = choice ->
          %{choice | candidate_probs: %{
            "meal|noun|1" => 0.51, "meal|noun|2" => 0.49
          }}
        choice -> choice
      end)
    end)

    report = RoleSemanticContext.extract(changed)
    patient = Enum.find(report.contexts, &(&1.role == :patient))
    assert patient.status == :unavailable_or_ambiguous
    assert patient.reason == :ambiguous_same_surface_noun_senses

    senses = [
      sense("cook", 1, "To prepare food for eating."),
      sense("cook", 2, "To smelt metal.")
    ]
    {_off, on} = run(changed, senses)
    assert on.proposed_id == nil
    assert on.status == :abstained
  end

  test "forged, absent or conflicting selected noun dictionary evidence cannot expand" do
    original = si("baker", "cooks", "cook", "meal",
      "A baker.", "Food prepared and eaten.")

    tampered = Map.update!(original, :active_cells, fn cells ->
      Enum.map(cells, fn
        %{id: "meal|noun|1"} = cell -> %{cell | definition: "A cricket bowling game."}
        cell -> cell
      end)
    end)

    no_scores = Map.update!(original, :lifg_choices, fn choices ->
      Enum.map(choices, fn
        %{token_index: 4} = c -> %{c | candidate_probs: nil}
        c -> c
      end)
    end)

    mismatched = RoleSemanticContext.extract(tampered)
    assert Enum.find(mismatched.contexts, &(&1.role == :patient)).reason ==
             :missing_or_inconsistent_selected_noun_definition

    no_probabilities = RoleSemanticContext.extract(no_scores)
    assert Enum.find(no_probabilities.contexts, &(&1.role == :patient)).reason ==
             :missing_noun_probability_slate

    for invalid <- [tampered, no_scores, %{original | source: :assistant},
                    %{original | atl_slate: %{winners: []}}] do
      report = RoleSemanticContext.extract(invalid)
      patient = Enum.find(report.contexts, &(Map.get(&1, :role) == :patient))
      assert patient == nil or patient.status != :grounded_one_hop
    end
  end

  test "incomplete noun score distributions cannot masquerade as confidence" do
    input = si("baker", "cooks", "cook", "meal",
      "A maker of bread.", "Food prepared for eating.")

    invalid = Map.update!(input, :lifg_choices, fn choices ->
      Enum.map(choices, fn
        %{token_index: 4} = c ->
          %{c | candidate_probs: %{
            "meal|noun|1" => 0.6, "meal|noun|2" => 0.2
          }}
        c -> c
      end)
    end)

    result = RoleSemanticContext.extract(invalid)
    patient = Enum.find(result.contexts, &(&1.role == :patient))
    assert patient.status == :unavailable_or_ambiguous
    assert patient.reason == :incomplete_noun_probability_slate
    assert result.active_count == 1 or result.active_count == 0
  end

  test "only the selected noun sense contributes and extra alternative cells cannot leak" do
    input = si("baker", "cooks", "cook", "meal",
      "A person who bakes bread.", "Food eaten at a sitting.")

    with_decoy = Map.update!(input, :active_cells, fn cells ->
      [%{id: "meal|noun|2", norm: "meal", pos: "noun",
         definition: "A cricket bowling meal and radar game."} | cells]
    end)

    proof = RoleSemanticContext.extract(with_decoy)
    patient = Enum.find(proof.contexts, &(&1.role == :patient))
    assert patient.status == :grounded_one_hop
    assert patient.noun_sense_id == "meal|noun|1"
    assert "food" in patient.terms
    refute "cricket" in patient.terms
    refute "radar" in patient.terms
    refute proof.inferred_noun_meanings_verified?
  end

  test "role-context scoring stays bounded even when both roles share many terms" do
    input = si("baker", "cooks", "cook", "meal",
      "Food heat bake eat bread dishes kitchen preparing.",
      "Food heat bake eat bread dishes kitchen preparing.")
    proof = RoleSemanticContext.extract(input)
    candidate = sense("cook", 1,
      "To prepare food heat bake eat bread dishes in a kitchen.")

    result = RoleSemanticContext.support(proof, candidate)
    assert result.patient_signal > 0
    assert result.actor_signal > 0
    assert result.patient_signal <= 0.105
    assert result.actor_signal <= 0.045
    assert result.signal <= 0.13
    assert result.one_hop_only?
    refute result.verified_sense_equivalence?
  end

  defp tie_patient(input, norm, p \\ 0.5) do
    Map.update!(input, :lifg_choices, fn choices ->
      Enum.map(choices, fn
        %{token_index: 4} = choice ->
          %{choice | candidate_probs: %{
            "#{norm}|noun|1" => p,
            "#{norm}|noun|2" => 1.0 - p
          }}
        choice -> choice
      end)
    end)
  end

  test "unanimous gloss term across tied real noun IDs supports a provisional verb sense" do
    input =
      si("baker", "cooks", "cook", "meal",
        "A person who bakes bread.", "Food prepared and eaten at a sitting.")
      |> tie_patient("meal")

    noun_loader = fn "meal" ->
      [
        %{id: "meal|noun|1", norm: "meal", pos: "noun",
          definition: "Food prepared and eaten at a sitting."},
        %{id: "meal|noun|2", norm: "meal", pos: "noun",
          definition: "An occasion for eating food at a particular time."}
      ]
    end

    before = input
    proof = RoleSemanticContext.extract(input, fetch_noun_senses: noun_loader)
    patient = Enum.find(proof.contexts, &(&1.role == :patient))

    assert patient.status == :grounded_tie_consensus
    assert patient.noun_sense_id == nil
    assert patient.noun_sense_ids == ["meal|noun|1", "meal|noun|2"]
    assert patient.source == :unanimous_tied_dictionary_glosses
    assert "food" in patient.terms
    refute patient.noun_sense_independently_verified?

    senses = [
      sense("cook", 1, "(transitive) To prepare food for eating."),
      sense("cook", 2, "(transitive) To deceive a person for profit."),
      sense("cook", 3, "(transitive) To smelt metals in a furnace.")
    ]

    opts = [fetch_base_senses: fn _ -> senses end,
      fetch_noun_senses: noun_loader]

    baseline = SemanticSenseValidator.evaluate(input,
      Keyword.put(opts, :role_context?, false))
    expanded = SemanticSenseValidator.evaluate(input,
      Keyword.put(opts, :role_context?, true))

    assert baseline.status == :abstained
    assert baseline.proposed_id == nil
    assert expanded.status == :provisional_hypothesis
    assert expanded.proposed_id == "cook|verb|1"
    assert expanded.proposed_with_patient_evidence?
    assert Enum.any?(expanded.eligible_top, fn row ->
      row.id == "cook|verb|1" and
        Enum.any?(row.one_hop_witnesses, fn witness ->
          witness.provenance == :unanimous_tied_dictionary_glosses and
            witness.source_noun_sense_id == nil and
            witness.source_noun_sense_ids == ["meal|noun|1", "meal|noun|2"] and
            "food" in witness.matched
        end)
    end)
    assert input == before
    refute expanded.sense_meaning_verified?
    refute expanded.applied_to_lifg?
    refute expanded.synaptic_weight_transferred?
  end

  test "disagreeing noun sense glosses abstain instead of selecting first ID" do
    input =
      si("baker", "cooks", "cook", "meal",
        "A person who bakes bread.", "Food prepared and eaten at a sitting.")
      |> tie_patient("meal")

    noun_loader = fn _ ->
      [
        %{id: "meal|noun|1", norm: "meal", pos: "noun",
          definition: "Food prepared and eaten at a sitting."},
        %{id: "meal|noun|2", norm: "meal", pos: "noun",
          definition: "Ground grains used as animal fodder."}
      ]
    end

    proof = RoleSemanticContext.extract(input, fetch_noun_senses: noun_loader)
    patient = Enum.find(proof.contexts, &(&1.role == :patient))
    assert patient.status == :unavailable_or_ambiguous
    assert patient.reason == :no_unanimous_noun_gloss_terms

    senses = [
      sense("cook", 1, "To prepare food for eating."),
      sense("cook", 2, "To smelt ore."),
      sense("cook", 3, "To damage equipment.")
    ]
    on = SemanticSenseValidator.evaluate(input,
      role_context?: true,
      fetch_noun_senses: noun_loader,
      fetch_base_senses: fn _ -> senses end)
    assert on.status == :abstained
    assert on.proposed_id == nil
  end

  test "a lower-ranked noun cannot manufacture consensus with a higher noun winner" do
    input =
      si("baker", "cooks", "cook", "meal",
        "A person who bakes bread.", "Food prepared and eaten.")
      |> tie_patient("meal", 0.3)

    probe = RoleSemanticContext.extract(input,
      fetch_noun_senses: fn _ ->
        raise "wrongly queried dictionary before checking selected ID rank"
      end)

    patient = Enum.find(probe.contexts, &(&1.role == :patient))
    assert patient.status == :unavailable_or_ambiguous
    assert patient.reason == :selected_noun_not_highest_scored
  end

  test "unavailable or duplicate tied dictionary IDs fail closed" do
    input =
      si("baker", "cooks", "cook", "meal",
        "A baker of bread.", "Food prepared and eaten.")
      |> tie_patient("meal")

    for wrong <- [
      [%{id: "meal|noun|1", norm: "meal", pos: "noun",
          definition: "Food prepared and eaten."}],
      [
        %{id: "meal|noun|1", norm: "meal", pos: "noun",
          definition: "Food prepared and eaten."},
        %{id: "meal|noun|1", norm: "meal", pos: "noun",
          definition: "Food prepared and eaten."}
      ],
      [
        %{id: "meal|noun|1", norm: "meal", pos: "noun",
          definition: "An invented different gloss."},
        %{id: "meal|noun|2", norm: "meal", pos: "noun",
          definition: "Food eaten at lunch."}
      ]
    ] do
      report = RoleSemanticContext.extract(input,
        fetch_noun_senses: fn _ -> wrong end)
      patient = Enum.find(report.contexts, &(&1.role == :patient))
      assert patient.status == :unavailable_or_ambiguous
      assert patient.reason in [
        :incomplete_tied_noun_dictionary_evidence,
        :selected_noun_definition_differs_from_dictionary
      ]
    end
  end

  test "even unanimous cricket noun glosses cannot clear absent utterance restrictions" do
    input =
      si("child", "throws", "throw", "ball",
        "A young person.", "A round ball used in cricket.")
      |> tie_patient("ball")

    noun_loader = fn _ ->
      [
        %{id: "ball|noun|1", norm: "ball", pos: "noun",
          definition: "A round ball used in cricket."},
        %{id: "ball|noun|2", norm: "ball", pos: "noun",
          definition: "A ball used in a game of cricket."}
      ]
    end

    senses = [
      sense("throw", 1, "To hurl an object through the air."),
      sense("throw", 5,
        "(transitive) To deliver a ball by straightening the bowling arm."),
      sense("throw", 6, "To send an error to the computer.")
    ]

    report = SemanticSenseValidator.evaluate(input,
      role_context?: true, fetch_noun_senses: noun_loader,
      fetch_base_senses: fn _ -> senses end)

    assert Enum.any?(report.exclusions, fn excluded ->
      excluded.id == "throw|verb|5" and
        {:missing_restriction_witness, :cricket_bowling} in excluded.exclusions
    end)
    refute Enum.any?(report.eligible_top, &(&1.id == "throw|verb|5"))
    refute report.sense_meaning_verified?
  end

  test "actor-only expansion cannot license unsupported verb meaning" do
    input = si("baker", "cooks", "cook", "meal",
      "A maker of food by heating.", "A regular occasion in the day.")
    senses = [
      sense("cook", 1, "To prepare food by heating."),
      sense("cook", 2, "To smelt ore in heat.")
    ]

    {_off, on} = run(input, senses)
    assert on.status == :abstained
    assert on.proposed_id == nil
    assert Enum.any?(on.eligible_top, fn row ->
      row.id == "cook|verb|1" and row.actor_support > 0.0
    end)
  end
end
