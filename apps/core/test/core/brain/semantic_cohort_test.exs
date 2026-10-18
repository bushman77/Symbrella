defmodule Core.Brain.SemanticCohortTest do
  use ExUnit.Case, async: false

  alias Core.Brain.{SemanticCohort, SemanticSenseShadow}
  alias Core.Semantic.{EpisodicProjection, SentenceMeaning}

  @evidence [:contiguous_surface, :attested_nominal_subject,
             :attested_nominal_object, :attested_finite_present_verb]
  @inflection ["tag:form-of", "tag:indicative", "tag:present",
               "tag:singular", "tag:third-person"]

  defp interpreted do
    choices = [
      %{token_index: 1, id: "gardener|noun|1",
        definition: "Someone who tends a garden.",
        candidate_probs: %{"gardener|noun|1" => 1.0}},
      %{token_index: 2, id: "plants|verb|1"},
      %{token_index: 4, id: "tree|noun|1",
        definition: "A perennial woody plant or shrub.",
        candidate_probs: %{"tree|noun|1" => 0.5,
                           "tree|noun|2" => 0.5}}
    ]

    %{
      sentence: "The gardener plants a tree.",
      source: :user,
      intent: :statement,
      session_id: "cohort-isolated-fixture",
      trace: [],
      tokens: [
        %{index: 0, phrase: "The", n: 1},
        %{index: 1, phrase: "gardener", n: 1},
        %{index: 2, phrase: "plants", n: 1},
        %{index: 3, phrase: "a", n: 1},
        %{index: 4, phrase: "tree.", n: 1}
      ],
      sentence_signatures: [
        %{id: :finite_present_svo_candidate, source: :core_syntax,
          span: {0, 4}, evidence: @evidence,
          roles: %{subject_determiner: 0, subject_head: 1,
            predicate: 2, object_determiner: 3, object_head: 4}}
      ],
      active_cells: [
        %{id: "gardener|noun|1", pos: "noun", norm: "gardener",
          definition: "Someone who tends a garden."},
        %{id: "tree|noun|1", pos: "noun", norm: "tree",
          definition: "A perennial woody plant or shrub."},
        %{id: "plants|verb|1", pos: "verb", norm: "plants",
          definition: "An inflected form of plant.",
          semantic_atoms: @inflection}
      ],
      lifg_choices: choices,
      semantic_choices: choices,
      atl_slate: %{winners: choices},
      response_text: "Untouched original response"
    }
    |> EpisodicProjection.attach()
    |> SentenceMeaning.attach()
  end

  defp row(id, gloss, opts \\ []) do
    match = Keyword.get(opts, :matched, ["plant"])
    role = Keyword.get(opts, :role, :patient)

    %{
      id: id,
      eligible?: true,
      exclusions: [], missing_restrictions: [],
      grammar: :compatible_direct_object,
      cohort_gloss: gloss,
      combined_diagnostic_score: Keyword.get(opts, :score, 0.1015),
      lexical_score: Keyword.get(opts, :direct, 0.0),
      patient_support: if(role == :patient, do: 0.1015, else: 0.0),
      actor_support: if(role == :actor, do: 0.0435, else: 0.0),
      matching_context: Keyword.get(opts, :literal, []),
      one_hop_witnesses: [
        %{role: role, matched: match, contribution: 0.1015,
          provenance: :unanimous_tied_dictionary_glosses}
      ]
    }
  end

  defp report(rows), do: %{
    status: :abstained,
    attested_base_lexeme: "plant",
    original_surface_id: "plants|verb|1",
    cohort_evidence_rows: rows
  }

  test "two senses sharing a patient witness form a cohort but no exact sense wins" do
    si = interpreted()
    before = si

    result = SemanticCohort.evaluate(si, report([
      row("plant|verb|1",
        "(transitive) To put a plant or seed in the soil to grow."),
      row("plant|verb|2",
        "(transitive) To supply an area with plants and vegetation.")
    ]))

    assert result.status == :candidate_cohorts
    assert result.source_frame == :finite_present_svo
    assert result.participant_surfaces.actor == "the gardener"
    assert result.cohort_count == 1
    assert [%{anchor_term: "plant", evidence_origin: :patient_gloss,
              member_count: 2, exact_sense_unresolved?: true} = cohort] =
      result.cohorts
    assert cohort.minimum_member_support == 0.1015
    assert Enum.sort(Enum.map(cohort.members, & &1.id)) ==
             ["plant|verb|1", "plant|verb|2"]
    assert Enum.any?(cohort.members,
      &("soil" in &1.distinguishing_definition_terms))
    assert cohort.shared_meaning_verified? == false
    assert result.semantic_equivalence_verified? == false
    assert result.chose_exact_sense? == false
    assert result.modified_lifg? == false
    assert result.modified_synapses? == false
    assert result.changed_response? == false
    assert si == before
    assert is_binary(Jason.encode!(result))
  end

  test "gloss resemblance without the same sentence witness cannot create a cohort" do
    si = interpreted()

    result = SemanticCohort.evaluate(si, report([
      row("plant|verb|1", "To put a plant in soil.",
        matched: ["plant"]),
      row("plant|verb|2", "To put a plant in a building.",
        matched: ["building"])
    ]))

    assert result.status == :no_supported_cohort
    assert result.cohort_count == 0
    assert result.cohorts == []
  end

  test "no transitive bridge joins senses without the exact shared anchor" do
    si = interpreted()

    rows = [
      row("plant|verb|1", "To put a plant in the soil.",
        matched: ["plant"]),
      row("plant|verb|2", "To put a plant in the soil.",
        matched: ["plant", "soil"]),
      row("plant|verb|3", "To till soil and prepare a field.",
        matched: ["soil"])
    ]

    result = SemanticCohort.evaluate(si, report(rows))
    assert result.cohort_count == 2
    assert Enum.any?(result.cohorts, fn c ->
      c.anchor_term == "plant" and
        Enum.sort(Enum.map(c.members, & &1.id)) ==
          ["plant|verb|1", "plant|verb|2"]
    end)
    assert Enum.any?(result.cohorts, fn c ->
      c.anchor_term == "soil" and
        Enum.sort(Enum.map(c.members, & &1.id)) ==
          ["plant|verb|2", "plant|verb|3"]
    end)
    refute Enum.any?(result.cohorts, &(&1.member_count == 3))
  end

  test "cohorts inspect the full eligible sense slate rather than a top-three display" do
    si = interpreted()

    # The source validator provides all eligible rows to this module.
    # These 14 weaker entries cannot create shared cohorts.
    weak =
      for index <- 1..14 do
        row("plant|verb|#{index + 20}",
          "To operate a completely unrelated mechanism.",
          matched: [], score: 0.0)
      end

    rows = weak ++ [
      row("plant|verb|91", "To put a plant in soil."),
      row("plant|verb|92", "To supply an area with plants.")
    ]
    result = SemanticCohort.evaluate(si, report(rows))
    assert result.eligible_sense_count == 16
    assert result.cohort_count == 1
    assert Enum.sort(Enum.map(hd(result.cohorts).members, & &1.id)) ==
             ["plant|verb|91", "plant|verb|92"]
  end

  test "independent direct sentence anchors can form a group without noun expansion" do
    si = interpreted()
    rows = [
      row("plant|verb|1", "To put a plant in soil.",
        literal: ["plant"], matched: [], direct: 0.29, score: 0.29),
      row("plant|verb|2", "To establish a plant on a property.",
        literal: ["plant"], matched: [], direct: 0.29, score: 0.29)
    ]

    result = SemanticCohort.evaluate(si, report(rows))
    assert [%{evidence_origin: :direct_sentence, anchor_term: "plant"}] =
      result.cohorts
  end

  test "ineligible specialist sense never enters a cohort even with shared words" do
    si = interpreted()

    # A real validator excludes this before cohort construction; also
    # reject any incorrectly shaped injection at the independent boundary.
    excluded = row("plant|verb|99", "To plant a cricket ball.")
      |> Map.put(:eligible?, false)

    assert SemanticCohort.evaluate(si,
      report([row("plant|verb|1", "To place a plant in soil."), excluded])
      ).status == :skipped

    # Even a malformed "eligible" row carrying a restriction cannot be
    # passed through by a later accidental caller.
    disguised =
      row("plant|verb|99", "To plant a cricket ball.")
      |> Map.put(:missing_restrictions, [:cricket_bowling])
      |> Map.put(:exclusions,
        [{:missing_restriction_witness, :cricket_bowling}])

    assert SemanticCohort.evaluate(si,
      report([row("plant|verb|1", "To place a plant in soil."), disguised])
      ).status == :skipped
  end

  test "unvalidated questions and assistant-origin sentences never form cohorts" do
    si = interpreted()
    evidence = report([
      row("plant|verb|1", "To place a plant in soil."),
      row("plant|verb|2", "To establish a plant in an area.")
    ])

    assert SemanticCohort.evaluate(%{si | source: :assistant}, evidence).status ==
             :skipped
    assert SemanticCohort.evaluate(%{si | sentence_meaning: nil}, evidence).status ==
             :skipped
    assert SemanticCohort.evaluate(%{si | sentence: "Who plants a tree?"}, evidence).status ==
             :skipped
  end

  test "live shadow cohorts share one validator call and preserve all prior Core fields" do
    si = interpreted()
    fetch_calls = :atomics.new(1, [])

    verb_senses = [
      %{id: "plant|verb|1", norm: "plant", pos: "verb",
        definition: "To put a plant in soil to grow.",
        gram_function: ["transitive"]},
      %{id: "plant|verb|2", norm: "plant", pos: "verb",
        definition: "To supply an area with plants.",
        gram_function: ["transitive"]},
      %{id: "plant|verb|3", norm: "plant", pos: "verb",
        definition: "To hide evidence at a scene.",
        gram_function: ["transitive"]}
    ]

    noun_senses = [
      %{id: "tree|noun|1", norm: "tree", pos: "noun",
        definition: "A perennial woody plant or shrub."},
      %{id: "tree|noun|2", norm: "tree", pos: "noun",
        definition: "A woody plant with a trunk."}
    ]

    out = SemanticSenseShadow.attach(si,
      fetch_base_senses: fn "plant" ->
        :atomics.add_get(fetch_calls, 1, 1)
        verb_senses
      end,
      fetch_noun_senses: fn "tree" -> noun_senses end)

    assert :atomics.get(fetch_calls, 1) == 1
    previous_trace = si.trace
    assert [%{stage: :semantic_sense_shadow, meta: meta} | ^previous_trace] =
      out.trace
    assert meta.semantic_cohort.status == :candidate_cohorts
    assert meta.semantic_cohort.cohort_count >= 1
    assert meta.semantic_cohort.changed_response? == false
    assert is_binary(Jason.encode!(meta))
    assert Map.delete(out, :trace) == Map.delete(si, :trace)
    assert out.lifg_choices == si.lifg_choices
    assert out.atl_slate == si.atl_slate
    assert out.response_text == si.response_text

    disabled = SemanticSenseShadow.attach(si,
      semantic_cohort_shadow?: false,
      fetch_base_senses: fn _ -> verb_senses end)
    assert hd(disabled.trace).meta.semantic_cohort.status == :disabled
  end
end
