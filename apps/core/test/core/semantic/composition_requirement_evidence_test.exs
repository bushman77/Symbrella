defmodule Core.Semantic.CompositionRequirementEvidenceTest do
  use ExUnit.Case, async: true

  alias Core.Semantic.Composition.RequirementEvidence

  defp context(noun_definition, predicate_definition) do
    %{
      object: %{
        chosen_id: "testbank|noun|1",
        dictionary_candidates: [
          %{id: "testbank|noun|1", definition: noun_definition}
        ]
      },
      predicate: %{
        chosen_id: "sat|noun|1",
        dictionary_candidates: [
          %{id: "sat|verb|1", definition: predicate_definition}
        ]
      }
    }
  end

  defp denotation(requirements), do: %{required_context: requirements}

  test "an attested geographical noun gloss witnesses geography, not the complete spatial relation" do
    input = context("An edge of river, lake, or other watercourse.",
      "simple past and past participle of sit")
    report = RequirementEvidence.audit(input, denotation([:geographic_landmark]))

    assert report.status == :all_requirements_have_candidate_witnesses
    assert [%{requirement: :geographic_landmark, status: :candidate_witness,
              source_sense_id: "testbank|noun|1"}] = report.requirement_checks
    assert report.predicate_form_of.attested_form_of_lemma == "sit"
    assert report.predicate_form_of.morphological_only?
    assert report.predicate_selected_sense_id == "sat|noun|1"
    refute report.semantic_compatibility_proven?
    refute report.modifies_lifg?
  end

  test "an artificial barrier gloss does not by itself establish physical support" do
    input = context("A thin artificial barrier separating two pieces of land.",
      "present participle and gerund of sit")
    report = RequirementEvidence.audit(input, denotation([:supporting_surface]))

    assert report.status == :unresolved
    assert [%{status: :unresolved, supporting_text: nil}] = report.requirement_checks
    assert report.predicate_form_of.attested_form_of_lemma == "sit"
    refute report.any_incompatibility_proven?
  end

  test "lack of evidence for transport or experiencer never becomes contradiction" do
    input = context("An edge of river.", "simple past and past participle of sit")
    for requirements <- [[:transport_landmark], [:experiencer]] do
      report = RequirementEvidence.audit(input, denotation(requirements))
      assert report.status == :unresolved
      refute report.any_incompatibility_proven?
      refute report.semantic_compatibility_proven?
    end
  end

  test "a multi-requirement candidate stays partially supported and unresolved" do
    input = context("A passenger vehicle and mode of transport.",
      "simple past and past participle of sit")
    report =
      RequirementEvidence.audit(input,
        denotation([:transport_landmark, :topic_bearing_predicate]))

    assert report.status == :some_requirements_have_candidate_witnesses
    assert Enum.map(report.requirement_checks, & &1.status) ==
             [:candidate_witness, :unresolved]
    refute report.semantic_compatibility_proven?
  end

  test "requirements are checked from gloss evidence rather than numeric ID" do
    original = context("An edge of river.", "simple past and past participle of sit")
    report = RequirementEvidence.audit(original, denotation([:geographic_landmark]))
    changed = put_in(original, [:object, :chosen_id], "testbank|noun|2")

    missing = RequirementEvidence.audit(changed, denotation([:geographic_landmark]))
    assert report.status == :all_requirements_have_candidate_witnesses
    assert missing.status == :unresolved
    assert missing.object_selected_sense_id == nil
  end
end
