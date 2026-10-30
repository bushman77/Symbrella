defmodule Core.Semantic.CompositionSenseAlignmentTest do
  use ExUnit.Case, async: true

  alias Core.Semantic.Composition.{Event, SenseAlignment}

  @definitions %{
    "on|preposition|1" =>
      "Indicating position or location. — Positioned at the upper surface of, touching from above.",
    "on|preposition|12" =>
      "Denoting physical contact or interaction with an object, such as impact or application of force.",
    "on|preposition|19" =>
      "Dealing with the subject of; about; concerning.",
    "on|preposition|39" =>
      "(especially when numbers of combatants or competitors are specified) Against; in opposition to."
  }

  defp evidence(sense_id) do
    {:ok, event} =
      Event.compose(%{
        actor: "bird", predicate: "sitting",
        preposition: "on", landmark: "fence"
      })

    proof = %{
      kind: :progressive_event_property,
      premises: %{lexical_witnesses: [
        %{role: :preposition, word: "on", sense_id: sense_id}
      ]},
      initial: event.initial,
      steps: event.steps,
      result: event.result,
      result_type: event.result_type,
      event_semantics: event.semantics
    }

    candidate = %{
      id: sense_id, norm: "on", pos: "preposition",
      definition: Map.get(@definitions, sense_id)
    }

    {proof, candidate}
  end

  test "the same valid lambda proof exposes different lexically attested relation families" do
    expected = %{
      "on|preposition|1" => {:family_consistent, :spatial_location},
      "on|preposition|12" => {:different_relation_family, :physical_contact},
      "on|preposition|19" => {:different_relation_family, :discourse_topic},
      "on|preposition|39" => {:different_relation_family, :opposition}
    }

    for {id, {status, family}} <- expected do
      {proof, candidate} = evidence(id)
      assert Event.verify(proof) == :ok

      result = SenseAlignment.audit(proof, candidate)
      assert result.status == status
      assert result.dictionary_gloss_family == family
      assert result.composed_relation == :on
      assert result.composed_relation_family == :spatial_location
      assert result.candidate_matches_witness?
      assert result.formal_proof_valid?
      refute result.verified_intended_sense?
      refute result.modifies_lifg?
    end
  end

  test "unknown definitions remain explicitly unresolved" do
    {proof, candidate} = evidence("on|preposition|1")
    candidate = %{candidate | definition: "An uncategorized use."}

    report = SenseAlignment.audit(proof, candidate)
    assert report.status == :unclassified_dictionary_definition
    assert report.dictionary_gloss_family == nil
  end

  test "a dictionary cell not matching the proof witness cannot authorize alignment" do
    {proof, candidate} = evidence("on|preposition|1")
    different = %{candidate | id: "on|preposition|19"}

    report = SenseAlignment.audit(proof, different)
    assert report.status == :unmatched_dictionary_witness
    refute report.candidate_matches_witness?
  end

  test "invalid beta trace cannot authorize alignment" do
    {proof, candidate} = evidence("on|preposition|1")
    broken = %{proof | steps: []}

    report = SenseAlignment.audit(broken, candidate)
    assert report.status == :invalid_formal_proof
    refute report.formal_proof_valid?
  end
end
