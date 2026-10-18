defmodule Core.Response.SemanticRoleFidelityTest do
  use ExUnit.Case, async: true

  alias Core.Response.SemanticRoleFidelity

  # Production builds this map only after independent SVO, LIFG and ATL
  # revalidation. The module consumes it without learning from LLM text.
  defp frame(actor \\ "the baker", verb \\ "cooks",
      base \\ "cook", patient \\ "a meal") do
    %{
      kind: :user_reported_action,
      evidence_level: :structurally_interpreted_not_verified,
      sentence: "#{String.capitalize(actor)} #{verb} #{patient}.",
      actor: actor,
      predicate_surface: verb,
      predicate_lemma: base,
      patient: patient,
      event_occurrence_verified?: false,
      exact_verb_sense_verified?: false
    }
  end

  test "the witnessed previous-action substitution is a high-confidence structural mismatch" do
    source = frame()
    draft = "The baker is also planting a tree, which is a tree in a garden."

    result = SemanticRoleFidelity.audit(source.sentence, draft, source)
    assert result.status == :role_substitution_detected
    assert result.mismatched_roles == [:predicate, :patient]
    assert result.source == :revalidated_sentence_roles
    assert result.draft_scope == :standalone_main_clause
    assert result.explicitly_claims_additional_action?
    assert result.event_truth_verified? == false
    assert result.exact_sense_verified? == false
    assert result.modified_cognition? == false
    refute Map.has_key?(result, :response_text)
  end

  test "detector uses source role bindings rather than the example word identities" do
    source = frame("the gardener", "plants", "plant", "a tree")
    report = SemanticRoleFidelity.audit(
      source.sentence, "The gardener is also cooking a meal.", source)

    assert report.status == :role_substitution_detected
    assert report.mismatched_roles == [:predicate, :patient]
  end

  test "explicit actor/patient reversal is detected without a new verb sense" do
    source = frame()
    report = SemanticRoleFidelity.audit(source.sentence,
      "A meal cooks the baker.", source)

    assert report.status == :role_substitution_detected
    assert report.mismatched_roles == [:actor, :patient]
  end

  test "ordinary synonyms and one-role changes stay diagnostic only" do
    source = frame()

    for draft <- [
      "The baker prepares dinner.",
      "The baker makes a supper."
    ] do
      report = SemanticRoleFidelity.audit(source.sentence, draft, source)
      assert report.status == :possible_role_drift
      assert report.mismatched_roles == [:predicate, :patient]
    end

    report = SemanticRoleFidelity.audit(source.sentence,
      "The baker cooks a dinner.", source)
    assert report.status == :no_high_confidence_role_drift
    assert report.mismatched_roles == [:patient]

    assert SemanticRoleFidelity.audit(source.sentence,
      "The baker cooks a meal.", source).status ==
      :no_high_confidence_role_drift

    assert SemanticRoleFidelity.audit(source.sentence,
      "The baker is also cooking a meal.", source).status ==
      :no_high_confidence_role_drift
  end

  test "ambiguous or explanatory drafts are not rewritten from lexical similarities" do
    source = frame()

    for draft <- [
      "The baker also prepares a meal.",
      "You're describing the baker cooking a meal.",
      "The baker is also planting a tree, but I cannot infer why.",
      "The baker is also planting a tree. This is a different event.",
      "I wonder whether the baker is also planting a tree.",
      "Previously you wrote: The gardener plants a tree.",
      "\"The baker is also planting a tree.\"",
      "The baker is also planting a tree\nand cooks a meal.",
      "The baker is the one who cooks the meal, not the gardener.",
      "The baker has planted a tree."
    ] do
      assert SemanticRoleFidelity.audit(source.sentence, draft, source).status ==
        :not_adjudicated
    end
  end

  test "the current sentence and authentic source frame are required" do
    source = frame()
    draft = "The baker is also planting a tree."

    assert SemanticRoleFidelity.audit(
      "The baker cooked a meal.", draft, source).status == :not_adjudicated

    assert SemanticRoleFidelity.audit(source.sentence, draft,
      %{source | kind: :untrusted}).status == :not_adjudicated

    assert SemanticRoleFidelity.audit(source.sentence, draft,
      %{source | event_occurrence_verified?: true}).status == :not_adjudicated

    assert SemanticRoleFidelity.audit(source.sentence, draft, nil).status ==
      :not_adjudicated
  end
end
