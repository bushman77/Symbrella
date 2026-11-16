defmodule Core.Agency.SemanticCorrectionEvaluatorTest do
  use ExUnit.Case, async: true

  alias Core.Agency.SemanticCorrectionEvaluator, as: Evaluator
  alias Core.Agency.LexicalRoleShadow
  alias Core.Semantic.Composition
  alias Core.Syntax.{Signatures, StructuralCompetition}

  defp input(actor \\ "bird", predicate \\ "flying",
             preposition \\ "into", landmark \\ "tree") do
    words = ["The", actor, "is", predicate, preposition, "the", landmark]
    pos = ~w(determiner noun verb verb preposition determiner noun)

    cells =
      words
      |> Enum.zip(pos)
      |> Enum.map(fn {word, part_of_speech} ->
        normalized = String.downcase(word)

        %{
          id: "#{normalized}|#{part_of_speech}|1",
          norm: normalized,
          pos: part_of_speech,
          semantic_atoms:
            if(word == predicate, do: ["tag:form-of", "tag:present", "tag:participle"],
              else: [])
        }
      end)

    adjective = %{
      id: "#{predicate}|adjective|1",
      norm: predicate,
      pos: "adjective"
    }

    choices =
      cells
      |> Enum.with_index()
      |> Enum.map(fn {cell, index} ->
        if index == 3 do
          %{
            token_index: index,
            id: adjective.id,
            candidate_probs: %{
              adjective.id => 0.7,
              "#{predicate}|verb|1" => 0.3
            }
          }
        else
          %{token_index: index, id: cell.id}
        end
      end)

    %Core.SemanticInput{
      source: :test,
      sentence: Enum.join(words, " ") <> ".",
      tokens: Enum.with_index(words, fn word, index ->
        %{phrase: word, index: index, n: 1}
      end),
      active_cells: [adjective | cells],
      lifg_choices: choices
    }
    |> Signatures.attach()
    |> StructuralCompetition.attach()
    |> Composition.attach()
    |> LexicalRoleShadow.attach()
  end

  defp reference(si, gold_id, case_id \\ "fixture-001") do
    %{
      case_id: case_id,
      split: :holdout,
      annotation_source: :synthetic_fixture,
      annotation_id: "synthetic-expert-label",
      utterance_sha256: Evaluator.utterance_sha256(si),
      token_index: 3,
      gold_sense_id: gold_id
    }
  end

  test "live semantic evaluator records a provisional candidate without claiming verification" do
    si = input()
    original = si.lifg_choices
    proof = si.compositional_derivation

    audited = Evaluator.attach(si, gold_sense_id: "flying|verb|1")

    assert %{decision: :awaiting_reference,
             reason: :independent_label_required, meta: meta} =
      Enum.find(audited.trace, &match?(%{stage: :semantic_correction_evaluation}, &1))

    assert meta.candidate_count == 1
    assert meta.reference_status == :not_provided
    refute meta.baseline_vs_candidate_compared?
    refute meta.independently_verified?
    refute meta.learning_applied?
    refute meta.policy_change_authorized?
    assert audited.lifg_choices == original
    assert audited.compositional_derivation == proof
    assert :ok == Core.SemanticInput.validate_contract(audited)
    refute inspect(meta) =~ "flying|verb|1"
  end

  test "an exact candidate sense matching a supplied fixture label counts as wrong-to-right" do
    si = input()
    before = si

    comparison = Evaluator.compare(si, reference(si, "flying|verb|1"))

    assert comparison.status == :compared_to_supplied_label
    assert comparison.outcome == :wrong_to_right
    assert comparison.baseline_sense_id == "flying|adjective|1"
    assert comparison.candidate_sense_id == "flying|verb|1"
    assert comparison.candidate_matches_label?
    refute comparison.baseline_matches_label?
    refute comparison.label_authenticated?
    refute comparison.semantic_truth_verified?
    refute comparison.learning_applied?
    assert si == before
  end

  test "a proposed alternative that disagrees with the reference is a regression" do
    si = input()
    comparison = Evaluator.compare(si, reference(si, "flying|adjective|1"))

    assert comparison.status == :compared_to_supplied_label
    assert comparison.outcome == :right_to_wrong
    assert comparison.baseline_matches_label?
    refute comparison.candidate_matches_label?
    refute comparison.policy_change_authorized?
  end

  test "reference hash, split, annotation provenance and inventory are mandatory" do
    si = input()
    valid = reference(si, "flying|verb|1")

    assert Evaluator.compare(si, %{valid | utterance_sha256: String.duplicate("0", 64)}).reason ==
             :reference_sentence_mismatch
    assert Evaluator.compare(si, %{valid | split: :training}).reason ==
             :non_holdout_reference
    assert Evaluator.compare(si, %{valid | annotation_id: ""}).reason ==
             :missing_annotation_provenance
    assert Evaluator.compare(si, %{valid | annotation_source: :self_reported}).reason ==
             :missing_annotation_provenance
    assert Evaluator.compare(si, %{valid | gold_sense_id: "nonexistent|verb|999"}).reason ==
             :reference_not_supported_by_current_evidence
    assert Evaluator.compare(si, %{valid | token_index: 6}).reason ==
             :reference_not_supported_by_current_evidence
  end

  test "missing score evidence cannot be turned into a benchmark correction" do
    si = input()
    broken = %{si | lifg_choices: Enum.map(si.lifg_choices, fn
      %{token_index: 3} = choice -> Map.delete(choice, :candidate_probs)
      choice -> choice
    end)}

    comparison = Evaluator.compare(broken, reference(broken, "flying|verb|1"))
    assert comparison.status == :abstained
    assert comparison.outcome == :not_scored
    assert comparison.reason == :no_unique_exact_sense_alternative
  end

  test "a second role conflict prevents single-sense evaluation" do
    si = input()

    changed = %{si | lifg_choices: Enum.map(si.lifg_choices, fn
      %{token_index: 6} = choice -> %{choice | id: "tree|proper_noun|1"}
      choice -> choice
    end)}
    |> StructuralCompetition.attach()
    |> Composition.attach()

    comparison = Evaluator.compare(changed, reference(changed, "flying|verb|1"))
    assert comparison.status == :abstained
    assert comparison.reason == :no_unique_exact_sense_alternative
    refute comparison.learning_applied?
  end

  test "a benchmark summarizes improvements, regressions and abstentions separately" do
    first = input()
    second = input("cat", "sleeping", "under", "table")

    good = Evaluator.compare(first, reference(first, "flying|verb|1", "case-1"))
    regression =
      Evaluator.compare(second, reference(second, "sleeping|adjective|1", "case-2"))
    abstain = Evaluator.compare(first, %{reference(first, "flying|verb|1") |
      utterance_sha256: String.duplicate("0", 64)})

    summary = Evaluator.aggregate([good, regression, abstain])

    assert summary.status == :benchmark_summary
    assert summary.total == 3
    assert summary.compared == 2
    assert summary.abstentions == 1
    assert summary.baseline_matches_supplied_labels == 1
    assert summary.candidate_matches_supplied_labels == 1
    assert summary.wrong_to_right == 1
    assert summary.right_to_wrong == 1
    assert summary.both_wrong == 0
    assert summary.net_label_agreement_gain == 0
    assert summary.reference_authentication == :not_established
    refute summary.semantic_truth_verified?
    refute summary.learning_applied?
  end

  test "duplicate cases and fabricated outcome reports are not counted" do
    si = input()
    report = Evaluator.compare(si, reference(si, "flying|verb|1", "duplicate"))

    assert Evaluator.aggregate([report, report]).status == :invalid_reports
    assert Evaluator.aggregate([report, %{report | case_id: "different-id"}]).status ==
             :invalid_reports

    forged = %{report | outcome: :right_to_wrong}
    assert Evaluator.aggregate([forged]).status == :invalid_reports

    assert Evaluator.aggregate(List.duplicate(report, 129)).status == :invalid_reports
  end

  test "unsupported inputs and missing structure abstain without activating learning" do
    assert Evaluator.compare(nil, nil).status == :abstained
    assert Evaluator.aggregate(%{}).status == :invalid_reports

    si = input()
    tampered = %{si | structural_competition: %{status: :preferred}}
    report = Evaluator.compare(tampered, reference(tampered, "flying|verb|1"))

    assert report.status == :abstained
    refute report.label_authenticated?
    refute report.policy_change_authorized?
  end
end
