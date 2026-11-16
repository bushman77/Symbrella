defmodule Core.Agency.SemanticHoldoutTest do
  use ExUnit.Case, async: true

  alias Core.Agency.SemanticHoldout, as: Holdout
  alias Core.Semantic.Composition
  alias Core.Syntax.{Signatures, StructuralCompetition}

  defp fixture(sentence) do
    words =
      sentence
      |> String.trim_trailing(".")
      |> String.split(" ", trim: true)

    ["The", actor, "is", predicate, _prep, "the", _landmark] = words
    positions = ~w(determiner noun verb verb preposition determiner noun)

    cells =
      words
      |> Enum.zip(positions)
      |> Enum.map(fn {word, pos} ->
        norm = String.downcase(word)
        %{
          id: "#{norm}|#{pos}|1",
          norm: norm,
          pos: pos,
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
              adjective.id => 0.70,
              "#{predicate}|verb|1" => 0.30
            }
          }
        else
          %{token_index: index, id: cell.id}
        end
      end)

    %Core.SemanticInput{
      source: :test,
      sentence: sentence,
      tokens: Enum.with_index(words, fn word, index ->
        %{phrase: word, index: index, n: 1}
      end),
      active_cells: [adjective | cells],
      lifg_choices: choices
    }
    |> Signatures.attach()
    |> StructuralCompetition.attach()
    |> Composition.attach()
  end

  defp case_data(case_id, sentence, sense_id) do
    %{
      "case_id" => case_id,
      "sentence" => sentence,
      "target_index" => 3,
      "gold_sense_id" => sense_id,
      "split" => "holdout",
      "reviewers" => [
        %{"reviewer_id" => "reviewer-a", "sense_id" => sense_id},
        %{"reviewer_id" => "reviewer-b", "sense_id" => sense_id}
      ]
    }
  end

  defp manifest do
    %{
      "version" => 1,
      "dataset_id" => "synthetic-holdout-fixture-v1",
      "development_utterance_sha256" => [
        Holdout.utterance_digest("The dog is sleeping beside the tree.")
      ],
      "cases" => [
        case_data("case-1", "The bird is flying into the tree.", "flying|verb|1"),
        case_data("case-2", "The cat is sleeping under the table.", "sleeping|adjective|1")
      ]
    }
  end

  test "synthetic two-reviewer fixture evaluates improvements and regressions but not truth" do
    dataset = manifest()
    before = dataset
    seal = Holdout.seal(dataset)

    result = Holdout.run(dataset, seal, &fixture/1)

    assert result.status == :evaluated_against_supplied_reviews
    assert result.dataset_id == "synthetic-holdout-fixture-v1"
    assert result.manifest_sha256 == seal
    assert result.report.status == :benchmark_summary
    assert result.report.total == 2
    assert result.report.compared == 2
    assert result.report.abstentions == 0
    assert result.report.wrong_to_right == 1
    assert result.report.right_to_wrong == 1
    assert result.report.net_label_agreement_gain == 0
    assert Enum.map(result.cases, & &1.outcome) == [:wrong_to_right, :right_to_wrong]
    refute result.external_reviewer_identity_verified?
    refute result.actual_training_isolation_verified?
    refute result.semantic_truth_verified?
    refute result.learning_applied?
    refute result.policy_change_authorized?
    assert dataset == before
  end

  test "the sealed digest rejects any changed reviewed label before invoking the resolver" do
    original = manifest()
    locked = Holdout.seal(original)
    [first | rest] = original["cases"]
    forged = %{first | "gold_sense_id" => "flying|adjective|1"}
    edited = %{original | "cases" => [forged | rest]}

    result = Holdout.run(edited, locked, fn _ -> flunk("resolver must not be called") end)
    assert result.status == :rejected
    assert result.reason == :manifest_seal_mismatch
    refute result.policy_change_authorized?
  end

  test "rejects declared development leakage and simple paraphrase-normalization duplicates" do
    data = manifest()
    first = hd(data["cases"])
    training = [Holdout.utterance_digest(first["sentence"])]
    contaminated = %{data | "development_utterance_sha256" => training}
    assert Holdout.validate_manifest(contaminated) == {:error, :development_overlap_detected}

    duplicate = %{first |
      "case_id" => "case-other", "sentence" => "  THE   BIRD is flying into the tree. "}
    repeated = %{data | "cases" => [first, duplicate]}

    assert Holdout.validate_manifest(repeated) ==
             {:error, :duplicate_case_or_utterance}
  end

  test "disagreement or a single reviewer fails before any comparison" do
    data = manifest()
    [first | rest] = data["cases"]

    [one, two] = first["reviewers"]
    disagree = %{first | "reviewers" => [one, %{two | "sense_id" => "flying|adjective|1"}]}
    assert Holdout.validate_manifest(%{data | "cases" => [disagree | rest]}) ==
             {:error, :reviewer_disagreement}

    one_review = %{first | "reviewers" => [one]}
    assert Holdout.validate_manifest(%{data | "cases" => [one_review | rest]}) ==
             {:error, :invalid_case}

    duplicate_reviewer = %{first | "reviewers" => [one, %{two | "reviewer_id" => "reviewer-a"}]}
    assert Holdout.validate_manifest(%{data | "cases" => [duplicate_reviewer | rest]}) ==
             {:error, :invalid_reviewer_declarations}
  end

  test "rejects training-split and duplicate-case identity declarations" do
    data = manifest()
    [first, second] = data["cases"]

    training = %{first | "split" => "training"}
    assert Holdout.validate_manifest(%{data | "cases" => [training, second]}) ==
             {:error, :invalid_case}

    repeated_id = %{second | "case_id" => first["case_id"]}
    assert Holdout.validate_manifest(%{data | "cases" => [first, repeated_id]}) ==
             {:error, :duplicate_case_or_utterance}
  end

  test "a failed resolver abstains, never claiming candidate accuracy" do
    data = manifest()
    result = Holdout.run(data, Holdout.seal(data), fn sentence ->
      if String.contains?(sentence, "bird"), do: fixture(sentence), else: raise("unavailable")
    end)

    assert result.status == :evaluated_against_supplied_reviews
    assert result.report.total == 2
    assert result.report.compared == 1
    assert result.report.abstentions == 1
    assert result.report.wrong_to_right == 1
    assert Enum.at(result.cases, 1).reason == :resolver_failed_or_sentence_changed
    refute result.learning_applied?
  end

  test "a resolver cannot quietly change the sentence associated with the reviewed label" do
    data = manifest()
    result = Holdout.run(data, Holdout.seal(data), fn _sentence ->
      fixture("The bird is flying into the tree.")
    end)

    assert result.report.compared == 1
    assert result.report.abstentions == 1
    assert Enum.at(result.cases, 1).status == :abstained
  end

  test "valid seal still rejects a missing development reference" do
    data = %{manifest() | "development_utterance_sha256" => []}
    result = Holdout.run(data, Holdout.seal(data), &fixture/1)

    assert result.status == :rejected
    assert result.reason == :invalid_development_hashes
  end

  test "case count is bounded and unsupported arguments are rejected" do
    data = manifest()
    one = hd(data["cases"])
    excessive =
      %{data | "cases" => Enum.map(1..129, fn i ->
        %{one | "case_id" => "many-#{i}"}
      end)}

    assert Holdout.validate_manifest(excessive) == {:error, :invalid_case_count}
    assert Holdout.run(nil, nil, nil).status == :rejected
    assert Holdout.utterance_digest(nil) == nil
    assert byte_size(Holdout.seal(data)) == 64
  end
end
