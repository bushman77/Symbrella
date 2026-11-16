defmodule Core.Agency.SemanticReviewTest do
  use ExUnit.Case, async: true

  alias Core.Agency.{SemanticHoldout, SemanticReview}

  defp input_cases do
    %{
      "dataset_id" => "curated-example-batch",
      "cases" => [
        %{
          "case_id" => "case-flying",
          "sentence" => "The bird is flying into the tree.",
          "target_index" => 3,
          "target_word" => "flying"
        },
        %{
          "case_id" => "case-sleeping",
          "sentence" => "The cat is sleeping under the table.",
          "target_index" => 3,
          "target_word" => "sleeping"
        }
      ]
    }
  end

  defp senses("flying") do
    [
      %{id: "flying|verb|1", norm: "flying", pos: "verb",
        definition: "moving through the air", example: "The bird was flying",
        status: "inactive"},
      %{id: "flying|adjective|1", norm: "flying", pos: "adjective",
        definition: "moving quickly", example: nil, status: "inactive"}
    ]
  end

  defp senses("sleeping") do
    [
      %{id: "sleeping|verb|1", norm: "sleeping", pos: "verb",
        definition: "being asleep", example: nil, status: "inactive"},
      %{id: "sleeping|adjective|1", norm: "sleeping", pos: "adjective",
        definition: "asleep", example: nil, status: "inactive"}
    ]
  end

  defp senses(_), do: []

  defp packet do
    assert {:ok, result} = SemanticReview.prepare(input_cases(), &senses/1)
    result
  end

  defp fill(packet, reviewer, choices) do
    blank = SemanticReview.blank_sheet(packet)

    %{blank |
      "reviewer_id" => reviewer,
      "votes" => Enum.map(blank["votes"], fn vote ->
        %{vote | "sense_id" => Map.fetch!(choices, vote["case_id"])}
      end)
    }
  end

  defp approved do
    %{
      "case-flying" => "flying|verb|1",
      "case-sleeping" => "sleeping|adjective|1"
    }
  end

  defp development do
    [SemanticHoldout.utterance_digest("The horse is moving beside the barn.")]
  end

  test "blind packets use exact current dictionary IDs and omit LIFG choices" do
    result = packet()

    assert result["version"] == 1
    assert result["dataset_id"] == "curated-example-batch"
    assert length(result["cases"]) == 2
    first = hd(result["cases"])

    assert first["target_index"] == 3
    assert first["target_word"] == "flying"
    assert Enum.map(first["senses"], & &1["id"]) ==
             ["flying|adjective|1", "flying|verb|1"]
    assert Enum.at(first["senses"], 1)["definition"] == "moving through the air"
    refute inspect(result) =~ "candidate_probs"
    refute inspect(result) =~ "lifg_choices"
    refute inspect(result) =~ "gold_sense_id"

    assert {:ok, shuffled} = SemanticReview.prepare(input_cases(), fn word ->
      senses(word) |> Enum.reverse()
    end)
    assert result == shuffled

    blank = SemanticReview.blank_sheet(result)
    assert blank["packet_sha256"] == SemanticReview.packet_sha256(result)
    assert blank["reviewer_id"] == ""
    assert Enum.all?(blank["votes"], &(is_nil(&1["sense_id"])))
  end

  test "two separate completed reviews freeze a valid holdout manifest" do
    p = packet()
    a = fill(p, "independent-reviewer-1", approved())
    b = fill(p, "independent-reviewer-2", approved())

    assert {:ok, manifest} = SemanticReview.finalize(p, a, b, development())
    assert {:ok, _} = SemanticHoldout.validate_manifest(manifest)
    assert manifest["review_packet_sha256"] == SemanticReview.packet_sha256(p)
    assert length(manifest["review_inventory"]) == 2
    assert :ok == SemanticReview.verify_current_dictionary(manifest, &senses/1)

    [first, second] = manifest["cases"]
    assert first["gold_sense_id"] == "flying|verb|1"
    assert second["gold_sense_id"] == "sleeping|adjective|1"
    assert Enum.map(first["reviewers"], & &1["reviewer_id"]) ==
             ["independent-reviewer-1", "independent-reviewer-2"]
    refute inspect(manifest) =~ "moving through the air"
    refute inspect(manifest) =~ "candidate_probs"
    refute inspect(manifest) =~ "original_lifg_id"
    assert byte_size(SemanticHoldout.seal(manifest)) == 64
  end

  test "two reviewers who disagree cannot silently omit hard cases" do
    p = packet()
    a = fill(p, "reviewer-a", approved())
    b = fill(p, "reviewer-b", Map.put(approved(), "case-flying", "flying|adjective|1"))

    assert SemanticReview.finalize(p, a, b, development()) ==
             {:error, :reviewer_disagreement}
  end

  test "duplicate reviewer IDs and incomplete votes are rejected" do
    p = packet()
    a = fill(p, "reviewer-a", approved())

    assert SemanticReview.finalize(p, a, a, development()) ==
             {:error, :duplicate_reviewer_identity}

    incomplete = %{a | "votes" => tl(a["votes"]), "reviewer_id" => "reviewer-b"}
    assert SemanticReview.finalize(p, a, incomplete, development()) ==
             {:error, :missing_review_votes}

    unknown = fill(p, "reviewer-b", Map.put(approved(), "case-flying", "flying|verb|99"))
    assert SemanticReview.finalize(p, a, unknown, development()) ==
             {:error, :invalid_or_missing_review_vote}
  end

  test "review sheets are bound to one frozen packet" do
    p = packet()
    a = fill(p, "reviewer-a", approved())
    b = fill(p, "reviewer-b", approved())
    modified = %{p | "dataset_id" => "changed"}

    assert SemanticReview.finalize(modified, a, b, development()) ==
             {:error, :review_packet_mismatch}
  end

  test "selected senses must come from the inventory that reviewers actually saw" do
    p = packet()
    a = fill(p, "reviewer-a", approved())
    duplicate = %{hd(a["votes"]) | "case_id" => "case-sleeping"}
    bad = %{a | "votes" => [duplicate | a["votes"]]}

    assert SemanticReview.finalize(p, bad, fill(p, "reviewer-b", approved()),
             development()) == {:error, :missing_review_votes}
  end

  test "changing a dictionary definition or removing a sense invalidates evaluation" do
    p = packet()
    a = fill(p, "reviewer-a", approved())
    b = fill(p, "reviewer-b", approved())
    assert {:ok, manifest} = SemanticReview.finalize(p, a, b, development())

    changed = fn word ->
      if word == "flying" do
        Enum.map(senses(word), fn
          %{id: "flying|verb|1"} = row -> %{row | definition: "changed definition"}
          row -> row
        end)
      else
        senses(word)
      end
    end

    assert SemanticReview.verify_current_dictionary(manifest, changed) ==
             {:error, :dictionary_inventory_changed}

    changed_morphology = fn word ->
      if word == "flying" do
        Enum.map(senses(word), fn
          %{id: "flying|verb|1"} = row ->
            Map.put(row, :semantic_atoms, ["tag:present", "tag:participle"])
          row -> row
        end)
      else
        senses(word)
      end
    end

    assert SemanticReview.verify_current_dictionary(manifest, changed_morphology) ==
             {:error, :dictionary_inventory_changed}
    assert SemanticReview.verify_current_dictionary(manifest, fn _ -> [] end) ==
             {:error, :dictionary_inventory_changed}
    assert SemanticReview.verify_current_dictionary(Map.delete(manifest, "review_inventory"),
             &senses/1) == {:error, :missing_review_inventory}
  end

  test "review inventory must remain bound to its original word and token" do
    p = packet()
    a = fill(p, "reviewer-a", approved())
    b = fill(p, "reviewer-b", approved())
    assert {:ok, manifest} = SemanticReview.finalize(p, a, b, development())

    [first | tail] = manifest["review_inventory"]
    wrong = %{first | "target_word" => "sleeping"}
    tampered = %{manifest | "review_inventory" => [wrong | tail]}

    assert SemanticReview.verify_current_dictionary(tampered, &senses/1) ==
             {:error, :invalid_review_inventory}
  end

  test "development overlap and missing development provenance fail validation" do
    p = packet()
    a = fill(p, "reviewer-a", approved())
    b = fill(p, "reviewer-b", approved())
    assert SemanticReview.finalize(p, a, b, []) ==
             {:error, :invalid_development_hashes}

    hash = SemanticHoldout.utterance_digest("The bird is flying into the tree.")
    assert SemanticReview.finalize(p, a, b, [hash]) ==
             {:error, :development_overlap_detected}
  end

  test "bad token indexes, mismatched words and insufficient inventories are refused" do
    [%{} = first, second] = input_cases()["cases"]
    wrong_word = %{first | "target_word" => "cat"}
    assert SemanticReview.prepare(%{input_cases() | "cases" => [wrong_word, second]},
             &senses/1) == {:error, :target_index_word_mismatch}

    bad_index = %{first | "target_index" => 5}
    assert SemanticReview.prepare(%{input_cases() | "cases" => [bad_index, second]},
             &senses/1) == {:error, :target_index_word_mismatch}

    assert SemanticReview.prepare(input_cases(), fn _ -> [%{}] end) ==
             {:error, :insufficient_or_oversized_sense_inventory}
    assert SemanticReview.prepare(input_cases(), fn _ -> raise "db offline" end) ==
             {:error, :dictionary_lookup_failed}
    assert SemanticReview.prepare(%{}, &senses/1) ==
             {:error, :invalid_source_cases}
  end
end
