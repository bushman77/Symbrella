defmodule Db.JSONL.DefinitionReviewGateTest do
  use ExUnit.Case, async: true

  alias Db.JSONL.{DefinitionOnlyBackfill, DefinitionReviewGate}

  defp plan do
    source = [
      %Db.BrainCell{
        id: "point|noun|1",
        word: "point",
        norm: "point",
        pos: "noun",
        definition: "A small mark. — A tiny spot.",
        example: "A tiny point.",
        synonyms: [],
        antonyms: [],
        gram_function: ["countable"],
        semantic_atoms: ["pos_raw:noun", "cat:en:mathematics"]
      }
    ]

    first = hd(source)

    stored = [
      %{first |
        definition: "A small mark.",
        semantic_atoms: ["pos_raw:noun"],
        status: "active",
        activation: 0.9
      }
    ]

    {:ok, plan} = DefinitionOnlyBackfill.plan("point", "noun", source, stored)
    plan
  end

  test "accepts only exact word, POS, sense count, patch count, and fingerprint" do
    plan = plan()

    assert String.length(plan.fingerprint) == 64
    assert :ok =
             DefinitionReviewGate.validate(
               plan, "point", "noun", 1, 1, plan.fingerprint
             )

    assert {:error, :unexpected_word_or_pos} =
             DefinitionReviewGate.validate(plan, "point", "verb", 1, 1, plan.fingerprint)

    assert {:error, {:sense_count_changed, 2, 1}} =
             DefinitionReviewGate.validate(plan, "point", "noun", 2, 1, plan.fingerprint)

    assert {:error, {:patch_count_changed, 2, 1}} =
             DefinitionReviewGate.validate(plan, "point", "noun", 1, 2, plan.fingerprint)

    assert {:error, :review_fingerprint_changed} =
             DefinitionReviewGate.validate(
               plan, "point", "noun", 1, 1, String.duplicate("a", 64)
             )

    assert {:error, :invalid_fingerprint_format} =
             DefinitionReviewGate.validate(plan, "point", "noun", 1, 1, "bad")
  end

  test "refuses missing approval inputs and zero-change plans" do
    plan = plan()

    assert {:error, :invalid_review_approval} =
             DefinitionReviewGate.validate(plan, "point", "noun", nil, 1, plan.fingerprint)

    assert {:error, :invalid_review_approval} =
             DefinitionReviewGate.validate(plan, "point", "noun", 1, nil, plan.fingerprint)

    assert {:error, :expected_counts_must_be_positive} =
             DefinitionReviewGate.validate(plan, "point", "noun", 1, 0, plan.fingerprint)

    assert {:error, :expected_counts_must_be_positive} =
             DefinitionReviewGate.validate(plan, "point", "noun", 0, 1, plan.fingerprint)
  end
end
