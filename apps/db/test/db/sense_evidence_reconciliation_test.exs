defmodule Db.JSONL.SenseEvidenceReconciliationTest do
  use ExUnit.Case, async: true

  alias Db.JSONL.SenseEvidenceReconciliation, as: Reconciliation

  defp source do
    %Db.BrainCell{
      id: "cash|proper_noun|3",
      norm: "cash",
      word: "cash",
      pos: "proper_noun",
      definition: "A number of places: — A city in Arkansas.",
      example: nil,
      gram_function: [],
      semantic_atoms: ["pos_raw:name", "cat:en:places in arkansas, usa"]
    }
  end

  defp stored do
    %{
      id: "cash|proper_noun|3",
      norm: "cash",
      pos: "proper_noun",
      definition: "A number of places:",
      example: nil,
      gram_function: [],
      semantic_atoms: ["pos_raw:name"]
    }
  end

  test "identifies exact-prefix evidence gaps without treating category tags as synonyms" do
    assert {:missing_source_evidence, detail} =
             Reconciliation.classify(source(), stored())

    assert detail.id == "cash|proper_noun|3"
    assert detail.original_definition == "A number of places:"
    assert detail.reconstructed_definition == "A number of places: — A city in Arkansas."
    assert detail.added_categories == ["cat:en:places in arkansas, usa"]
    assert detail.has_multiple_glosses?
  end

  test "classifies fully restored rows separately from missing ones" do
    current = %{stored() |
      definition: source().definition,
      semantic_atoms: source().semantic_atoms}

    assert {:matched_current, detail} = Reconciliation.classify(source(), current)
    assert detail.added_categories == []
    refute detail.has_multiple_glosses?
  end

  test "counts missing categories when full gloss is already present" do
    current = %{stored() | definition: source().definition}
    assert {:missing_source_evidence, detail} = Reconciliation.classify(source(), current)
    assert detail.original_definition == detail.reconstructed_definition
    assert detail.added_categories == ["cat:en:places in arkansas, usa"]
  end

  test "never calls a source-ID collision safely recoverable when gloss differs" do
    conflicting = %{stored() | definition: "An unrelated geographic name."}
    assert {:different_definition, _} = Reconciliation.classify(source(), conflicting)
    assert {:absent_id, _} = Reconciliation.classify(source(), nil)
  end

  test "rejects metadata and word/POS mismatches" do
    assert {:metadata_mismatch, atom_detail} =
             Reconciliation.classify(source(),
               %{stored() | semantic_atoms: ["pos_raw:unrelated"]})

    assert atom_detail.stored_noncategory_atoms == ["pos_raw:unrelated"]
    assert atom_detail.reconstructed_noncategory_atoms == ["pos_raw:name"]

    assert {:metadata_mismatch, example_detail} =
             Reconciliation.classify(source(),
               %{stored() | example: "A changed example."})

    assert example_detail.stored_example == "A changed example."
    assert example_detail.reconstructed_example == nil

    assert {:wrong_word_pos, _} =
             Reconciliation.classify(source(), %{stored() | pos: "noun"})
  end

  test "labels an added usually-plural derivation without treating it as a safe backfill" do
    derived_source = %{source() | gram_function: ["countable", "usually plural"]}
    original = %{stored() | gram_function: ["countable"]}

    assert {:grammar_derivation_drift, detail} =
             Reconciliation.classify(derived_source, original)

    assert detail.stored_gram_function == ["countable"]
    assert detail.reconstructed_gram_function == ["countable", "usually plural"]

    unrelated = %{derived_source | gram_function: ["countable", "plural-only"]}

    assert {:metadata_mismatch, _} =
             Reconciliation.classify(unrelated, original)
  end

  test "distinguishes glossary-free translation hubs from true ID conflicts" do
    source_without_gloss = %{source() | definition: nil}
    stored_without_gloss = %{stored() | definition: nil}

    assert {:no_gloss, _} =
             Reconciliation.classify(source_without_gloss, stored_without_gloss)

    assert {:different_definition, _} =
             Reconciliation.classify(source(), stored_without_gloss)
  end

  test "preserves a source category when it is already stored" do
    existing = %{stored() | semantic_atoms: source().semantic_atoms}
    assert {:missing_source_evidence, detail} = Reconciliation.classify(source(), existing)
    assert detail.added_categories == []
  end
end
