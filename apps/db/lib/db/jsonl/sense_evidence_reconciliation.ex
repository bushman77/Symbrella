defmodule Db.JSONL.SenseEvidenceReconciliation do
  @moduledoc """
  Read-only classification of actual BrainCells versus the current Wiktextract
  per-sense reconstruction. A matching positional ID is *not* proof of immutable
  semantic identity. Only exact old-first-gloss plus other metadata matches
  produce a candidate for the separate human-reviewed repair pipeline.
  """

  @type verdict :: :absent_id | :matched_current | :missing_source_evidence |
                     :different_definition | :metadata_mismatch | :wrong_word_pos

  @spec classify(Db.BrainCell.t(), map() | nil) :: {verdict(), map()}
  def classify(source, nil) do
    {:absent_id, details(source, nil, [])}
  end

  def classify(source, stored) when is_map(stored) do
    expected_definition = source.definition
    previous_definition = stored.definition
    expected_atoms = List.wrap(source.semantic_atoms)
    stored_atoms = List.wrap(stored.semantic_atoms)
    expected_categories = categories(expected_atoms)
    existing_categories = categories(stored_atoms)
    missing_categories = expected_categories -- existing_categories

    detail =
      details(source, stored, missing_categories)

    cond do
      source.norm != stored.norm or source.pos != stored.pos ->
        {:wrong_word_pos, detail}

      not metadata_matches?(source, stored) ->
        {:metadata_mismatch, detail}

      is_nil(previous_definition) or is_nil(expected_definition) ->
        {:different_definition, detail}

      previous_definition != expected_definition and
          not String.starts_with?(expected_definition, previous_definition <> " — ") ->
        {:different_definition, detail}

      previous_definition == expected_definition and missing_categories == [] ->
        {:matched_current, detail}

      true ->
        {:missing_source_evidence, detail}
    end
  end

  @spec categories(list()) :: [String.t()]
  def categories(atoms) do
    atoms
    |> Enum.filter(&(is_binary(&1) and String.starts_with?(&1, "cat:")))
    |> Enum.uniq()
    |> Enum.sort()
  end

  defp metadata_matches?(source, stored) do
    noncategory_atoms(source.semantic_atoms) == noncategory_atoms(stored.semantic_atoms) and
      source.gram_function == stored.gram_function and
      source.example == stored.example
  end

  defp noncategory_atoms(atoms) do
    atoms
    |> List.wrap()
    |> Enum.reject(&(is_binary(&1) and String.starts_with?(&1, "cat:")))
    |> Enum.sort()
  end

  defp details(source, nil, missing_categories) do
    %{
      id: source.id,
      original_definition: nil,
      reconstructed_definition: source.definition,
      added_categories: missing_categories,
      has_multiple_glosses?: false
    }
  end

  defp details(source, stored, missing_categories) do
    previous = stored.definition
    expected = source.definition

    %{
      id: source.id,
      original_definition: previous,
      reconstructed_definition: expected,
      added_categories: missing_categories,
      has_multiple_glosses?: is_binary(previous) and is_binary(expected) and
        previous != expected,
      # Explain the six observed metadata mismatches without discarding the
      # source sense or weakening the conservative reconciliation gate.
      stored_example: stored.example,
      reconstructed_example: source.example,
      stored_gram_function: stored.gram_function,
      reconstructed_gram_function: source.gram_function,
      stored_noncategory_atoms: noncategory_atoms(stored.semantic_atoms),
      reconstructed_noncategory_atoms: noncategory_atoms(source.semantic_atoms)
    }
  end
end
