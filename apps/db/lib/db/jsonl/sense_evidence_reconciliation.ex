defmodule Db.JSONL.SenseEvidenceReconciliation do
  @moduledoc """
  Read-only classification of actual BrainCells versus the current Wiktextract
  per-sense reconstruction. A matching positional ID is *not* proof of immutable
  semantic identity. Only exact old-first-gloss plus other metadata matches
  produce a candidate for the separate human-reviewed repair pipeline.
  """

  @type verdict :: :absent_id | :matched_current | :missing_source_evidence |
                     :different_definition | :no_gloss | :grammar_derivation_drift |
                     :metadata_mismatch | :wrong_word_pos

  @spec classify(Db.BrainCell.t(), map() | nil) :: {verdict(), map()}
  def classify(source, nil) do
    {:absent_id, details(source, nil, [], [])}
  end

  def classify(source, stored) when is_map(stored) do
    expected_definition = source.definition
    previous_definition = stored.definition
    expected_atoms = List.wrap(source.semantic_atoms)
    stored_atoms = List.wrap(stored.semantic_atoms)
    expected_categories = categories(expected_atoms)
    existing_categories = categories(stored_atoms)
    missing_categories = expected_categories -- existing_categories
    missing_usage = usage_atoms(expected_atoms) -- usage_atoms(stored_atoms)

    detail =
      details(source, stored, missing_categories, missing_usage)

    cond do
      source.norm != stored.norm or source.pos != stored.pos ->
        {:wrong_word_pos, detail}

      not non_grammar_metadata_matches?(source, stored) ->
        {:metadata_mismatch, detail}

      is_nil(previous_definition) and is_nil(expected_definition) ->
        {:no_gloss, detail}

      is_nil(previous_definition) or is_nil(expected_definition) ->
        {:different_definition, detail}

      grammar_derivation_drift?(source.gram_function, stored.gram_function) ->
        # The current grammar canonicalizer adds "usually plural" to these
        # cases. Keep them out of automated repair until their provenance is
        # reviewed, even when gloss and category evidence could be restored.
        {:grammar_derivation_drift, detail}

      source.gram_function != stored.gram_function ->
        {:metadata_mismatch, detail}

      previous_definition != expected_definition and
          not String.starts_with?(expected_definition, previous_definition <> " — ") ->
        {:different_definition, detail}

      previous_definition == expected_definition and
          missing_categories == [] and missing_usage == [] ->
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

  defp non_grammar_metadata_matches?(source, stored) do
    structural_atoms(source.semantic_atoms) == structural_atoms(stored.semantic_atoms) and
      source.example == stored.example
  end

  defp grammar_derivation_drift?(source, stored)
       when is_list(source) and is_list(stored) do
    source_set = MapSet.new(source)
    stored_set = MapSet.new(stored)

    MapSet.difference(source_set, stored_set) == MapSet.new(["usually plural"]) and
      MapSet.subset?(stored_set, source_set)
  end

  defp grammar_derivation_drift?(_, _), do: false

  defp usage_atoms(atoms) do
    atoms
    |> List.wrap()
    |> Enum.filter(&(is_binary(&1) and String.starts_with?(&1, "usage:")))
    |> Enum.uniq()
    |> Enum.sort()
  end

  defp structural_atoms(atoms) do
    atoms
    |> List.wrap()
    |> Enum.reject(&(is_binary(&1) and
                      (String.starts_with?(&1, "cat:") or
                       String.starts_with?(&1, "usage:"))))
    |> Enum.sort()
  end

  defp details(source, nil, missing_categories, missing_usage) do
    %{
      id: source.id,
      original_definition: nil,
      reconstructed_definition: source.definition,
      added_categories: missing_categories,
      added_usage_atoms: missing_usage,
      has_multiple_glosses?: false
    }
  end

  defp details(source, stored, missing_categories, missing_usage) do
    previous = stored.definition
    expected = source.definition

    %{
      id: source.id,
      original_definition: previous,
      reconstructed_definition: expected,
      added_categories: missing_categories,
      added_usage_atoms: missing_usage,
      has_multiple_glosses?: is_binary(previous) and is_binary(expected) and
        previous != expected,
      # Explain the six observed metadata mismatches without discarding the
      # source sense or weakening the conservative reconciliation gate.
      stored_example: stored.example,
      reconstructed_example: source.example,
      stored_gram_function: stored.gram_function,
      reconstructed_gram_function: source.gram_function,
      stored_noncategory_atoms: structural_atoms(stored.semantic_atoms),
      reconstructed_noncategory_atoms: structural_atoms(source.semantic_atoms)
    }
  end
end
