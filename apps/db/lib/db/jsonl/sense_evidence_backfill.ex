defmodule Db.JSONL.SenseEvidenceBackfill do
  @moduledoc """
  Guarded, opt-in restoration of lost Wiktextract gloss/category evidence.

  Restricted to a single source word and POS. The caller must first inspect
  an immutable plan fingerprint; no DB write is performed by plan/4.

  The numbered dictionary ID is *positional*, not a permanent source ID.
  Matching IDs, counts and original first glosses are necessary but NOT
  independent proof that sense order has never changed. Human review of
  every regenerated definition is required before approval.

  Only :definition, :semantic_atoms and :updated_at are written. All activation
  state, embeddings, connections, tags, grammar, synonyms and IDs are left
  intact. Writes are guarded by previous column values in one transaction.
  """

  import Ecto.Query, only: [from: 2]

  @type plan :: map()

  @spec plan(String.t(), String.t(), [Db.BrainCell.t()], [Db.BrainCell.t()]) ::
          {:ok, plan()} | {:error, term()}
  def plan(word, pos, source_cells, stored_cells)
      when is_binary(word) and is_binary(pos) and
             is_list(source_cells) and is_list(stored_cells) do
    source = Enum.filter(source_cells, &(get(&1, :pos) == pos))
    stored = Enum.filter(stored_cells, &(get(&1, :pos) == pos))

    source_ids = Enum.map(source, &get(&1, :id))
    stored_ids = Enum.map(stored, &get(&1, :id))

    cond do
      source == [] ->
        {:error, :no_source_senses}

      source_ids != Enum.uniq(source_ids) or stored_ids != Enum.uniq(stored_ids) ->
        {:error, :duplicate_sense_ids}

      Enum.sort(source_ids) != Enum.sort(stored_ids) ->
        {:error, {:source_db_id_mismatch, Enum.sort(source_ids), Enum.sort(stored_ids)}}

      not Enum.all?(source ++ stored, &matches_word_pos?(&1, word, pos)) ->
        {:error, :word_or_pos_mismatch}

      true ->
        stored_by_id = Map.new(stored, &{get(&1, :id), &1})

        with {:ok, rows} <- build_rows(source, stored_by_id) do
          patches = Enum.filter(rows, & &1.changed?)
          fingerprint = fingerprint(word, pos, rows)

          {:ok, %{
            word: word,
            pos: pos,
            reviewed_source_ids: Enum.sort(source_ids),
            row_count: length(rows),
            patch_count: length(patches),
            rows: rows,
            patches: patches,
            fingerprint: fingerprint
          }}
        end
    end
  end

  def plan(_, _, _, _), do: {:error, :invalid_arguments}

  @spec apply(plan(), String.t()) :: {:ok, non_neg_integer()} | {:error, term()}
  def apply(%{fingerprint: expected, rows: rows, patches: patches,
              word: word, pos: pos}, approved_fingerprint)
      when is_binary(approved_fingerprint) do
    cond do
      approved_fingerprint != expected ->
        {:error, :fingerprint_mismatch}

      # Prevent callers from bypassing the intended review via altered plans.
      fingerprint(word, pos, rows) != expected or
          Enum.filter(rows, & &1.changed?) != patches or
          Enum.any?(patches, &(!valid_patch?(&1, word, pos))) ->
        {:error, :invalid_plan}

      true ->
        normalized_word = String.downcase(String.trim(word))

        Db.transaction(fn ->
          now = NaiveDateTime.utc_now() |> NaiveDateTime.truncate(:second)

          Enum.each(patches, fn patch ->
            query =
              from(c in Db.BrainCell,
                where:
                  c.id == ^patch.id and c.norm == ^normalized_word and c.pos == ^pos and
                    c.definition == ^patch.stored_definition and
                    c.semantic_atoms == ^patch.stored_semantic_atoms
              )

            {count, _} =
              Db.update_all(query,
                set: [
                  definition: patch.reconstructed_definition,
                  semantic_atoms: patch.reconstructed_semantic_atoms,
                  updated_at: now
                ])

            if count != 1 do
              Db.rollback({:stale_row, patch.id, count})
            end
          end)

          length(patches)
        end)
    end
  end

  def apply(_, _), do: {:error, :invalid_plan}

  defp build_rows(source, stored_by_id) do
    source
    |> Enum.sort_by(&get(&1, :id))
    |> Enum.reduce_while({:ok, []}, fn reconstructed, {:ok, acc} ->
      stored = Map.fetch!(stored_by_id, get(reconstructed, :id))

      case build_row(reconstructed, stored) do
        {:ok, row} -> {:cont, {:ok, [row | acc]}}
        {:error, reason} -> {:halt, {:error, {get(reconstructed, :id), reason}}}
      end
    end)
    |> case do
      {:ok, rows} -> {:ok, Enum.reverse(rows)}
      error -> error
    end
  end

  defp build_row(source, stored) do
    old = get(stored, :definition)
    new = get(source, :definition)
    old_atoms = get(stored, :semantic_atoms)
    new_atoms = get(source, :semantic_atoms)

    cond do
      not is_binary(old) or String.trim(old) == "" or not is_binary(new) ->
        {:error, :missing_definition}

      not (old == new or String.starts_with?(new, old <> " — ")) ->
        {:error, :existing_gloss_not_source_prefix}

      not is_list(old_atoms) or not is_list(new_atoms) ->
        {:error, :missing_semantic_atoms}

      non_category_atoms(old_atoms) != non_category_atoms(new_atoms) ->
        {:error, :existing_noncategory_metadata_mismatch}

      get(stored, :example) != get(source, :example) ->
        {:error, :existing_example_mismatch}

      get(stored, :gram_function) != get(source, :gram_function) ->
        {:error, :existing_grammar_mismatch}

      true ->
        new_categories = Enum.filter(new_atoms, &category?/1)

        # Preserve any already-added stored categories, and do not modify
        # stored non-category atoms or state. Only append source categories.
        enriched = Enum.uniq(old_atoms ++ new_categories)

        {:ok, %{
          id: get(stored, :id),
          word: get(stored, :norm),
          pos: get(stored, :pos),
          stored_definition: old,
          reconstructed_definition: new,
          stored_semantic_atoms: old_atoms,
          reconstructed_semantic_atoms: enriched,
          added_categories: Enum.reject(new_categories, &(&1 in old_atoms)),
          changed?: old != new or enriched != old_atoms
        }}
    end
  end

  defp matches_word_pos?(cell, word, pos) do
    id = get(cell, :id)
    norm = get(cell, :norm)

    is_binary(id) and is_binary(norm) and
      String.downcase(String.trim(norm)) == String.downcase(word) and
      get(cell, :pos) == pos and
      String.starts_with?(id, String.downcase(word) <> "|" <> pos <> "|")
  end

  defp valid_patch?(patch, word, pos) do
    patch.changed? and patch.pos == pos and
      patch.word == String.downcase(word) and
      is_binary(patch.stored_definition) and
      is_binary(patch.reconstructed_definition) and
      (patch.stored_definition == patch.reconstructed_definition or
         String.starts_with?(patch.reconstructed_definition,
           patch.stored_definition <> " — ")) and
      is_list(patch.added_categories) and
      is_list(patch.stored_semantic_atoms) and
      is_list(patch.reconstructed_semantic_atoms)
  end

  defp non_category_atoms(atoms) do
    atoms
    |> Enum.reject(&category?/1)
    |> Enum.sort()
  end

  defp category?(atom), do: is_binary(atom) and String.starts_with?(atom, "cat:")

  defp fingerprint(word, pos, rows) do
    serialized =
      {word, pos,
       Enum.map(rows, fn row ->
         {row.id, row.stored_definition, row.reconstructed_definition,
          row.stored_semantic_atoms, row.reconstructed_semantic_atoms}
       end)}

    :crypto.hash(:sha256, :erlang.term_to_binary(serialized))
    |> Base.encode16(case: :lower)
  end

  defp get(map, field) when is_map(map), do: Map.get(map, field)
  defp get(_, _), do: nil
end
