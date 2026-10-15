defmodule Db.JSONL.DefinitionOnlyBackfill do
  @moduledoc """
  Reviewed, opt-in definition-only recovery from Wiktextract gloss paths.

  Every source POS sense must correspond to a stored positional sense ID.
  Reviews include all senses of that word/POS, not only changed ones.
  Reordered equal first-glosses cannot be proven stable by positional IDs:
  inspect examples and full definitions manually before approval.

  Plan generation does no writes. Apply is fingerprint-gated, transactional,
  and compares original definitions, examples, relations, grammar and atoms
  against the database at UPDATE time. No fields except definition and
  updated_at may be modified.
  """

  import Ecto.Query, only: [from: 2]

  @type plan :: map()

  @spec plan(String.t(), String.t(), list(), list()) ::
          {:ok, plan()} | {:error, term()}
  def plan(word, pos, source_cells, stored_cells)
      when is_binary(word) and is_binary(pos) and
             is_list(source_cells) and is_list(stored_cells) do
    norm = String.downcase(String.trim(word))
    source = Enum.filter(source_cells, &(&1.pos == pos))
    stored = Enum.filter(stored_cells, &(&1.pos == pos))

    source_ids = Enum.map(source, & &1.id)
    stored_ids = Enum.map(stored, & &1.id)

    cond do
      source == [] ->
        {:error, :no_source_senses}

      length(source_ids) != length(Enum.uniq(source_ids)) or
          length(stored_ids) != length(Enum.uniq(stored_ids)) ->
        {:error, :duplicate_ids}

      Enum.sort(source_ids) != Enum.sort(stored_ids) ->
        {:error, {:id_census_mismatch, Enum.sort(source_ids), Enum.sort(stored_ids)}}

      not Enum.all?(source ++ stored, fn cell ->
        cell.norm == norm and cell.pos == pos and
          String.starts_with?(cell.id, norm <> "|" <> pos <> "|")
      end) ->
        {:error, :word_pos_mismatch}

      true ->
        stored_by_id = Map.new(stored, &{&1.id, &1})

        result =
          source
          |> Enum.sort_by(& &1.id)
          |> Enum.reduce_while({:ok, []}, fn reconstructed, {:ok, rows} ->
            original = Map.fetch!(stored_by_id, reconstructed.id)

            case compare_sense(original, reconstructed) do
              {:ok, row} -> {:cont, {:ok, [row | rows]}}
              {:error, reason} -> {:halt, {:error, {reconstructed.id, reason}}}
            end
          end)

        case result do
          {:error, _} = error ->
            error

          {:ok, reversed_rows} ->
            rows = Enum.reverse(reversed_rows)
            patches = Enum.filter(rows, & &1.changed?)
            {:ok, %{
              word: norm,
              pos: pos,
              row_count: length(rows),
              patch_count: length(patches),
              rows: rows,
              patches: patches,
              fingerprint: fingerprint(norm, pos, rows)
            }}
        end
    end
  end

  def plan(_, _, _, _), do: {:error, :invalid_arguments}

  @spec apply(plan(), String.t()) :: {:ok, non_neg_integer()} | {:error, term()}
  def apply(%{word: word, pos: pos, rows: rows, patches: patches,
              fingerprint: expected}, approved)
      when is_binary(approved) do
    cond do
      approved != expected ->
        {:error, :fingerprint_mismatch}

      fingerprint(word, pos, rows) != expected or
          Enum.filter(rows, & &1.changed?) != patches or
          Enum.any?(patches, fn p ->
            p.word != word or p.pos != pos or
              not String.starts_with?(p.id, word <> "|" <> pos <> "|") or
              not is_binary(p.old_definition) or
              not is_binary(p.new_definition) or
              not String.starts_with?(p.new_definition, p.old_definition <> " — ")
          end) ->
        {:error, :invalid_plan}

      true ->
        Db.transaction(fn ->
          now = NaiveDateTime.utc_now() |> NaiveDateTime.truncate(:second)

          Enum.each(patches, fn row ->
            query =
              from(c in Db.BrainCell,
                where:
                  c.id == ^row.id and c.norm == ^word and c.pos == ^pos and
                    c.definition == ^row.old_definition and
                    c.synonyms == ^row.synonyms and
                    c.antonyms == ^row.antonyms and
                    c.gram_function == ^row.gram_function and
                    c.semantic_atoms == ^row.semantic_atoms
              )

            query =
              if is_nil(row.example) do
                from(c in query, where: is_nil(c.example))
              else
                from(c in query, where: c.example == ^row.example)
              end

            {count, _} =
              Db.update_all(query,
                set: [definition: row.new_definition, updated_at: now])

            if count != 1, do: Db.rollback({:stale_or_missing_row, row.id, count})
          end)

          length(patches)
        end)
    end
  end

  def apply(_, _), do: {:error, :invalid_plan}

  defp compare_sense(original, reconstructed) do
    cond do
      original.example != reconstructed.example ->
        {:error, :example_conflict}

      original.gram_function != reconstructed.gram_function ->
        {:error, :grammar_conflict}

      original.synonyms != reconstructed.synonyms or
          original.antonyms != reconstructed.antonyms ->
        {:error, :relation_conflict}

      immutable_atoms(original.semantic_atoms) !=
          immutable_atoms(reconstructed.semantic_atoms) ->
        {:error, :lexical_metadata_conflict}

      original.definition == reconstructed.definition ->
        {:ok, make_row(original, reconstructed, false)}

      is_binary(original.definition) and is_binary(reconstructed.definition) and
          String.starts_with?(reconstructed.definition,
            original.definition <> " — ") ->
        {:ok, make_row(original, reconstructed, true)}

      true ->
        {:error, :definition_conflict}
    end
  end

  defp make_row(original, reconstructed, changed?) do
    %{
      id: original.id,
      word: original.norm,
      pos: original.pos,
      old_definition: original.definition,
      new_definition: reconstructed.definition,
      example: original.example,
      synonyms: original.synonyms,
      antonyms: original.antonyms,
      gram_function: original.gram_function,
      semantic_atoms: original.semantic_atoms,
      changed?: changed?
    }
  end

  defp immutable_atoms(atoms) do
    atoms
    |> List.wrap()
    |> Enum.reject(fn atom ->
      is_binary(atom) and
        (String.starts_with?(atom, "cat:") or String.starts_with?(atom, "usage:"))
    end)
    |> Enum.sort()
  end

  defp fingerprint(word, pos, rows) do
    fields =
      Enum.map(rows, fn row ->
        {row.id, row.old_definition, row.new_definition, row.example,
         row.synonyms, row.antonyms, row.gram_function, row.semantic_atoms}
      end)

    :crypto.hash(:sha256, :erlang.term_to_binary({word, pos, fields}))
    |> Base.encode16(case: :lower)
  end
end
