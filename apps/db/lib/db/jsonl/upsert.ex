defmodule Db.JSONL.Upsert do
  @moduledoc false

  alias Db.JSONL.{Fetch, Populate}

  @spec upsert_word(String.t(), keyword()) :: {:ok, map()} | {:error, any()}
  def upsert_word(word, opts \\ []) when is_binary(word) do
    with {:ok, senses} <- Fetch.fetch_senses(word, opts) do
      cells = Populate.populate_structs_from_senses(senses, opts)
      upsert_cells(cells, opts)
    end
  end

  @spec upsert_cells(list(), keyword()) :: {:ok, map()} | {:error, any()}
  def upsert_cells(cells, opts \\ []) when is_list(cells) do
    repo = Keyword.get(opts, :repo, Db)
    batch_size = Keyword.get(opts, :batch_size, 500)
    validate? = Keyword.get(opts, :validate?, false)
    on_db_error = Keyword.get(opts, :on_db_error, :raise)
    returning? = Keyword.get(opts, :returning?, false)

    now = NaiveDateTime.utc_now() |> NaiveDateTime.truncate(:second)

    {rows, invalid_count} =
      if validate? do
        Enum.reduce(cells, {[], 0}, fn cell, {acc, bad} ->
          attrs = cell_to_attrs(cell, now)
          cs = Db.BrainCell.changeset(%Db.BrainCell{}, attrs)

          if cs.valid? do
            {[cs.changes | acc], bad}
          else
            {acc, bad + 1}
          end
        end)
        |> then(fn {ok_rows_rev, bad} -> {Enum.reverse(ok_rows_rev), bad} end)
      else
        {Enum.map(cells, &cell_to_attrs(&1, now)), 0}
      end

    {upserted, batches, db_errors, ids} =
      rows
      |> Enum.chunk_every(batch_size)
      |> Enum.reduce({0, 0, 0, []}, fn batch, {cnt, b, errs, id_acc} ->
        case safe_insert_all(repo, batch, returning?, on_db_error) do
          {:ok, {n, returned}} ->
            ids =
              case returned do
                nil -> id_acc
                list when is_list(list) -> list ++ id_acc
                _ -> id_acc
              end

            {cnt + n, b + 1, errs, ids}

          {:error, _e} when on_db_error == :count ->
            {cnt, b + 1, errs + 1, id_acc}

          {:error, e} when on_db_error == :halt ->
            throw({:db_error, e, %{upserted_so_far: cnt, batches: b}})

          {:error, e} ->
            raise e
        end
      end)

    {:ok,
     %{
       upserted: upserted,
       batches: batches,
       invalid_skipped: invalid_count,
       db_errors: db_errors,
       returned_ids: Enum.reverse(ids)
     }}
  catch
    {:db_error, e, meta} -> {:error, {:db_error, e, meta}}
  end

  # ───────────────────────── internals used by Import ─────────────────────────

  @doc false
  @spec safe_insert_all(module(), [map()], boolean(), :raise | :halt | :count) ::
          {:ok, {non_neg_integer(), any()}} | {:error, any()}
  def safe_insert_all(repo, rows, returning?, _on_db_error) do
    rows = dedupe_merge_rows(rows)

    conflict_target = [:id]
    on_conflict = {:replace, replace_cols()}

    opts = [on_conflict: on_conflict, conflict_target: conflict_target]
    opts = if returning?, do: Keyword.put(opts, :returning, [:id]), else: opts

    try do
      {:ok, repo.insert_all(Db.BrainCell, rows, opts)}
    rescue
      e -> {:error, e}
    end
  end

  defp replace_cols do
    [
      :word,
      :norm,
      :pos,
      :type,
      :status,
      :gram_function,
      :definition,
      :example,
      :synonyms,
      :antonyms,
      :semantic_atoms,
      :updated_at
    ]
  end

  @doc false
  @spec cell_to_attrs(Db.BrainCell.t() | map(), NaiveDateTime.t()) :: map()
  def cell_to_attrs(%Db.BrainCell{} = c, now) do
    %{
      id: c.id,
      word: c.word,
      norm: c.norm,
      pos: c.pos,
      type: c.type,
      status: c.status,
      gram_function: c.gram_function || [],
      definition: c.definition,
      example: c.example,
      synonyms: c.synonyms || [],
      antonyms: c.antonyms || [],
      semantic_atoms: c.semantic_atoms || [],
      inserted_at: now,
      updated_at: now
    }
  end

  def cell_to_attrs(%{} = m, now) do
    %{
      id: Map.get(m, :id) || Map.get(m, "id"),
      word: Map.get(m, :word) || Map.get(m, "word"),
      norm: Map.get(m, :norm) || Map.get(m, "norm"),
      pos: Map.get(m, :pos) || Map.get(m, "pos"),
      type: Map.get(m, :type) || Map.get(m, "type"),
      status: Map.get(m, :status) || Map.get(m, "status"),
      gram_function: Map.get(m, :gram_function) || Map.get(m, "gram_function") || [],
      definition: Map.get(m, :definition) || Map.get(m, "definition"),
      example: Map.get(m, :example) || Map.get(m, "example"),
      synonyms: Map.get(m, :synonyms) || Map.get(m, "synonyms") || [],
      antonyms: Map.get(m, :antonyms) || Map.get(m, "antonyms") || [],
      semantic_atoms: Map.get(m, :semantic_atoms) || Map.get(m, "semantic_atoms") || [],
      inserted_at: now,
      updated_at: now
    }
  end

  # Prevents Postgres "ON CONFLICT DO UPDATE command cannot affect row a second time"
  # by ensuring each insert_all/3 call has unique ids. We also merge duplicates so we
  # don't drop synonym/atom payloads.
  defp dedupe_merge_rows(rows) when is_list(rows) do
    {acc, order_rev} =
      Enum.reduce(rows, {%{}, []}, fn row, {m, order} ->
        id = Map.get(row, :id)

        cond do
          not is_binary(id) or id == "" ->
            {m, order}

          true ->
            case Map.fetch(m, id) do
              {:ok, existing} ->
                {Map.put(m, id, merge_rows(existing, row)), order}

              :error ->
                {Map.put(m, id, row), [id | order]}
            end
        end
      end)

    order_rev
    |> Enum.reverse()
    |> Enum.map(&Map.fetch!(acc, &1))
  end

  defp merge_rows(a, b) do
    %{
      id: Map.get(a, :id) || Map.get(b, :id),
      word: pick_first_present(Map.get(a, :word), Map.get(b, :word)),
      norm: pick_first_present(Map.get(a, :norm), Map.get(b, :norm)),
      pos: pick_first_present(Map.get(a, :pos), Map.get(b, :pos)),
      type: pick_first_present(Map.get(a, :type), Map.get(b, :type)),
      status: pick_first_present(Map.get(a, :status), Map.get(b, :status)),
      gram_function: merge_list(Map.get(a, :gram_function), Map.get(b, :gram_function)),
      definition: pick_longer(Map.get(a, :definition), Map.get(b, :definition)),
      example: pick_longer(Map.get(a, :example), Map.get(b, :example)),
      synonyms: merge_list(Map.get(a, :synonyms), Map.get(b, :synonyms)),
      antonyms: merge_list(Map.get(a, :antonyms), Map.get(b, :antonyms)),
      semantic_atoms: merge_list(Map.get(a, :semantic_atoms), Map.get(b, :semantic_atoms)),
      inserted_at: min_dt(Map.get(a, :inserted_at), Map.get(b, :inserted_at)),
      updated_at: max_dt(Map.get(a, :updated_at), Map.get(b, :updated_at))
    }
  end

  defp pick_first_present(a, b) do
    cond do
      is_binary(a) and String.trim(a) != "" -> a
      is_binary(b) and String.trim(b) != "" -> b
      not is_nil(a) -> a
      true -> b
    end
  end

  defp pick_longer(a, b) do
    a2 = if is_binary(a), do: String.trim(a), else: a
    b2 = if is_binary(b), do: String.trim(b), else: b

    cond do
      is_binary(a2) and a2 != "" and (not is_binary(b2) or b2 == "") -> a2
      is_binary(b2) and b2 != "" and (not is_binary(a2) or a2 == "") -> b2
      is_binary(a2) and is_binary(b2) and byte_size(b2) > byte_size(a2) -> b2
      true -> a2
    end
  end

  defp merge_list(a, b) do
    (List.wrap(a) ++ List.wrap(b))
    |> Enum.filter(&is_binary/1)
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
    |> Enum.uniq()
  end

  defp min_dt(nil, b), do: b
  defp min_dt(a, nil), do: a

  defp min_dt(%NaiveDateTime{} = a, %NaiveDateTime{} = b) do
    case NaiveDateTime.compare(a, b) do
      :gt -> b
      _ -> a
    end
  end

  defp min_dt(a, _b), do: a

  defp max_dt(nil, b), do: b
  defp max_dt(a, nil), do: a

  defp max_dt(%NaiveDateTime{} = a, %NaiveDateTime{} = b) do
    case NaiveDateTime.compare(a, b) do
      :lt -> b
      _ -> a
    end
  end

  defp max_dt(a, _b), do: a
end
