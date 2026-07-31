defmodule Db.BrainCellEmbeddings do
  @moduledoc """
  DB context for BrainCell semantic embeddings.

  Boundary rule:

  - This module does not create embeddings.
  - This module does not depend on Nx.
  - This module stores, retrieves, and queries persisted BrainCell vectors.
  - Runtime math belongs in Core/Brain/Nx-facing modules.
  - Persistent vectors live in `Db.BrainCell.embedding`.

  A BrainCell embedding represents one exact sense id, for example:

      "there|noun|0"

  not the broad norm:

      "there"
  """

  import Ecto.Query

  alias Db.BrainCell

  @embedding_dim Application.compile_env(:db, :brain_cell_embedding_dim, 768)

  @type brain_cell_id :: String.t()
  @type embedding_vector :: [number()]
  @type sense_text :: String.t()

  @doc """
  Returns global embedding coverage for `brain_cells`.
  """
  @spec coverage() :: %{
          total_cells: non_neg_integer(),
          with_embedding: non_neg_integer(),
          without_embedding: non_neg_integer()
        }
  def coverage do
    Db.one(
      from(c in BrainCell,
        select: %{
          total_cells: count(c.id),
          with_embedding: fragment("count(?) FILTER (WHERE ? IS NOT NULL)", c.id, c.embedding),
          without_embedding: fragment("count(?) FILTER (WHERE ? IS NULL)", c.id, c.embedding)
        }
      )
    )
  end

  @doc """
  Returns embedding coverage for one normalized word, such as `"there"`.
  """
  @spec coverage_for_norm(String.t()) :: map()
  def coverage_for_norm(norm) when is_binary(norm) do
    normalized = normalize_norm(norm)

    Db.one(
      from(c in BrainCell,
        where: c.norm == ^normalized,
        select: %{
          norm: ^normalized,
          total_cells: count(c.id),
          with_embedding: fragment("count(?) FILTER (WHERE ? IS NOT NULL)", c.id, c.embedding),
          without_embedding: fragment("count(?) FILTER (WHERE ? IS NULL)", c.id, c.embedding)
        }
      )
    )
  end

  @doc """
  Fetches one exact BrainCell sense by id.
  """
  @spec get_cell(brain_cell_id()) :: BrainCell.t() | nil
  def get_cell(id) when is_binary(id) do
    Db.get(BrainCell, id)
  end

  @doc """
  Fetches one exact BrainCell sense by id.

  Raises if the row does not exist.
  """
  @spec get_cell!(brain_cell_id()) :: BrainCell.t()
  def get_cell!(id) when is_binary(id) do
    Db.get!(BrainCell, id)
  end

  @doc """
  Returns true if one exact BrainCell sense already has an embedding.
  """
  @spec embedded?(brain_cell_id()) :: boolean()
  def embedded?(id) when is_binary(id) do
    Db.exists?(
      from(c in BrainCell,
        where: c.id == ^id and not is_nil(c.embedding)
      )
    )
  end

  @doc """
  Returns true if one exact BrainCell sense exists but has no embedding.
  """
  @spec needs_embedding?(brain_cell_id()) :: boolean()
  def needs_embedding?(id) when is_binary(id) do
    Db.exists?(
      from(c in BrainCell,
        where: c.id == ^id and is_nil(c.embedding)
      )
    )
  end

  @doc """
  Lists exact BrainCell senses that do not yet have embeddings.

  This is intentionally ordered by id so repeated small batches are predictable.
  """
  @spec missing_cells(keyword()) :: [BrainCell.t()]
  def missing_cells(opts \\ []) do
    limit = Keyword.get(opts, :limit, 100)

    Db.all(
      from(c in BrainCell,
        where: is_nil(c.embedding),
        order_by: [asc: c.id],
        limit: ^limit
      )
    )
  end

  @doc """
  Lists exact BrainCell senses for a norm that do not yet have embeddings.
  """
  @spec missing_cells_for_norm(String.t(), keyword()) :: [BrainCell.t()]
  def missing_cells_for_norm(norm, opts \\ []) when is_binary(norm) do
    normalized = normalize_norm(norm)
    limit = Keyword.get(opts, :limit, 100)

    Db.all(
      from(c in BrainCell,
        where: c.norm == ^normalized,
        where: is_nil(c.embedding),
        order_by: [asc: c.id],
        limit: ^limit
      )
    )
  end

  @doc """
  Lists exact BrainCell senses by id prefix.

  Useful for sense-family checks such as:

      "there|noun|"

  This is id-prefix lookup, not broad norm lookup.
  """
  @spec cells_by_id_prefix(String.t(), keyword()) :: [BrainCell.t()]
  def cells_by_id_prefix(prefix, opts \\ []) when is_binary(prefix) do
    limit = Keyword.get(opts, :limit, 100)
    pattern = prefix <> "%"

    Db.all(
      from(c in BrainCell,
        where: like(c.id, ^pattern),
        order_by: [asc: c.id],
        limit: ^limit
      )
    )
  end

  @doc """
  Builds stable sense text for one BrainCell.

  This text is what an embedding producer should consume.

  Do include stable semantic fields:
  - id
  - word
  - norm
  - pos
  - type
  - definition
  - example
  - gram_function
  - synonyms
  - antonyms
  - semantic_atoms

  Do not include runtime fields:
  - activation
  - modulated_activation
  - dopamine
  - serotonin
  - timestamps
  - connections
  """
  @spec sense_text(BrainCell.t()) :: sense_text()
  def sense_text(%BrainCell{} = cell) do
    [
      scalar_field("id", cell.id),
      scalar_field("word", cell.word),
      scalar_field("norm", cell.norm),
      scalar_field("part_of_speech", cell.pos),
      scalar_field("type", cell.type),
      scalar_field("definition", cell.definition),
      scalar_field("example", cell.example),
      list_field("gram_function", cell.gram_function),
      list_field("synonyms", cell.synonyms),
      list_field("antonyms", cell.antonyms),
      list_field("semantic_atoms", cell.semantic_atoms)
    ]
    |> Enum.reject(&(&1 == ""))
    |> Enum.join("\n")
  end

  @doc """
  Returns `{cell, sense_text}` pairs for missing embeddings.

  This lets another layer do:

      pairs = Db.BrainCellEmbeddings.missing_sense_texts(limit: 32)
      texts = Enum.map(pairs, fn {_cell, text} -> text end)
      vectors = SomeEmbeddingRuntime.embed_many(texts)

  Then call `put_embedding/2` for each exact cell id.
  """
  @spec missing_sense_texts(keyword()) :: [{BrainCell.t(), sense_text()}]
  def missing_sense_texts(opts \\ []) do
    opts
    |> missing_cells()
    |> Enum.map(fn cell -> {cell, sense_text(cell)} end)
  end

  @doc """
  Stores one embedding on one exact BrainCell id.

  The vector must match `:brain_cell_embedding_dim`.

  This uses `Db.BrainCell.changeset/2`, so schema-level embedding validation
  remains the enforcement point.
  """
  @spec put_embedding(brain_cell_id(), embedding_vector() | Pgvector.t()) ::
          {:ok, BrainCell.t()} | {:error, Ecto.Changeset.t()} | {:error, :not_found}
  def put_embedding(id, vector) when is_binary(id) do
    case get_cell(id) do
      %BrainCell{} = cell ->
        put_embedding(cell, vector)

      nil ->
        {:error, :not_found}
    end
  end

  @spec put_embedding(BrainCell.t(), embedding_vector() | Pgvector.t()) ::
          {:ok, BrainCell.t()} | {:error, Ecto.Changeset.t()}
  def put_embedding(%BrainCell{} = cell, %Pgvector{} = vector) do
    cell
    |> BrainCell.changeset(%{embedding: vector})
    |> Db.update()
  end

  def put_embedding(%BrainCell{} = cell, vector) when is_list(vector) do
    with {:ok, normalized_vector} <- normalize_vector(vector) do
      cell
      |> BrainCell.changeset(%{embedding: normalized_vector})
      |> Db.update()
    end
  end

  @doc """
  Clears one embedding.

  Useful while testing embedding generation.
  """
  @spec clear_embedding(brain_cell_id()) ::
          {:ok, BrainCell.t()} | {:error, Ecto.Changeset.t()} | {:error, :not_found}
  def clear_embedding(id) when is_binary(id) do
    case get_cell(id) do
      %BrainCell{} = cell ->
        cell
        |> BrainCell.changeset(%{embedding: nil})
        |> Db.update()

      nil ->
        {:error, :not_found}
    end
  end

  @doc """
  Finds nearest embedded BrainCells to a query vector.

  This expects a persisted-compatible vector/list, not an Nx tensor.
  Runtime code should convert Nx tensors into flat lists before calling this.
  """
  @spec nearest(embedding_vector() | Pgvector.t(), keyword()) :: [map()]
  def nearest(query_vector, opts \\ [])

  def nearest(%Pgvector{} = query_vector, opts) do
    limit = Keyword.get(opts, :limit, 10)

    Db.all(
      from(c in BrainCell,
        where: not is_nil(c.embedding),
        order_by: fragment("? <-> ?", c.embedding, type(^query_vector, Pgvector.Ecto.Vector)),
        limit: ^limit,
        select: %{
          id: c.id,
          norm: c.norm,
          word: c.word,
          pos: c.pos,
          definition: c.definition,
          example: c.example,
          semantic_atoms: c.semantic_atoms,
          distance: fragment("? <-> ?", c.embedding, type(^query_vector, Pgvector.Ecto.Vector)),
          score:
            fragment(
              "1.0 / (1.0 + (? <-> ?))",
              c.embedding,
              type(^query_vector, Pgvector.Ecto.Vector)
            )
        }
      )
    )
  end

  def nearest(query_vector, opts) when is_list(query_vector) do
    with {:ok, pgvector} <- to_pgvector(query_vector) do
      nearest(pgvector, opts)
    else
      {:error, _reason} -> []
    end
  end

  @doc """
  Converts a flat numeric list into a Pgvector after dimension validation.
  """
  @spec to_pgvector(embedding_vector()) :: {:ok, Pgvector.t()} | {:error, term()}
  def to_pgvector(vector) when is_list(vector) do
    with {:ok, normalized_vector} <- normalize_vector(vector) do
      {:ok, Pgvector.new(normalized_vector)}
    end
  end

  def to_pgvector(_other), do: {:error, :invalid_vector}

  # -- private helpers --------------------------------------------------------

  defp normalize_vector(vector) when is_list(vector) do
    normalized =
      Enum.map(vector, fn
        value when is_integer(value) -> value * 1.0
        value when is_float(value) -> value
        other -> {:invalid_value, other}
      end)

    invalid = Enum.find(normalized, &match?({:invalid_value, _}, &1))

    cond do
      invalid ->
        {:error, invalid}

      length(normalized) != @embedding_dim ->
        {:error, {:wrong_embedding_size, length(normalized), expected: @embedding_dim}}

      true ->
        {:ok, normalized}
    end
  end

  defp scalar_field(_name, nil), do: ""
  defp scalar_field(_name, ""), do: ""

  defp scalar_field(name, value) do
    "#{name}: #{value}"
  end

  defp list_field(_name, nil), do: ""
  defp list_field(_name, []), do: ""

  defp list_field(name, values) when is_list(values) do
    values =
      values
      |> Enum.map(&to_string/1)
      |> Enum.map(&String.trim/1)
      |> Enum.reject(&(&1 == ""))

    case values do
      [] -> ""
      _ -> "#{name}: #{Enum.join(values, ", ")}"
    end
  end

  defp normalize_norm(value) when is_binary(value) do
    value
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/^\p{P}+/u, "")
    |> String.replace(~r/\p{P}+$/u, "")
    |> String.replace(~r/\s+/u, " ")
  end
end
