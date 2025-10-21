defmodule Db.Episode do
  @moduledoc """
  Ecto schema for hippocampus episodes (persistent slates).

  Notes
  -----
  • `embedding` uses `Pgvector.Ecto.Vector`; during cast it becomes a `%Pgvector{}`.
  • We validate that embedding is either a list or `%Pgvector{}` and that its length
    matches `@embedding_dim`.
  • `token_count` is derived from the (possibly updated) `:tokens` field so it stays
    consistent even if callers forget to set it.
  • `si` is normalized to a plain map, recursively converting structs (e.g., `Core.SemanticInput`, `%Db.BrainCell{}`) and data structures (e.g., `MapSet` to list) for JSONB serialization.

  Query helpers
  -------------
  • `knn/2` performs nearest-neighbor search using pgvector cosine distance:
      distance = embedding <-> ^query
      similarity = 1.0 - distance
  """

  use Ecto.Schema
  import Ecto.Changeset
  import Ecto.Query, only: [from: 2]
  require Logger

  alias Db.Repo

  @type t :: %__MODULE__{}
  @embedding_dim Application.compile_env(:db, :embedding_dim, 1536)

  schema "episodes" do
    field(:user_id, :binary_id)
    field(:tokens, {:array, :string}, default: [])
    field(:token_count, :integer, default: 0)
    field(:si, :map, default: %{})
    field(:embedding, Pgvector.Ecto.Vector)
    field(:tags, {:array, :string}, default: [])
    timestamps(type: :naive_datetime_usec)
  end

  @doc """
  Build a changeset for an episode.
  - Ensures `:tokens` is an array of strings (coerces with `to_string/1`).
  - Derives `:token_count` from `:tokens`.
  - Validates `:embedding` length for both list and `%Pgvector{}`.
  - Normalizes `:si` to a plain map for JSONB serialization.
  """
  @spec changeset(t(), map()) :: Ecto.Changeset.t()
  def changeset(ep, attrs) do
    ep
    |> cast(attrs, [:user_id, :tokens, :token_count, :si, :tags, :embedding])
    |> validate_required([:tokens, :si])
    |> normalize_tokens()
    |> normalize_si()
    |> put_token_count()
    |> validate_embedding_shape()
  end

  # --- Helpers ---------------------------------------------------------------

  # Ensure tokens is a list of strings. Do not lowercase here (that lives in the
  # context) to keep this schema side-effect free. Safe to call even if nil.
  defp normalize_tokens(changeset) do
    case get_change(changeset, :tokens) do
      nil ->
        changeset

      list when is_list(list) ->
        put_change(changeset, :tokens, Enum.map(list, &to_string/1))

      other ->
        add_error(changeset, :tokens, "must be a list of strings, got: #{inspect(other)}")
    end
  end

  # Recursively convert si to a plain map, stripping structs and converting non-JSON types.
  defp normalize_si(changeset) do
    case get_change(changeset, :si) do
      nil ->
        changeset

      %{} = si ->
        normalized = to_plain_map(si)
        # Debug: Check top-level keys
        Logger.debug("Si normalized: #{inspect(Map.keys(normalized), limit: 10)}")
        put_change(changeset, :si, normalized)

      other ->
        add_error(changeset, :si, "must be a map, got: #{inspect(other)}")
    end
  end

  # Core recursive converter: Handles structs, maps, lists, MapSets, and tuples.
  defp to_plain_map(%{__struct__: _} = struct) do
    # Strip __struct__ and __meta__, recurse on nests
    struct
    |> Map.from_struct()
    # Drop Ecto metadata
    |> Map.delete(:__meta__)
    |> Map.new(fn {k, v} ->
      {k, to_plain_map(v)}
    end)
    |> convert_non_json_types()
  end

  defp to_plain_map(tuple) when is_tuple(tuple) do
    # Convert tuples (e.g., span: {0, 11} or trace tuples) to lists
    Tuple.to_list(tuple) |> Enum.map(&to_plain_map/1)
  end

  defp to_plain_map(map) when is_map(map) do
    # Recurse on nested maps (non-structs)
    Map.new(map, fn {k, v} ->
      {k, to_plain_map(v)}
    end)
  end

  defp to_plain_map(list) when is_list(list) do
    # Recurse on lists
    Enum.map(list, &to_plain_map/1)
  end

  defp to_plain_map(%MapSet{} = set) do
    # Convert MapSet to list for JSON
    Enum.to_list(set)
  end

  # Fallback: Leave primitives as-is
  defp to_plain_map(value), do: value

  # Specialized handling for known non-JSON nests in si (e.g., activation_summary, trace)
  defp convert_non_json_types(map) when is_map(map) do
    map
    |> Map.update(:activation_summary, %{}, fn summary ->
      Map.update(summary, :db_hits, [], fn hits ->
        # Ensures MapSet → list
        to_plain_map(hits)
      end)
    end)
    |> Map.update(:trace, [], fn trace ->
      Enum.map(trace, fn item ->
        # Handles tuples → lists or maps
        to_plain_map(item)
      end)
    end)
    |> Map.update(:active_cells, [], fn cells ->
      # Ensures BrainCell structs → plain maps
      Enum.map(cells, &to_plain_map/1)
    end)
  end

  defp convert_non_json_types(value), do: value

  defp put_token_count(changeset) do
    tokens = get_field(changeset, :tokens) || []
    put_change(changeset, :token_count, length(tokens))
  end

  defp validate_embedding_shape(changeset) do
    val = get_change(changeset, :embedding, get_field(changeset, :embedding))

    cond do
      is_nil(val) ->
        changeset

      match?(%Pgvector{}, val) ->
        case Pgvector.to_list(val) do
          list when is_list(list) and length(list) == @embedding_dim ->
            changeset

          list when is_list(list) ->
            add_error(
              changeset,
              :embedding,
              "embedding length must be #{@embedding_dim} (got #{length(list)})"
            )

          _ ->
            add_error(changeset, :embedding, "invalid pgvector value")
        end

      is_list(val) ->
        if length(val) == @embedding_dim do
          changeset
        else
          add_error(
            changeset,
            :embedding,
            "embedding length must be #{@embedding_dim} (got #{length(val)})"
          )
        end

      true ->
        add_error(changeset, :embedding, "embedding must be a list of floats or a %Pgvector{}")
    end
  end

  # ───────────────────────── Query helpers (KNN) ─────────────────────────

  @doc """
  Nearest-neighbor search by `embedding` using pgvector **cosine** distance.

  ## Options
    * `:k`           – number of neighbors (default 8)
    * `:user_id`     – scope by user (optional)
    * `:tokens_any`  – list of tokens; requires overlap via `tokens && ^list` (optional)
    * `:tags_any`    – list of tags; requires overlap via `tags && ^list` (optional)
    * `:since`       – minimum `inserted_at` (NaiveDateTime) (optional)

  ## Returns
    A list of maps (lightweight payload for recall):
    %{
      id:           episode_id,
      si:           map(),
      tags:         [string()],
      inserted_at:  NaiveDateTime.t(),
      distance:     float()   # cosine distance in 0..2 (similarity = 1 - distance)
    }

  Notes
  -----
  • Accepts a raw list(float) or `%Pgvector{}` for `embedding`.
  • Uses `type(^emb, Pgvector.Ecto.Vector)` so Postgres treats the param as `vector`.
  """
  @spec knn([number()] | Pgvector.t(), keyword()) ::
          [%{id: any(), si: map(), tags: [String.t()], inserted_at: NaiveDateTime.t(), distance: float()}]
  def knn(embedding, opts \\ []) do
    with {:ok, emb} <- to_pgvector(embedding) do
      k          = Keyword.get(opts, :k, 8)
      user_id    = Keyword.get(opts, :user_id)
      tokens_any = Keyword.get(opts, :tokens_any)
      tags_any   = Keyword.get(opts, :tags_any)
      since      = Keyword.get(opts, :since)

      base =
        from e in __MODULE__,
          where: not is_nil(e.embedding),
          select: %{
            id: e.id,
            si: e.si,
            tags: e.tags,
            inserted_at: e.inserted_at,
            distance: fragment("? <-> ?", e.embedding, type(^emb, Pgvector.Ecto.Vector))
          },
          order_by: fragment("? <-> ?", e.embedding, type(^emb, Pgvector.Ecto.Vector)),
          limit: ^k

      scoped =
        base
        |> then(fn q -> if user_id, do: from(e in q, where: e.user_id == ^user_id), else: q end)
        |> then(fn q -> if is_list(tokens_any) and tokens_any != [], do: from(e in q, where: fragment("? && ?", e.tokens, ^tokens_any)), else: q end)
        |> then(fn q -> if is_list(tags_any) and tags_any != [], do: from(e in q, where: fragment("? && ?", e.tags, ^tags_any)), else: q end)
        |> then(fn q -> if match?(%NaiveDateTime{}, since), do: from(e in q, where: e.inserted_at >= ^since), else: q end)

      Repo.all(scoped)
    else
      {:error, reason} ->
        Logger.warning("Db.Episode.knn/2 skipped: #{reason}")
        []
    end
  end

  defp to_pgvector(%Pgvector{} = v), do: {:ok, v}
  defp to_pgvector(list) when is_list(list), do: {:ok, Pgvector.new(list)}
  defp to_pgvector(other), do: {:error, "invalid embedding param: #{inspect(other)}"}
end

