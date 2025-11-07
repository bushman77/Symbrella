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

  # Your repo module is `Db` (not Db.Repo). Alias it to keep Repo.* calls intact.
  alias Db, as: Repo

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

  # ───────────────────────── Public API (used by Hippocampus) ─────────────────────────

  @doc """
  Insert an episode. Accepts either:
    • `%{si: map(), tokens: [string()], tags: [string()], embedding: list|%Pgvector{}}`, or
    • Hippocampus payload `%{at, slate, meta, norms}` which we adapt.

  Returns `{:ok, %Db.Episode{}} | {:error, Ecto.Changeset.t()}`.
  """
  @spec insert(map()) :: {:ok, t()} | {:error, Ecto.Changeset.t()}
  def insert(%{si: _} = attrs) when is_map(attrs) do
    attrs
    |> normalize_direct_attrs()
    |> then(&changeset(%__MODULE__{}, &1))
    |> Repo.insert()
  end

  def insert(%{slate: slate} = attrs) when is_map(attrs) do
    # Adapt Hippocampus payload → episodes row
    si        = build_si_from_slate(slate, Map.get(attrs, :meta, %{}))
    tokens    = choose_tokens(attrs, si)
    tags      = Map.get(attrs, :tags, Map.get(attrs, "tags", [])) |> ensure_string_list()
    embedding = Map.get(attrs, :embedding)

    base =
      %{
        si: si,
        tokens: tokens,
        tags: tags
      }
      |> maybe_put_embedding(embedding)

    base
    |> then(&changeset(%__MODULE__{}, &1))
    |> Repo.insert()
  end

  def insert(other) do
    {:error, change(%__MODULE__{}, %{}) |> add_error(:base, "unsupported episode payload: #{inspect(other)}")}
  end

  @doc """
  "Enqueue" a write. If you don’t have a job runner yet, we just perform a best-effort
  synchronous insert and return `:ok`. This keeps Hippocampus happy without adding infra.
  """
  @spec enqueue_write(map()) :: :ok
  def enqueue_write(attrs) when is_map(attrs) do
    _ = insert(attrs)
    :ok
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
        |> then(fn q -> if is_list(tags_any)    and tags_any    != [], do: from(e in q, where: fragment("? && ?", e.tags,   ^tags_any)), else: q end)
        |> then(fn q -> if match?(%NaiveDateTime{}, since), do: from(e in q, where: e.inserted_at >= ^since), else: q end)

      Repo.all(scoped)
    else
      {:error, reason} ->
        Logger.warning("Db.Episode.knn/2 skipped: #{reason}")
        []
    end
  end

  # ───────────────────────── Changeset helpers ─────────────────────────

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

  defp normalize_si(changeset) do
    case get_change(changeset, :si) do
      nil ->
        changeset

      %{} = si ->
        normalized = to_plain_map(si)
        Logger.debug("Si normalized: #{inspect(Map.keys(normalized), limit: 10)}")
        put_change(changeset, :si, normalized)

      other ->
        add_error(changeset, :si, "must be a map, got: #{inspect(other)}")
    end
  end

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
            add_error(changeset, :embedding, "embedding length must be #{@embedding_dim} (got #{length(list)})")

          _ ->
            add_error(changeset, :embedding, "invalid pgvector value")
        end

      is_list(val) ->
        if length(val) == @embedding_dim do
          changeset
        else
          add_error(changeset, :embedding, "embedding length must be #{@embedding_dim} (got #{length(val)})")
        end

      true ->
        add_error(changeset, :embedding, "embedding must be a list of floats or a %Pgvector{}")
    end
  end

  # ───────────────────────── Normalizers & builders ─────────────────────────

  defp normalize_direct_attrs(attrs) do
    %{
      si: Map.get(attrs, :si) || Map.get(attrs, "si") || %{},
      tokens: attrs |> Map.get(:tokens, Map.get(attrs, "tokens", [])) |> ensure_string_list(),
      tags: attrs |> Map.get(:tags, Map.get(attrs, "tags", [])) |> ensure_string_list()
    }
    |> maybe_put_embedding(Map.get(attrs, :embedding, Map.get(attrs, "embedding")))
  end

  defp maybe_put_embedding(map, nil), do: map
  defp maybe_put_embedding(map, %Pgvector{} = v), do: Map.put(map, :embedding, v)
  defp maybe_put_embedding(map, list) when is_list(list), do: Map.put(map, :embedding, Pgvector.new(list))
  defp maybe_put_embedding(map, _), do: map

  defp ensure_string_list(v) when is_list(v), do: Enum.map(v, &to_string/1)
  defp ensure_string_list(_), do: []

  defp build_si_from_slate(slate, meta) do
    %{
      "slate" => to_plain_map(slate),
      "meta"  => to_plain_map(meta || %{})
    }
  end

  defp choose_tokens(attrs, si) do
    cond do
      is_list(attrs[:norms]) ->
        ensure_string_list(attrs[:norms])

      is_list(Map.get(attrs, "norms")) ->
        ensure_string_list(Map.get(attrs, "norms"))

      tokens = get_in(si, ["slate", "tokens"]) ->
        ensure_string_list(tokens)

      true ->
        []
    end
  end

  # Core recursive converter: Handles structs, maps, lists, MapSets, and tuples.
  defp to_plain_map(%{__struct__: _} = struct) do
    struct
    |> Map.from_struct()
    |> Map.delete(:__meta__)
    |> Map.new(fn {k, v} -> {k, to_plain_map(v)} end)
    |> convert_non_json_types()
  end

  defp to_plain_map(tuple) when is_tuple(tuple), do: tuple |> Tuple.to_list() |> Enum.map(&to_plain_map/1)
  defp to_plain_map(map) when is_map(map), do: Map.new(map, fn {k, v} -> {k, to_plain_map(v)} end)
  defp to_plain_map(list) when is_list(list), do: Enum.map(list, &to_plain_map/1)
  defp to_plain_map(%MapSet{} = set), do: Enum.to_list(set)
  defp to_plain_map(value), do: value

  # Specialized handling for known non-JSON nests in si (e.g., activation_summary, trace)
  defp convert_non_json_types(map) when is_map(map) do
    map
    |> Map.update(:activation_summary, %{}, fn summary ->
      Map.update(summary, :db_hits, [], fn hits -> to_plain_map(hits) end)
    end)
    |> Map.update(:trace, [], fn trace -> Enum.map(trace, &to_plain_map/1) end)
    |> Map.update(:active_cells, [], fn cells -> Enum.map(cells, &to_plain_map/1) end)
  end
  defp convert_non_json_types(value), do: value

  # ───────────────────────── Utilities ─────────────────────────

  defp to_pgvector(%Pgvector{} = v), do: {:ok, v}
  defp to_pgvector(list) when is_list(list), do: {:ok, Pgvector.new(list)}
  defp to_pgvector(other), do: {:error, "invalid embedding param: #{inspect(other)}"}
end

