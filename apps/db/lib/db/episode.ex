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
    field(:session_id, :string)
    field(:conversation_id, :string)
    field(:source, :string)
    field(:role, :string)
    field(:sentence, :string)
    field(:normalized_text, :string)
    field(:intent, :string)
    field(:confidence, :float)
    field(:tokens, {:array, :string}, default: [])
    field(:token_count, :integer, default: 0)
    field(:winners, :map, default: %{})
    field(:affect, :map, default: %{})
    field(:uncertainty, :float)
    field(:si, :map, default: %{})
    field(:meta, :map, default: %{})
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
    |> cast(attrs, [
      :user_id,
      :session_id,
      :conversation_id,
      :source,
      :role,
      :sentence,
      :normalized_text,
      :intent,
      :confidence,
      :tokens,
      :token_count,
      :winners,
      :affect,
      :uncertainty,
      :si,
      :meta,
      :tags,
      :embedding
    ])
    |> validate_required([:tokens, :si])
    |> normalize_compact_fields()
    |> normalize_tokens()
    |> normalize_si()
    |> normalize_meta()
    |> normalize_json_field(:winners, %{})
    |> normalize_json_field(:affect, %{})
    |> put_token_count()
    |> put_normalized_text()
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
    meta = Map.get(attrs, :meta, Map.get(attrs, "meta", %{}))
    si = build_si_from_slate(slate, meta)
    tokens = choose_tokens(attrs, si)
    tags = Map.get(attrs, :tags, Map.get(attrs, "tags", [])) |> ensure_string_list()
    embedding = Map.get(attrs, :embedding)

    base =
      attrs
      |> compact_attrs(si)
      |> Map.merge(%{
        si: si,
        tokens: tokens,
        tags: tags
      })
      |> maybe_put_embedding(embedding)

    base
    |> then(&changeset(%__MODULE__{}, &1))
    |> Repo.insert()
  end

  def insert(other) do
    {:error,
     change(%__MODULE__{}, %{})
     |> add_error(:base, "unsupported episode payload: #{inspect(other)}")}
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
          [
            %{
              id: any(),
              si: map(),
              tags: [String.t()],
              inserted_at: NaiveDateTime.t(),
              distance: float()
            }
          ]
  def knn(embedding, opts \\ []) do
    with {:ok, emb} <- to_pgvector(embedding) do
      k = Keyword.get(opts, :k, 8)
      user_id = Keyword.get(opts, :user_id)
      tokens_any = Keyword.get(opts, :tokens_any)
      tags_any = Keyword.get(opts, :tags_any)
      since = Keyword.get(opts, :since)

      base =
        from(e in __MODULE__,
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
        )

      scoped =
        base
        |> then(fn q -> if user_id, do: from(e in q, where: e.user_id == ^user_id), else: q end)
        |> then(fn q ->
          if is_list(tokens_any) and tokens_any != [],
            do: from(e in q, where: fragment("? && ?", e.tokens, ^tokens_any)),
            else: q
        end)
        |> then(fn q ->
          if is_list(tags_any) and tags_any != [],
            do: from(e in q, where: fragment("? && ?", e.tags, ^tags_any)),
            else: q
        end)
        |> then(fn q ->
          if match?(%NaiveDateTime{}, since),
            do: from(e in q, where: e.inserted_at >= ^since),
            else: q
        end)

      Repo.all(scoped)
    else
      {:error, reason} ->
        Logger.warning("Db.Episode.knn/2 skipped: #{reason}")
        []
    end
  end

  @type recalled :: %{
          id: any(),
          si: map(),
          tags: [String.t()],
          inserted_at: NaiveDateTime.t(),
          score: float(),
          components: %{jaccard: float(), recency: float()}
        }

  @doc """
  Recall episodes by cue tokens (fast overlap query + recency scoring).

  This is the “human” path: cues → candidate pool → rank by overlap + freshness.

  ## Options
    * `:k`            – return top k (default 8)
    * `:pool`         – candidate pool size before scoring (default 50)
    * `:user_id`      – optional scope by user
    * `:tags_any`     – optional overlap filter on tags
    * `:since`        – optional minimum inserted_at
    * `:half_life_ms` – recency decay half-life (default 24h)
    * `:w_jaccard`    – weight for overlap (default 0.70)
    * `:w_recency`    – weight for freshness (default 0.30)
  """
  @spec recall([String.t()], keyword()) :: [recalled()]
  def recall(tokens, opts \\ []) when is_list(tokens) do
    cue = normalize_token_list(tokens)

    if cue == [] do
      []
    else
      k = Keyword.get(opts, :k, 8)
      pool = Keyword.get(opts, :pool, 50)
      user_id = Keyword.get(opts, :user_id)
      tags_any = Keyword.get(opts, :tags_any)
      since = Keyword.get(opts, :since)
      half_life_ms = Keyword.get(opts, :half_life_ms, 86_400_000)
      wj = Keyword.get(opts, :w_jaccard, 0.70)
      wr = Keyword.get(opts, :w_recency, 0.30)

      base =
        from(e in __MODULE__,
          where: fragment("? && ?", e.tokens, ^cue),
          select: %{
            id: e.id,
            si: e.si,
            tags: e.tags,
            inserted_at: e.inserted_at,
            tokens: e.tokens
          },
          order_by: [desc: e.inserted_at],
          limit: ^pool
        )

      scoped =
        base
        |> then(fn q -> if user_id, do: from(e in q, where: e.user_id == ^user_id), else: q end)
        |> then(fn q ->
          if is_list(tags_any) and tags_any != [],
            do: from(e in q, where: fragment("? && ?", e.tags, ^ensure_string_list(tags_any))),
            else: q
        end)
        |> then(fn q ->
          if match?(%NaiveDateTime{}, since),
            do: from(e in q, where: e.inserted_at >= ^since),
            else: q
        end)

      now = NaiveDateTime.utc_now()

      scoped
      |> Repo.all()
      |> Enum.map(fn row ->
        j = jaccard(row.tokens || [], cue)
        r = recency(now, row.inserted_at, half_life_ms)
        s = wj * j + wr * r

        %{
          id: row.id,
          si: row.si,
          tags: row.tags || [],
          inserted_at: row.inserted_at,
          score: s,
          components: %{jaccard: j, recency: r}
        }
      end)
      |> Enum.sort_by(& &1.score, :desc)
      |> Enum.take(k)
    end
  end

  defp normalize_token_list(list) when is_list(list) do
    list
    |> Enum.map(&to_string/1)
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
    |> Enum.map(&String.downcase/1)
    |> Enum.uniq()
  end

  defp recency(now, inserted_at, half_life_ms) do
    age_ms = max(0, NaiveDateTime.diff(now, inserted_at, :millisecond))
    :math.exp(-age_ms / max(1.0, half_life_ms * 1.0))
  end

  defp jaccard(a, b) do
    sa = MapSet.new(Enum.map(a, &String.downcase(to_string(&1))))
    sb = MapSet.new(Enum.map(b, &String.downcase(to_string(&1))))
    inter = MapSet.size(MapSet.intersection(sa, sb))
    union = MapSet.size(MapSet.union(sa, sb))
    if union == 0, do: 0.0, else: inter / union
  end

  # ───────────────────────── Changeset helpers ─────────────────────────

  defp normalize_compact_fields(changeset) do
    changeset
    |> stringify_field(:session_id)
    |> stringify_field(:conversation_id)
    |> stringify_field(:source)
    |> stringify_field(:role)
    |> stringify_field(:sentence)
    |> stringify_field(:normalized_text)
    |> stringify_field(:intent)
  end

  defp stringify_field(changeset, field) do
    case get_change(changeset, field) do
      nil ->
        changeset

      value ->
        value =
          value
          |> to_string()
          |> String.trim()

        if value == "",
          do: put_change(changeset, field, nil),
          else: put_change(changeset, field, value)
    end
  end

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

  defp normalize_meta(changeset) do
    case get_change(changeset, :meta) do
      nil ->
        changeset

      %{} = meta ->
        put_change(changeset, :meta, to_plain_map(meta))

      other ->
        add_error(changeset, :meta, "must be a map, got: #{inspect(other)}")
    end
  end

  defp normalize_json_field(changeset, field, default) do
    case get_change(changeset, field, get_field(changeset, field)) do
      nil ->
        put_change(changeset, field, default)

      value when is_map(value) or is_list(value) ->
        put_change(changeset, field, to_plain_map(value))

      other ->
        add_error(changeset, field, "must be a JSON map/list, got: #{inspect(other)}")
    end
  end

  defp put_token_count(changeset) do
    tokens = get_field(changeset, :tokens) || []
    put_change(changeset, :token_count, length(tokens))
  end

  defp put_normalized_text(changeset) do
    existing = get_field(changeset, :normalized_text)
    sentence = get_field(changeset, :sentence)

    cond do
      is_binary(existing) and existing != "" ->
        changeset

      is_binary(sentence) and sentence != "" ->
        put_change(changeset, :normalized_text, normalize_text(sentence))

      true ->
        changeset
    end
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

  # ───────────────────────── Normalizers & builders ─────────────────────────

  defp normalize_direct_attrs(attrs) do
    si = Map.get(attrs, :si) || Map.get(attrs, "si") || %{}
    compact = compact_attrs(attrs, si)

    compact
    |> Map.merge(%{
      si: Map.get(attrs, :si) || Map.get(attrs, "si") || %{},
      tokens: attrs |> Map.get(:tokens, Map.get(attrs, "tokens", [])) |> ensure_string_list(),
      tags: attrs |> Map.get(:tags, Map.get(attrs, "tags", [])) |> ensure_string_list()
    })
    |> maybe_put_embedding(Map.get(attrs, :embedding, Map.get(attrs, "embedding")))
  end

  defp maybe_put_embedding(map, nil), do: map
  defp maybe_put_embedding(map, %Pgvector{} = v), do: Map.put(map, :embedding, v)

  defp maybe_put_embedding(map, list) when is_list(list),
    do: Map.put(map, :embedding, Pgvector.new(list))

  defp maybe_put_embedding(map, _), do: map

  defp ensure_string_list(v) when is_list(v), do: Enum.map(v, &to_string/1)
  defp ensure_string_list(_), do: []

  defp build_si_from_slate(slate, meta) do
    %{
      "slate" => to_plain_map(slate),
      "meta" => to_plain_map(meta || %{})
    }
  end

  defp compact_attrs(attrs, si) do
    meta = Map.get(attrs, :meta) || Map.get(attrs, "meta") || si_map_get(si, :meta, %{})
    slate = Map.get(attrs, :slate) || Map.get(attrs, "slate") || si_map_get(si, :slate, %{})

    sentence =
      first_present([
        map_get_any(attrs, :sentence),
        map_get_any(si, :sentence),
        map_get_any(meta, :sentence)
      ])

    intent =
      first_present([
        map_get_any(attrs, :intent),
        map_get_any(si, :intent),
        map_get_any(meta, :intent)
      ])

    confidence =
      first_number([
        map_get_any(attrs, :confidence),
        map_get_any(si, :confidence),
        map_get_any(meta, :confidence)
      ])

    %{
      user_id: map_get_any(attrs, :user_id),
      session_id:
        first_present([
          map_get_any(attrs, :session_id),
          map_get_any(si, :session_id),
          map_get_any(meta, :session_id)
        ]),
      conversation_id:
        first_present([
          map_get_any(attrs, :conversation_id),
          map_get_any(si, :conversation_id),
          map_get_any(meta, :conversation_id)
        ]),
      source:
        first_present([
          map_get_any(attrs, :source),
          map_get_any(si, :source),
          map_get_any(meta, :source)
        ]),
      role:
        first_present([
          map_get_any(attrs, :role),
          map_get_any(si, :role),
          map_get_any(meta, :role)
        ]),
      sentence: sentence,
      normalized_text:
        first_present([map_get_any(attrs, :normalized_text), normalize_text(sentence)]),
      intent: intent || "unknown",
      confidence: confidence,
      winners: compact_winners(map_get_any(attrs, :winners) || map_get_any(slate, :winners)),
      affect:
        map_get_any(attrs, :affect) ||
          map_get_any(si, :emotion) ||
          map_get_any(meta, :emotion) ||
          %{},
      uncertainty:
        first_number([
          map_get_any(attrs, :uncertainty),
          map_get_any(si, :uncertainty),
          map_get_any(meta, :uncertainty)
        ]),
      meta: compact_meta(meta)
    }
  end

  defp compact_meta(meta) when is_map(meta) do
    meta
    |> Map.take([
      :kind,
      "kind",
      :key,
      "key",
      :value,
      "value",
      :subject,
      "subject",
      :self?,
      "self?",
      :autobiographical?,
      "autobiographical?",
      :response_tone,
      "response_tone",
      :response_meta,
      "response_meta"
    ])
  end

  defp compact_meta(_), do: %{}

  defp compact_winners(%{"items" => items}) when is_list(items), do: %{"items" => items}
  defp compact_winners(%{items: items}) when is_list(items), do: %{"items" => items}
  defp compact_winners(list) when is_list(list), do: %{"items" => Enum.take(list, 12)}
  defp compact_winners(%{} = map), do: map
  defp compact_winners(_), do: %{}

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

  defp map_get_any(map, key) when is_map(map) and is_atom(key) do
    Map.get(map, key) || Map.get(map, Atom.to_string(key))
  end

  defp map_get_any(_, _), do: nil

  defp si_map_get(map, key, default) when is_map(map) and is_atom(key) do
    map_get_any(map, key) || default
  end

  defp si_map_get(_, _, default), do: default

  defp first_present(values) when is_list(values) do
    Enum.find_value(values, fn
      nil ->
        nil

      value when is_binary(value) ->
        value = String.trim(value)
        if value == "", do: nil, else: value

      value ->
        value
        |> to_string()
        |> String.trim()
        |> case do
          "" -> nil
          text -> text
        end
    end)
  end

  defp first_number(values) when is_list(values) do
    Enum.find_value(values, fn
      value when is_number(value) -> value * 1.0
      _ -> nil
    end)
  end

  defp normalize_text(nil), do: nil

  defp normalize_text(value) do
    value
    |> to_string()
    |> String.downcase()
    |> String.replace(~r/\s+/u, " ")
    |> String.trim()
    |> case do
      "" -> nil
      text -> text
    end
  end

  # Core recursive converter: Handles structs, maps, lists, MapSets, and tuples.
  defp to_plain_map(%MapSet{} = set), do: Enum.to_list(set)

  defp to_plain_map(%{__struct__: _} = struct) do
    struct
    |> Map.from_struct()
    |> Map.delete(:__meta__)
    |> Map.new(fn {k, v} -> {k, to_plain_map(v)} end)
    |> convert_non_json_types()
  end

  defp to_plain_map(tuple) when is_tuple(tuple),
    do: tuple |> Tuple.to_list() |> Enum.map(&to_plain_map/1)

  defp to_plain_map(map) when is_map(map), do: Map.new(map, fn {k, v} -> {k, to_plain_map(v)} end)
  defp to_plain_map(list) when is_list(list), do: Enum.map(list, &to_plain_map/1)
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

  @doc """
  Fetch most recent episodes (newest first).

  Used by Brain.Hippocampus to warm its in-memory window after reboot.
  """
  @spec recent(pos_integer()) :: [t()]
  def recent(limit \\ 200) when is_integer(limit) and limit > 0 do
    import Ecto.Query, only: [from: 2]

    from(e in __MODULE__,
      order_by: [desc: e.inserted_at],
      limit: ^limit
    )
    |> Db.all()
  end

  @doc """
  Fetch all persisted episodes, newest first.
  """
  @spec list_all() :: [t()]
  def list_all do
    import Ecto.Query, only: [from: 2]

    from(e in __MODULE__,
      order_by: [desc: e.inserted_at]
    )
    |> Db.all()
  end

  @spec insert_row(map()) :: {:ok, integer()} | {:error, term()}
  def insert_row(%{} = row) do
    now = NaiveDateTime.utc_now() |> NaiveDateTime.truncate(:second)

    row =
      row
      |> Map.put_new(:inserted_at, now)
      |> Map.put_new(:updated_at, now)

    try do
      {n, _} = Db.insert_all("episodes", [row], on_conflict: :nothing)
      {:ok, n}
    rescue
      e -> {:error, e}
    end
  end
end
