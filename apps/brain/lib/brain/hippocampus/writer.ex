defmodule Brain.Hippocampus.Writer do
  @moduledoc """
  Optional persistence of newly-encoded episodes to the `episodes` table.

  • Safe by default (OFF). Enable with:
      - opts:   persist: true
      - or env: EPISODES_PERSIST=on|true|1
      - or app: Application.put_env(:brain, :episodes_persist, :on)

  • Does not alter the SI. Returns SI unchanged.

  • Uses your Repo module `Db` (not Db.Repo).
  """

  require Logger
  alias Ecto.Adapters.SQL
  # NOTE: your repo module is `Db`
  # (no alias needed; we will call Db.insert_all / SQL.query(Db, ...))

  @spec maybe_persist(map(), keyword()) :: map()
  def maybe_persist(%{} = si, opts \\ []) do
    if enabled?(opts) and repo_ready?() do
      row = build_row(si, opts)

      # Try Ecto insert_all first (works if pgvector type is configured).
      ok? =
        try do
          {n, _} = Db.insert_all("episodes", [row], on_conflict: :nothing)
          n > 0
        rescue
          _ -> false
        end

      # If Ecto path failed and we have an embedding list, fall back to raw SQL.
      if not ok? and is_list(row.embedding) do
        try_raw_sql(row)
      else
        :ok
      end
      |> case do
        :ok -> emit(:ok)
        {:error, reason} -> emit({:error, reason})
      end
    end

    si
  rescue
    e ->
      Logger.debug("Hippo.Writer persist skipped: #{Exception.message(e)}")
      si
  end

  # ── enable/guards ───────────────────────────────────────────────────

  defp enabled?(opts) do
    opt =
      case Keyword.get(opts, :persist, nil) do
        nil ->
          env =
            Application.get_env(:brain, :episodes_persist, nil) ||
              System.get_env("EPISODES_PERSIST", "")

          to_bool(env)
        v -> to_bool(v)
      end

    opt == true
  end

  defp repo_ready?(),
    do: Code.ensure_loaded?(Db) and function_exported?(Db, :insert_all, 3)

  defp to_bool(v) when is_binary(v) do
    case String.downcase(String.trim(v)) do
      s when s in ["1", "true", "on", "yes"] -> true
      _ -> false
    end
  end
  defp to_bool(v) when is_boolean(v), do: v
  defp to_bool(_), do: false

  # ── row builders ────────────────────────────────────────────────────

  defp build_row(%{} = si, opts) do
    tokens = tokens_from_si(si)
    tags   = build_tags(si, opts)

    %{
      user_id: user_id_from(si, opts),
      tokens: tokens,
      token_count: length(tokens),
      si: compact_si(si),
      tags: tags,
      embedding: embedding_from(opts)
    }
  end

  defp user_id_from(si, opts) do
    cond do
      is_binary(Keyword.get(opts, :user_id)) -> Keyword.get(opts, :user_id)
      is_binary(get_in(si, [:user, :id])) -> get_in(si, [:user, :id])
      is_binary(get_in(si, ["user", "id"])) -> get_in(si, ["user", "id"])
      true -> nil
    end
  end

  # Prefer explicit embedding; allow nil (index is partial on NOT NULL)
  defp embedding_from(opts) do
    case Keyword.get(opts, :embedding) do
      %_{} = v -> v
      l when is_list(l) -> Enum.map(l, &(&1 * 1.0))
      _ -> nil
    end
  end

  defp build_tags(si, _opts) do
    base =
      []
      |> maybe_put("auto")
      |> maybe_put("hippo")

    src =
      case (si[:source] || si["source"]) do
        s when is_atom(s) -> Atom.to_string(s)
        s when is_binary(s) -> s
        _ -> nil
      end

    base
    |> maybe_put(src)
    |> Enum.uniq()
  end

  defp maybe_put(list, nil), do: list
  defp maybe_put(list, ""), do: list
  defp maybe_put(list, v), do: [v | list]

  defp tokens_from_si(%{} = si) do
    toks =
      (si[:tokens] || si["tokens"] || [])
      |> Enum.flat_map(fn
        %{} = t ->
          v = t[:phrase] || t["phrase"] || t[:lemma] || t["lemma"] || t[:word] || t["word"]
          if is_binary(v), do: [norm(v)], else: []
        _ ->
          []
      end)

    winners =
      get_in(si, [:atl_slate, :winners]) || get_in(si, ["atl_slate", "winners"]) || []
      |> Enum.flat_map(fn w ->
        [w[:lemma], w["lemma"], w[:norm], w["norm"], w[:mwe], w["mwe"]]
      end)
      |> Enum.filter(&is_binary/1)
      |> Enum.map(&norm/1)

    (toks ++ winners) |> Enum.uniq()
  end

  defp compact_si(%{} = si), do: si

  defp norm(nil), do: ""
  defp norm(v) when is_binary(v),
    do: v |> String.downcase() |> String.replace(~r/\s+/u, " ") |> String.trim()
  defp norm(v),
    do:
      v
      |> Kernel.to_string()
      |> String.downcase()
      |> String.replace(~r/\s+/u, " ")
      |> String.trim()

  # ── raw SQL fallback for pgvector ───────────────────────────────────

  defp try_raw_sql(%{embedding: emb} = row) when is_list(emb) do
    # Build a parameterized query that casts ARRAY[...] to vector
    placeholders = Enum.map_join(1..length(emb), ",", &"$#{&1}")
    sql = """
    INSERT INTO episodes (user_id, tokens, token_count, si, tags, embedding, inserted_at, updated_at)
    VALUES ($#{length(emb) + 1}::uuid, $#{length(emb) + 2}::text[], $#{length(emb) + 3}::int,
            $#{length(emb) + 4}::jsonb, $#{length(emb) + 5}::text[],
            ARRAY[#{placeholders}]::vector,
            now(), now())
    ON CONFLICT DO NOTHING
    """

    params =
      emb ++
        [
          row.user_id,
          row.tokens,
          row.token_count,
          row.si,
          row.tags
        ]

    case SQL.query(Db, sql, params) do
      {:ok, _} -> :ok
      {:error, reason} -> {:error, reason}
    end
  end

  defp try_raw_sql(_), do: :ok

  # ── telemetry ───────────────────────────────────────────────────────

  defp emit(:ok) do
    if Code.ensure_loaded?(:telemetry) and function_exported?(:telemetry, :execute, 3) do
      :telemetry.execute([:brain, :hippo, :persist], %{ok: 1}, %{})
    end
  end

  defp emit({:error, reason}) do
    if Code.ensure_loaded?(:telemetry) and function_exported?(:telemetry, :execute, 3) do
      :telemetry.execute([:brain, :hippo, :persist], %{error: 1}, %{reason: inspect(reason)})
    end
  end
end

