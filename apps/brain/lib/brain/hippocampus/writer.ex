defmodule Brain.Hippocampus.Writer do
  @moduledoc """
  Episodic persistence + mood priming based on SI outcomes.

  ✅ Optional episode writing to the `episodes` table
  ✅ Optional priming: small vector mood deltas on success/failure
  ✅ Fully non-blocking, fault-tolerant, and easy to override via opts/env/config

  Persistence:
    - Pass `persist: true` in opts, or enable via `EPISODES_PERSIST=on`

  Priming:
    - Pass `priming: true` in opts, or enable via `HIPPO_PRIMING=on`

  Priming vectors (overridable):
    - success: %{da: +0.02, "5ht": -0.01, glu: +0.02, ne: +0.01}
    - failure: %{da: -0.01, "5ht": +0.02, glu: 0.00, ne: +0.02}
  """

  require Logger
  alias Ecto.Adapters.SQL

  @type opts ::
          [
            {:persist, boolean()}
            | {:priming, boolean()}
            | {:priming_vectors, map()}
            | {:user_id, binary()}
            | {:embedding, any()}
          ]

  @spec maybe_persist(map(), opts) :: map()
  def maybe_persist(%{} = si, opts \\ []) do
    persist_result =
      if enabled?(opts) and repo_ready?() do
        row = build_row(si, opts)

        try do
          {n, _} = Db.insert_all("episodes", [row], on_conflict: :nothing)
          if n > 0, do: :ok, else: try_raw_sql(row)
        rescue
          _ -> :noop
        end
      else
        :noop
      end

    case persist_result do
      :ok -> emit_persist(:ok)
      {:error, reason} -> emit_persist({:error, reason})
      _ -> :noop
    end

    maybe_prime(si, opts)
    si
  rescue
    e ->
      Logger.debug("Hippo.Writer failed: #{Exception.message(e)}")
      si
  end

  # ──────── Persistence guards ─────────────

  defp enabled?(opts) do
    Keyword.get(opts, :persist) ||
      Application.get_env(:brain, :episodes_persist) ||
      System.get_env("EPISODES_PERSIST", "") |> to_bool()
  end

  defp priming_enabled?(opts) do
    Keyword.get(opts, :priming) ||
      Application.get_env(:brain, :hippo_priming) ||
      System.get_env("HIPPO_PRIMING", "") |> to_bool()
  end

  defp to_bool("1"), do: true
  defp to_bool("true"), do: true
  defp to_bool("on"), do: true
  defp to_bool("yes"), do: true
  defp to_bool(v) when is_boolean(v), do: v
  defp to_bool(_), do: false

  defp repo_ready?(),
    do: Code.ensure_loaded?(Db) and function_exported?(Db, :insert_all, 3)

  # ──────── Priming logic ─────────────

  defp maybe_prime(%{} = si, opts) do
    if priming_enabled?(opts) and Code.ensure_loaded?(Brain.MoodCore) do
      case infer_outcome(si) do
        :success -> apply_priming(:success, si, opts)
        :failure -> apply_priming(:failure, si, opts)
        _ -> :noop
      end
    else
      :noop
    end
  end

  defp apply_priming(outcome, si, opts) do
    deltas = priming_deltas(outcome, opts)

    if map_has_numbers?(deltas) do
      try do
        Brain.MoodCore.bump(deltas)
        emit_priming(:ok, outcome, deltas, si)
      rescue
        e -> emit_priming({:error, Exception.message(e)}, outcome, deltas, si)
      end
    end
  end

  defp priming_deltas(:success, opts),
    do: Map.get(priming_vectors(opts), :success, %{})

  defp priming_deltas(:failure, opts),
    do: Map.get(priming_vectors(opts), :failure, %{})

  defp priming_vectors(opts) do
    Keyword.get(opts, :priming_vectors) ||
      Application.get_env(:brain, :hippo_priming_vectors) ||
      %{
        success: %{da: 0.02, "5ht": -0.01, glu: 0.02, ne: 0.01},
        failure: %{da: -0.01, "5ht": 0.02, glu: 0.00, ne: 0.02}
      }
  end

  defp map_has_numbers?(%{} = map),
    do: Enum.any?(map, fn {_k, v} -> is_number(v) end)

  defp map_has_numbers?(_), do: false

  # ──────── Outcome heuristics ─────────────

  defp infer_outcome(si) do
    case get_in(si, [:outcome]) || get_in(si, ["outcome"]) do
      v when v in [:success, :failure, :neutral] -> v
      "success" -> :success
      "failure" -> :failure
      _ -> infer_from_confidence(si)
    end
  end

  defp infer_from_confidence(si) do
    winners = get_in(si, [:atl_slate, :winners]) || get_in(si, ["atl_slate", "winners"]) || []

    conf =
      get_in(si, [:intent, :confidence]) ||
        get_in(si, ["intent", "confidence"]) ||
        get_in(si, [:confidence]) ||
        get_in(si, ["confidence"])

    cond do
      is_list(winners) and winners != [] and is_number(conf) and conf >= 0.65 ->
        :success

      truthy?(get_in(si, [:reanalysis, :gave_up]) || get_in(si, ["reanalysis", "gave_up"])) ->
        :failure

      true ->
        :neutral
    end
  end

  defp truthy?(v) when is_boolean(v), do: v
  defp truthy?(_), do: false

  # ──────── DB row construction ─────────────

  defp build_row(si, opts) do
    %{
      user_id: user_id_from(si, opts),
      tokens: tokens_from_si(si),
      token_count: length(tokens_from_si(si)),
      si: compact_si(si),
      tags: build_tags(si),
      embedding: embedding_from(opts)
    }
  end

  defp compact_si(%{} = si), do: si

  defp user_id_from(si, opts),
    do:
      Keyword.get(opts, :user_id) ||
        get_in(si, [:user, :id]) ||
        get_in(si, ["user", "id"])

  defp embedding_from(opts) do
    case Keyword.get(opts, :embedding) do
      %_{} = v -> v
      l when is_list(l) -> Enum.map(l, &(&1 * 1.0))
      _ -> nil
    end
  end

  defp build_tags(si) do
    base = ["hippo", "auto"]
    src = si[:source] || si["source"]

    (base ++ List.wrap(src))
    |> Enum.map(&to_string/1)
    |> Enum.uniq()
  end

  defp tokens_from_si(si) do
    toks =
      si[:tokens] || si["tokens"] ||
        []
        |> Enum.flat_map(&token_extract/1)

    winners =
      get_in(si, [:atl_slate, :winners]) || get_in(si, ["atl_slate", "winners"]) ||
        []
        |> Enum.flat_map(&winner_extract/1)

    Enum.uniq(toks ++ winners)
  end

  defp token_extract(%{} = t) do
    [t[:phrase], t["phrase"], t[:lemma], t["lemma"], t[:word], t["word"]]
    |> Enum.find(&is_binary/1)
    |> then(&if(&1, do: [norm(&1)], else: []))
  end

  defp token_extract(_), do: []

  defp winner_extract(w),
    do:
      [w[:lemma], w["lemma"], w[:norm], w["norm"], w[:mwe], w["mwe"]]
      |> Enum.filter(&is_binary/1)
      |> Enum.map(&norm/1)

  defp norm(nil), do: ""
  defp norm(v) when is_binary(v), do: v |> String.downcase() |> String.trim()
  defp norm(v), do: to_string(v) |> String.downcase() |> String.trim()

  # ──────── pgvector fallback ─────────────

  defp try_raw_sql(%{embedding: emb} = row) when is_list(emb) do
    placeholders = Enum.map_join(1..length(emb), ",", &"$#{&1}")

    sql = """
    INSERT INTO episodes (user_id, tokens, token_count, si, tags, embedding, inserted_at, updated_at)
    VALUES ($#{length(emb) + 1}, $#{length(emb) + 2}, $#{length(emb) + 3},
            $#{length(emb) + 4}, $#{length(emb) + 5},
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

  # ──────── Telemetry ─────────────

  defp emit_persist(:ok),
    do: :telemetry.execute([:brain, :hippo, :persist], %{ok: 1}, %{})

  defp emit_persist({:error, reason}),
    do: :telemetry.execute([:brain, :hippo, :persist], %{error: 1}, %{reason: inspect(reason)})

  defp emit_priming(:ok, outcome, deltas, si),
    do:
      :telemetry.execute(
        [:brain, :hippo, :priming],
        Map.merge(%{applied: 1}, deltas),
        %{
          outcome: outcome,
          source: si[:source] || si["source"],
          confidence: get_in(si, [:intent, :confidence]) || get_in(si, ["intent", "confidence"]),
          winners?:
            (get_in(si, [:atl_slate, :winners]) || get_in(si, ["atl_slate", "winners"]) || []) !=
              []
        }
      )

  defp emit_priming({:error, reason}, outcome, deltas, si),
    do:
      :telemetry.execute(
        [:brain, :hippo, :priming],
        Map.merge(%{applied: 0, error: 1}, deltas),
        %{
          outcome: outcome,
          reason: reason,
          source: si[:source] || si["source"]
        }
      )
end
