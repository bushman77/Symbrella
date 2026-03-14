defmodule Brain.Hippocampus.Config do
  @moduledoc """
  Defaults and normalizers for Hippocampus.

  Adds a `:recall_source` knob to choose the recall backend:
    • :memory — current in-memory window (default)
    • :db     — pgvector-backed recall via Db.Episode
    • :hybrid — merge memory + db results
  """

  @default_keep 300
  @default_half_life 300_000
  @default_recall_limit 3
  @default_min_jaccard 0.0
  @default_recall_source :memory

  @type recall_source :: :memory | :db | :hybrid

  @spec defaults() :: %{
          window_keep: pos_integer(),
          half_life_ms: pos_integer(),
          recall_limit: pos_integer(),
          min_jaccard: float(),
          recall_source: recall_source()
        }
  def defaults do
    %{
      window_keep: @default_keep,
      half_life_ms: @default_half_life,
      recall_limit: @default_recall_limit,
      min_jaccard: @default_min_jaccard,
      recall_source: @default_recall_source
    }
  end

  @spec normalize_keep(any()) :: pos_integer()
  def normalize_keep(k) when is_integer(k) and k > 0, do: k
  def normalize_keep(_), do: @default_keep

  @spec normalize_half_life(any()) :: pos_integer()
  def normalize_half_life(h) when is_integer(h) and h > 0, do: h
  def normalize_half_life(_), do: @default_half_life

  @spec normalize_limit(any()) :: pos_integer()
  def normalize_limit(k) when is_integer(k) and k > 0, do: k
  def normalize_limit(_), do: @default_recall_limit

  @spec normalize_min_jaccard(any()) :: float()
  def normalize_min_jaccard(x) when is_number(x) and x >= 0 and x <= 1, do: x * 1.0
  def normalize_min_jaccard(_), do: @default_min_jaccard

  @doc """
  Normalize a recall source. Accepts atoms (:memory | :db | :hybrid)
  or strings ("memory" | "db" | "hybrid" | short forms like "mem").
  """
  @spec normalize_source(any()) :: recall_source()
  def normalize_source(v) when v in [:memory, :db, :hybrid], do: v

  def normalize_source(v) when is_binary(v) do
    case String.downcase(v) do
      "memory" -> :memory
      "mem" -> :memory
      "db" -> :db
      "database" -> :db
      "hybrid" -> :hybrid
      _ -> @default_recall_source
    end
  end

  def normalize_source(_), do: @default_recall_source

  @spec test_env?() :: boolean()
  def test_env? do
    mix_env =
      (Code.ensure_loaded?(Mix) and function_exported?(Mix, :env, 0) and Mix.env()) || :prod

    mix_env == :test
  end

  @doc """
  Merge per-call options into the base options map, normalizing values.

  - `base` is typically the configured `state.opts`.
  - `incoming` is a map of per-call overrides.

  Recognized keys:
    :window_keep, :half_life_ms, :recall_limit, :min_jaccard, :recall_source, :source, :limit
  """
  @spec merge_opts(map(), map()) :: map()
  def merge_opts(base, incoming) when is_map(base) and is_map(incoming) do
    merged = Map.merge(base, incoming)

    # allow aliases
    recall_source =
      merged
      |> Map.get(:source, Map.get(merged, :recall_source, @default_recall_source))
      |> normalize_source()

    limit =
      merged
      |> Map.get(:limit, Map.get(merged, :recall_limit, @default_recall_limit))
      |> normalize_limit()

    keep =
      merged
      |> Map.get(:window_keep, @default_keep)
      |> normalize_keep()

    half_life =
      merged
      |> Map.get(:half_life_ms, @default_half_life)
      |> normalize_half_life()

    min_j =
      merged
      |> Map.get(:min_jaccard, @default_min_jaccard)
      |> normalize_min_jaccard()

    merged
    |> Map.put(:recall_source, recall_source)
    |> Map.put(:source, recall_source)
    |> Map.put(:recall_limit, limit)
    |> Map.put(:limit, limit)
    |> Map.put(:window_keep, keep)
    |> Map.put(:half_life_ms, half_life)
    |> Map.put(:min_jaccard, min_j)
  end
end
