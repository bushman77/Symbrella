# apps/brain/lib/brain/hippocampus/writer.ex
defmodule Brain.Hippocampus.Writer do
  @moduledoc """
  Hippocampus episode persistence.

  IMPORTANT:
  - brain must not depend on core (db <- brain <- core <- web).
  - Therefore, anything we persist must be JSON-safe without requiring Core structs
    (e.g., `%Core.Token{}`) to implement Jason.Encoder.

  This module deep-normalizes incoming SI/episode payloads into plain maps/lists
  before JSON encoding, preventing Jason.Encoder crashes.
  """

  require Logger

  alias Brain.Utils.Safe
  alias Db.Episode

  # ---------------------------------------------------------------------------
  # Public
  # ---------------------------------------------------------------------------

  @doc """
  Persist an episode if enabled via opts.

  Expects:
    - `episode` map-like (may contain structs)
    - `opts` keyword with `:persist?` boolean

  Returns:
    - :ok (best-effort; never raises to callers)
  """
  @spec maybe_persist(map(), keyword()) :: :ok
  def maybe_persist(%{} = episode, opts) when is_list(opts) do
    persist? = Keyword.get(opts, :persist?, Keyword.get(opts, :persist, true))
    priming? = Keyword.get(opts, :priming, false)

    episode_plain = deep_plain(episode)

    if priming? do
      maybe_emit_priming(episode_plain)
    end

    if persist? do
      try do
        insert_row!(episode_plain)
        :ok
      rescue
        e ->
          Logger.debug("Hippo.Writer failed: #{Exception.message(e)}")
          :ok
      catch
        :exit, reason ->
          Logger.debug("Hippo.Writer exit: #{inspect(reason)}")
          :ok

        kind, reason ->
          Logger.debug("Hippo.Writer throw/error: #{inspect({kind, reason})}")
          :ok
      end
    else
      :ok
    end
  end

  def maybe_persist(_other, _opts), do: :ok

  defp maybe_emit_priming(%{} = episode) do
    case priming_outcome(episode) do
      :neutral ->
        :ok

      outcome when outcome in [:success, :failure] ->
        if Code.ensure_loaded?(:telemetry) and function_exported?(:telemetry, :execute, 3) do
          :telemetry.execute(
            [:brain, :hippo, :priming],
            %{count: 1},
            %{outcome: outcome}
          )
        end

        :ok
    end
  end

  defp priming_outcome(%{} = ep) do
    cond do
      get_in(ep, [:reanalysis, :gave_up]) == true ->
        :failure

      success_episode?(ep) ->
        :success

      true ->
        :neutral
    end
  end

  defp success_episode?(ep) do
    winners =
      get_in(ep, [:atl_slate, :winners]) ||
        get_in(ep, ["atl_slate", "winners"]) ||
        []

    conf =
      get_in(ep, [:intent, :confidence]) ||
        get_in(ep, ["intent", "confidence"]) ||
        0.0

    winners != [] and is_number(conf) and conf >= 0.5
  end

  @doc """
  Insert an episode row (raises on DB errors).

  This is called internally by `maybe_persist/2` and is allowed to raise;
  callers are expected to wrap it.
  """
  @spec insert_row!(map()) :: :ok
  def insert_row!(%{} = episode) do
    case Episode.insert(episode_payload(episode)) do
      {:ok, _episode} ->
        :ok

      {:error, reason} ->
        raise "Hippo.Writer insert failed: #{inspect(reason)}"
    end
  end

  # ---------------------------------------------------------------------------
  # Payload builder
  # ---------------------------------------------------------------------------

  defp episode_payload(%{} = episode) do
    ep = deep_plain(episode)

    meta = Safe.get(ep, :meta) || Safe.get(ep, "meta") || %{}
    slate = Safe.get(ep, :slate) || Safe.get(ep, "slate") || %{}
    norms = Safe.get(ep, :norms) || Safe.get(ep, "norms") || []
    embedding = Safe.get(ep, :embedding) || Safe.get(ep, "embedding")

    %{
      slate: slate,
      meta: meta,
      norms: normalize_norms(norms),
      tags: episode_tags(meta, slate)
    }
    |> maybe_put_embedding(embedding)
  end

  defp normalize_norms(norms) when is_list(norms) do
    norms
    |> Enum.map(&to_string/1)
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
    |> Enum.map(&String.downcase/1)
    |> Enum.uniq()
  end

  defp normalize_norms(_), do: []

  defp episode_tags(meta, slate) do
    (List.wrap(Safe.get(meta, :tags) || Safe.get(meta, "tags")) ++
       List.wrap(Safe.get(slate, :tags) || Safe.get(slate, "tags")))
    |> Enum.map(&to_string/1)
    |> Enum.reject(&(&1 == ""))
    |> Enum.uniq()
  end

  defp maybe_put_embedding(map, nil), do: map
  defp maybe_put_embedding(map, embedding), do: Map.put(map, :embedding, embedding)

  # ---------------------------------------------------------------------------
  # Compactors (JSON-safe)
  # ---------------------------------------------------------------------------

  # Keep only a bounded slice of SI. Must be JSON-safe.

  # ---------------------------------------------------------------------------
  # Deep normalization (JSON-safe)
  # ---------------------------------------------------------------------------

  # Convert any nested structs into JSON-safe maps recursively.
  defp deep_plain(%MapSet{} = set), do: MapSet.to_list(set)

  defp deep_plain(%_{} = s) do
    s
    |> Map.from_struct()
    |> Map.drop([:__struct__, :__meta__])
    |> Enum.map(fn {k, v} -> {k, deep_plain(v)} end)
    |> Map.new()
  end

  defp deep_plain(%{} = m) do
    m
    |> Enum.map(fn {k, v} -> {k, deep_plain(v)} end)
    |> Map.new()
  end

  defp deep_plain(list) when is_list(list), do: Enum.map(list, &deep_plain/1)
  defp deep_plain(other), do: other
end
