defmodule Brain.Telemetry do
  @moduledoc """
  Attach lightweight telemetry handlers for Brain events.

  Currently logs the LIFG Stage-1 pipeline stop event:

      [:brain, :pipeline, :lifg_stage1, :stop]

  Emitters may provide different metadata shapes depending on which wrapper produced
  the event. This logger supports both:

  - Newer pipeline wrapper meta:
      %{winners: n, boosts: n, inhibitions: n, ...}

  - Older meta (or additional optional fields):
      %{groups: ..., ctx_dim: ..., normalize: ..., scores_mode: ..., parallel: ...}

  Use `attach!/0` once at boot. Safe to call multiple times.
  """

  require Logger

  @event [:brain, :pipeline, :lifg_stage1, :stop]
  @handler_id "brain-lifg-stage1-logger"

  @doc """
  Attach a logger handler for the LIFG Stage-1 stop event.
  Safe to call multiple times (subsequent calls are ignored).
  """
  def attach!() do
    try do
      :telemetry.attach(@handler_id, @event, &__MODULE__.handle_lifg_stop/4, nil)
      :ok
    rescue
      ArgumentError ->
        # already attached (or invalid args); treat as OK for "attach once" semantics
        :ok
    catch
      :error, {:badarg, _} -> :ok
    end
  end

  @doc """
  Detach the logger handler. No-op if not attached.
  """
  def detach!() do
    try do
      :telemetry.detach(@handler_id)
      :ok
    rescue
      ArgumentError -> :ok
    catch
      :error, {:badarg, _} -> :ok
    end
  end

  @doc false
  def handle_lifg_stop(_event, measurements, metadata, _config) do
    duration_ms = Map.get(measurements, :duration_ms, 0)

    winners = Map.get(metadata, :winners)
    boosts = Map.get(metadata, :boosts)
    inhibitions = Map.get(metadata, :inhibitions)

    # Optional/legacy fields (log if present)
    groups = Map.get(metadata, :groups)
    ctx_dim = Map.get(metadata, :ctx_dim)
    norm = Map.get(metadata, :normalize)
    scores = Map.get(metadata, :scores_mode) || Map.get(metadata, :scores)
    parallel = Map.get(metadata, :parallel)

    Logger.info(fn ->
      base =
        "[LIFG] #{duration_ms}ms" <>
          (if is_integer(winners), do: " winners=#{winners}", else: "") <>
          (if is_integer(boosts), do: " boosts=#{boosts}", else: "") <>
          (if is_integer(inhibitions), do: " inhibitions=#{inhibitions}", else: "")

      extras =
        " groups=#{inspect(groups)} ctx_dim=#{inspect(ctx_dim)} " <>
          "norm=#{inspect(norm)} scores=#{inspect(scores)} parallel=#{inspect(parallel)}"

      base <> extras
    end)
  end

  # helper for other telemetry sites (kept for compatibility)
  def meta(extra \\ %{}) when is_map(extra) do
    Map.merge(%{v: 3}, extra)
  end
end

