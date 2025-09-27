
defmodule Brain.Telemetry do
  @moduledoc """
  Attach lightweight telemetry handlers for Brain events.

  ## What it logs
  When `Brain.lifg_stage1/3` completes it emits:
      :telemetry.execute(
        [:brain, :pipeline, :lifg_stage1, :stop],
        %{duration_ms: ms},
        %{groups: ..., ctx_dim: ..., normalize: ..., scores_mode: ..., parallel: ...}
      )

  This module provides `attach!/0` to log that event as a single concise line.

  ## Usage
      # in your Application start/2 (umbrella or app that owns Brain)
      def start(_type, _args) do
        children = [
          # ... your Registry, DynamicSupervisor, etc ...
        ]

        # Attach once at boot:
        Brain.Telemetry.attach!()

        Supervisor.start_link(children, strategy: :one_for_one, name: MyApp.Supervisor)
      end

  ## Optional: Detach (e.g., in tests)
      Brain.Telemetry.detach!()
  """

  require Logger

  @event [:brain, :pipeline, :lifg_stage1, :stop]
  @handler_id "brain-lifg-stage1-logger"

  @doc """
  Attach a logger handler for the LIFG Stage‑1 stop event.
  Safe to call multiple times (subsequent calls are ignored).
  """
  def attach!() do
    :telemetry.attach(
      @handler_id,
      @event,
      &__MODULE__.handle_lifg_stop/4,
      nil
    )

    :ok
  catch
    :error, {:badarg, _} -> :ok # already attached
  end

  @doc """
  Detach the logger handler. No‑op if not attached.
  """
  def detach!() do
    :telemetry.detach(@handler_id)
    :ok
  catch
    :error, {:badarg, _} -> :ok
  end

  @doc false
  def handle_lifg_stop(_event, measurements, metadata, _config) do
    # Measurements
    duration_ms = Map.get(measurements, :duration_ms, 0)

    # Metadata (as emitted by Brain.lifg_stage1)
    groups   = Map.get(metadata, :groups)
    ctx_dim  = Map.get(metadata, :ctx_dim)
    norm     = Map.get(metadata, :normalize)
    scores   = Map.get(metadata, :scores_mode)
    parallel = Map.get(metadata, :parallel)

    Logger.info(fn ->
      "[LIFG] #{duration_ms}ms groups=#{inspect(groups)} ctx_dim=#{inspect(ctx_dim)} " <>
      "norm=#{inspect(norm)} scores=#{inspect(scores)} parallel=#{inspect(parallel)}"
    end)
  end
end
