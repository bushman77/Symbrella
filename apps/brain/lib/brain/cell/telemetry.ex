defmodule Brain.Cell.Telemetry do
  @moduledoc """
  Minimal, dependency-free telemetry wiring for Brain cell calls.

  • Attaches once (idempotent) and logs *only* when useful:
      - slow calls over `warn_ms` (default 200ms)
      - any error calls with reason
  • Keeps us metrics-lib agnostic (PromEx/TelemetryMetrics are *optional*).

  Configure (optional) in :brain app env (config/*.exs):
      config :brain, Brain.Cell.Safe, warn_ms: 200
  """
  require Logger

  @id_calls "brain-cell-call-logger"
  @id_errs  "brain-cell-call-error-logger"

  @spec ensure_attached() :: :ok
  def ensure_attached do
    case :persistent_term.get({__MODULE__, :attached?}, false) do
      true ->
        :ok

      false ->
        attach()
        :persistent_term.put({__MODULE__, :attached?}, true)
        :ok
    end
  catch
    :error, _ -> :ok # be conservative; never fail callers
  end

  defp attach do
    :telemetry.attach_many(
      @id_calls,
      [[:brain, :cell, :call]],
      &__MODULE__.handle_call/4,
      %{}
    )

    :telemetry.attach_many(
      @id_errs,
      [[:brain, :cell, :call, :error]],
      &__MODULE__.handle_error/4,
      %{}
    )
  end

  # Log only when duration exceeds warn threshold to avoid noise.
  def handle_call(_event, %{duration: dur_native}, meta, _cfg) do
    ms = System.convert_time_unit(dur_native, :native, :millisecond)
    warn_ms =
      Application.get_env(:brain, Brain.Cell.Safe, [])
      |> Keyword.get(:warn_ms, 200)

    if ms >= warn_ms do
      Logger.warning(fn ->
        "[brain/cell] slow call tag=#{meta[:tag]} ms=#{ms} timeout=#{meta[:timeout]} attempts=#{meta[:attempts]} retry=#{meta[:retry]}"
      end)
    end

    :ok
  rescue
    _ -> :ok
  end

  def handle_error(_event, %{duration: dur_native}, meta, _cfg) do
    ms = System.convert_time_unit(dur_native, :native, :millisecond)

    Logger.warning(fn ->
      "[brain/cell] ERROR tag=#{meta[:tag]} reason=#{inspect(meta[:reason])} ms=#{ms} timeout=#{meta[:timeout]} attempts=#{meta[:attempts]} retry=#{meta[:retry]}"
    end)

    :ok
  rescue
    _ -> :ok
  end
end

