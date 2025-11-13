# apps/core/lib/core/curiosity/bridge.ex
defmodule Core.Curiosity.Bridge do
  @moduledoc """
  Listens to standalone `Curiosity` proposals and nudges `Core.Curiosity`
  (no user input, fully decoupled).

  Triggers when score ≥ threshold and at least `min_gap_ms` elapsed since last trigger.
  Tunable via config: :core, Core.Curiosity.Bridge
    threshold: 0.6
    min_gap_ms: 30000
  """

  @handler_id "core-curiosity-bridge"
  @default_threshold 0.60
  @default_min_gap_ms 30_000

  def attach do
    _ = :telemetry.attach(@handler_id, [:curiosity, :proposal], &__MODULE__.handle/4, %{})
    :ok
  rescue
    _ -> :ok
  end

  def detach do
    :telemetry.detach(@handler_id)
    :ok
  rescue
    _ -> :ok
  end

  def handle(_event, meas, _meta, _cfg) do
    score = Map.get(meas, :score, 0.0)
    now = System.system_time(:millisecond)
    last = :persistent_term.get({__MODULE__, :last_trigger_ms}, 0)

    if score >= threshold() and now - last >= min_gap_ms() do
      :persistent_term.put({__MODULE__, :last_trigger_ms}, now)
      Core.Curiosity.probe_now()
    end

    :ok
  end

  defp threshold do
    Application.get_env(:core, __MODULE__, [])
    |> Keyword.get(:threshold, @default_threshold)
  end

  defp min_gap_ms do
    Application.get_env(:core, __MODULE__, [])
    |> Keyword.get(:min_gap_ms, @default_min_gap_ms)
  end
end
