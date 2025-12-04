defmodule Brain.BlackboardTelemetryBridgeTest do
  use ExUnit.Case, async: true

  alias Brain.Blackboard
  alias Brain.Bus

  setup do
    # Ensure Blackboard is running (it usually is under the app supervisor)
    if is_nil(Process.whereis(Brain.Blackboard)) do
      start_supervised!({Brain.Blackboard, []})
    end

    :ok = ensure_ready()
    :ok = Bus.subscribe(Blackboard.topic())

    :ok
  end

  test "bridges [:brain, :wm, :update] telemetry onto brain:blackboard" do
    :telemetry.execute(
      [:brain, :wm, :update],
      %{size: 2, added: 1, removed: 0, capacity: 7},
      %{reason: :test}
    )

    assert_receive {:blackboard,
                    %{
                      kind: :telemetry,
                      event: [:brain, :wm, :update],
                      measurements: meas,
                      meta: meta
                    }},
                   500

    assert meas.size == 2
    assert meas.added == 1
    assert meas.capacity == 7
    assert meta.reason == :test
  end

  test "bridges [:brain, :pipeline, :lifg_stage1, :stop] telemetry onto brain:blackboard" do
    :telemetry.execute(
      [:brain, :pipeline, :lifg_stage1, :stop],
      %{duration_ms: 12},
      %{winners: 1, boosts: 1, inhibitions: 0}
    )

    assert_receive {:blackboard,
                    %{
                      kind: :telemetry,
                      event: [:brain, :pipeline, :lifg_stage1, :stop],
                      measurements: %{duration_ms: 12},
                      meta: %{winners: 1, boosts: 1, inhibitions: 0}
                    }},
                   500
  end

  defp ensure_ready do
    wait_until(fn ->
      s = Blackboard.state()
      s[:telemetry_attached?] == true
    end)
  end

  defp wait_until(fun, timeout_ms \\ 500, step_ms \\ 20) when is_function(fun, 0) do
    t0 = System.monotonic_time(:millisecond)

    cond do
      fun.() ->
        :ok

      System.monotonic_time(:millisecond) - t0 > timeout_ms ->
        {:error, :timeout}

      true ->
        Process.sleep(step_ms)
        wait_until(fun, timeout_ms, step_ms)
    end
  end
end
