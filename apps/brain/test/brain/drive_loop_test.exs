defmodule Brain.DriveLoopTest do
  use ExUnit.Case, async: false

  test "optional idle status heartbeat emits only after idle threshold" do
    parent = self()
    handler_id = "drive-loop-idle-status-test-#{System.unique_integer([:positive])}"

    :ok =
      :telemetry.attach(
        handler_id,
        [:brain, :drive_loop, :idle_status],
        fn event, measurements, metadata, _config ->
          send(parent, {:idle_status, event, measurements, metadata})
        end,
        nil
      )

    on_exit(fn -> :telemetry.detach(handler_id) end)

    {:ok, original} = Brain.DriveLoop.status()

    on_exit(fn ->
      :ok =
        Brain.DriveLoop.configure(
          idle_threshold_ms: original.idle_threshold_ms,
          idle_status_interval_ms: original.idle_status_interval_ms
        )
    end)

    :ok = Brain.DriveLoop.configure(idle_threshold_ms: 0, idle_status_interval_ms: 5)

    assert_receive {:idle_status, [:brain, :drive_loop, :idle_status], measurements, metadata},
                   100

    assert measurements.idle_ms >= 0
    assert metadata.current == :idle
    assert metadata.threshold_ms == 0
    assert metadata.interval_ms == 5
  end
end
