defmodule Brain.TelemetryTest do
  use ExUnit.Case, async: false

  import ExUnit.CaptureLog

  setup do
    original_cognition = Application.get_env(:brain, :log_cognition_pipeline?, false)
    original_lifg = Application.get_env(:brain, :log_lifg_stage1?, false)
    original_logger_level = Logger.level()

    Logger.configure(level: :info)

    on_exit(fn ->
      Application.put_env(:brain, :log_cognition_pipeline?, original_cognition)
      Application.put_env(:brain, :log_lifg_stage1?, original_lifg)
      Logger.configure(level: original_logger_level)
    end)

    :ok
  end

  test "cognition console logs idle status when enabled" do
    Application.put_env(:brain, :log_cognition_pipeline?, true)
    Application.put_env(:brain, :log_lifg_stage1?, false)

    log =
      capture_log([level: :info], fn ->
        Brain.Telemetry.handle(
          [:brain, :drive_loop, :idle_status],
          %{idle_ms: 61_000},
          %{current: :idle, threshold_ms: 60_000},
          nil
        )
      end)

    assert log =~ "[Cog] idle"
    assert log =~ "idle_ms=61000"
    assert log =~ "threshold_ms=60000"
  end

  test "cognition console is quiet when disabled" do
    Application.put_env(:brain, :log_cognition_pipeline?, false)
    Application.put_env(:brain, :log_lifg_stage1?, false)

    log =
      capture_log([level: :info], fn ->
        Brain.Telemetry.handle(
          [:brain, :drive_loop, :idle_status],
          %{idle_ms: 61_000},
          %{current: :idle, threshold_ms: 60_000},
          nil
        )
      end)

    assert log == ""
  end

  test "legacy LIFG logging flag still logs Stage1 stop events" do
    Application.put_env(:brain, :log_cognition_pipeline?, false)
    Application.put_env(:brain, :log_lifg_stage1?, true)

    log =
      capture_log([level: :info], fn ->
        Brain.Telemetry.handle(
          [:brain, :pipeline, :lifg_stage1, :stop],
          %{duration_ms: 12, weak: 1, missing: 0, low_confidence: 2},
          %{intent: :greet, confidence: 0.61, scores_mode: :all},
          nil
        )
      end)

    assert log =~ "[LIFG] 12ms"
    assert log =~ "weak=1"
    assert log =~ "intent=:greet"
  end
end
