defmodule Brain.MetaMonitorTest do
  use ExUnit.Case, async: false

  alias Brain.{MetaMonitor, SelfModel}

  test "warnings/2 detects high uncertainty, low stability, overload, and recent errors" do
    warnings =
      MetaMonitor.warnings(%SelfModel{
        uncertainty: 0.8,
        stability: 0.2,
        cognitive_load: 0.9,
        recent_errors: [%{kind: :lifg_payload_gaps, count: 2}]
      })

    assert Enum.any?(warnings, &(&1.kind == :high_uncertainty))
    assert Enum.any?(warnings, &(&1.kind == :low_stability))
    assert Enum.any?(warnings, &(&1.kind == :high_cognitive_load))
    assert Enum.any?(warnings, &(&1.kind == :recent_errors))
  end

  test "warnings/2 detects contradictions and stuck loops" do
    warnings =
      MetaMonitor.warnings(%SelfModel{
        confidence: 0.9,
        uncertainty: 0.9,
        stability: 0.7,
        recent_actions: [
          %{action: :ask_clarifying_question},
          %{action: :ask_clarifying_question},
          %{action: :ask_clarifying_question}
        ]
      })

    assert Enum.any?(warnings, &(&1.kind == :contradiction))
    assert Enum.any?(warnings, &(&1.kind == :stuck_loop))
  end

  test "recovery_suggestions/1 maps warnings to bounded repair actions" do
    suggestions =
      MetaMonitor.recovery_suggestions([
        %{kind: :high_cognitive_load},
        %{kind: :contradiction},
        %{kind: :stuck_loop}
      ])

    assert :reduce_scope in suggestions
    assert :surface_uncertainty in suggestions
    assert :change_strategy in suggestions
  end

  test "warnings/2 returns empty list for stable self-model" do
    assert MetaMonitor.warnings(%SelfModel{
             uncertainty: 0.2,
             stability: 0.8,
             cognitive_load: 0.1,
             recent_errors: []
           }) == []
  end

  test "check/2 emits warning telemetry with count and v metadata" do
    handler_id = "meta-monitor-warning-#{System.unique_integer([:positive])}"
    parent = self()

    :ok =
      :telemetry.attach(
        handler_id,
        [:brain, :meta_monitor, :warning],
        fn event, meas, meta, _ ->
          send(parent, {:telemetry, event, meas, meta})
        end,
        nil
      )

    assert {:warning, warnings} =
             MetaMonitor.check(%SelfModel{
               uncertainty: 0.95,
               stability: 0.1,
               cognitive_load: 0.2,
               recent_errors: []
             })

    assert length(warnings) == 2

    assert_receive {:telemetry, [:brain, :meta_monitor, :warning], meas, meta}, 100

    assert meas.count == 2
    assert meas.critical == 2
    assert meta.v == 1
    assert meta.self_model_v == 1
    assert :high_uncertainty in meta.warning_kinds
    assert :low_stability in meta.warning_kinds
    assert :prefer_evidence in meta.recovery_suggestions

    :telemetry.detach(handler_id)
  end

  test "check/2 does not emit telemetry when there are no warnings" do
    handler_id = "meta-monitor-no-warning-#{System.unique_integer([:positive])}"
    parent = self()

    :ok =
      :telemetry.attach(
        handler_id,
        [:brain, :meta_monitor, :warning],
        fn event, meas, meta, _ ->
          send(parent, {:telemetry, event, meas, meta})
        end,
        nil
      )

    assert {:ok, []} =
             MetaMonitor.check(%SelfModel{
               uncertainty: 0.1,
               stability: 0.9,
               cognitive_load: 0.1,
               recent_errors: []
             })

    refute_receive {:telemetry, [:brain, :meta_monitor, :warning], _meas, _meta}, 50

    :telemetry.detach(handler_id)
  end
end
