defmodule Brain.SelfPortraitTest do
  use ExUnit.Case, async: false

  test "emits monitor telemetry when observing a LIFG payload gap" do
    if is_nil(Process.whereis(Brain.SelfPortrait)) do
      start_supervised!({Brain.SelfPortrait, []})
    else
      :ok = Brain.SelfPortrait.reset()
    end

    handler_id = "self-portrait-monitor-test-#{System.unique_integer([:positive])}"

    assert :ok =
             :telemetry.attach(
               handler_id,
               [:brain, :self_portrait, :monitor],
               fn _event, meas, meta, pid -> send(pid, {:self_monitor, meas, meta}) end,
               self()
             )

    on_exit(fn -> :telemetry.detach(handler_id) end)

    Brain.SelfPortrait.observe(%{
      kind: :telemetry,
      event: [:brain, :pipeline, :lifg_stage1, :stop],
      measurements: %{kept: 1},
      meta: %{
        kept_tokens: 1,
        tokens: [],
        choices: [],
        finalists: []
      },
      at_ms: 1
    })

    assert_receive {:self_monitor, %{count: 1}, %{issue: :lifg_payload_gap, severity: :warning}},
                   200
  end

  test "emits monitor telemetry when observing a LIFG POS anomaly" do
    if is_nil(Process.whereis(Brain.SelfPortrait)) do
      start_supervised!({Brain.SelfPortrait, []})
    else
      :ok = Brain.SelfPortrait.reset()
    end

    handler_id = "self-portrait-pos-monitor-test-#{System.unique_integer([:positive])}"

    assert :ok =
             :telemetry.attach(
               handler_id,
               [:brain, :self_portrait, :monitor],
               fn _event, meas, meta, pid -> send(pid, {:self_monitor, meas, meta}) end,
               self()
             )

    on_exit(fn -> :telemetry.detach(handler_id) end)

    Brain.SelfPortrait.observe(%{
      kind: :telemetry,
      event: [:brain, :pipeline, :lifg_stage1, :stop],
      measurements: %{kept: 3},
      meta: %{
        intent: :greet,
        kept_tokens: 3,
        tokens: [
          %{index: 0, phrase: "Good"},
          %{index: 1, phrase: "morning"},
          %{index: 2, phrase: "Symbrella"}
        ],
        choices: [
          %{token_index: 0, chosen_id: "good|verb|1", margin: 0.05},
          %{token_index: 1, chosen_id: "morning|noun|0", margin: 1.0},
          %{token_index: 2, chosen_id: "symbrella|assistant|0", margin: 1.0}
        ],
        finalists: [%{token_index: 0, ranking: [{"good|verb|1", 0.05}]}]
      },
      at_ms: 1
    })

    assert_receive {:self_monitor, %{count: 1}, %{issue: :lifg_pos_anomaly, severity: :warning}},
                   200
  end
end
