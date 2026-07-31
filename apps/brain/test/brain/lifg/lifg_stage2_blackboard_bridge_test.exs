# apps/brain/test/brain/lifg_stage2_blackboard_bridge_test.exs
defmodule Brain.LIFGStage2BlackboardBridgeTest do
  use ExUnit.Case, async: false

  alias Brain.Bus
  alias Brain.Blackboard

  @moduledoc """
  Integration coverage for the Stage2 gating path:

    Stage1-like trace → Brain.LIFG.Stage2.run/2 → Brain.gate_from_lifg/2
      → WM update telemetry → Blackboard bridge → PubSub message

  This test intentionally does NOT use `:telemetry.execute/3` to simulate WM updates.
  """

  setup do
    # Ensure Blackboard is running (it usually is under the app supervisor)
    if is_nil(Process.whereis(Brain.Blackboard)) do
      start_supervised!({Brain.Blackboard, []})
    end

    # Subscribe to blackboard topic for bridged telemetry
    :ok = Bus.subscribe(Blackboard.topic())

    # Attach telemetry listener for gate decisions
    :ok =
      :telemetry.attach(
        "lifg-stage2-gate-test",
        [:brain, :gate, :decision],
        fn event, meas, meta, pid -> send(pid, {:gate, event, meas, meta}) end,
        self()
      )

    on_exit(fn ->
      :telemetry.detach("lifg-stage2-gate-test")
    end)

    :ok
  end

  test "Stage2 emits gate decision and WM update is bridged onto brain:blackboard" do
    # Stage2 expects a Stage1-ish event in trace with :choices.
    # Provide two choices: one above threshold (commit), one below (ignored).
    si = %{
      sentence: "Hello there",
      tokens: [
        %{index: 0, phrase: "Hello", span: {0, 5}, n: 1, mw: false},
        %{index: 1, phrase: "there", span: {6, 11}, n: 1, mw: false}
      ],
      trace: [
        %{
          stage: :lifg_stage1,
          choices: [
            %{
              token_index: 0,
              chosen_id: "hello there|phrase|fallback",
              lemma: "hello there",
              score: 0.95,
              margin: 0.20
            },
            %{
              token_index: 1,
              chosen_id: "there|noun|0",
              lemma: "there",
              score: 0.10,
              margin: 0.01
            }
          ]
        }
      ]
    }

    # Ensure WM starts clean-ish (best effort)
    _ = Brain.defocus(fn _ -> true end)

    # Trigger async Stage2 gating
    :ok =
      Brain.gate_from_lifg(si,
        lifg_stage2_enabled: true,
        lifg_min_score: 0.60,
        boost_margin: 0.10
      )

    # 1) Gate decision telemetry should fire for the committed item
    assert_receive {:gate, [:brain, :gate, :decision], %{score: score}, meta}, 500
    assert is_number(score) and score >= 0.60
    assert meta.source == :lifg
    assert meta.decision in [:allow, :boost]
    assert meta.id == "hello there|phrase|fallback"
    assert meta.token_index == 0

    # 2) Blackboard should receive the bridged WM update telemetry (real pipeline signal)
    assert_receive {:blackboard,
                    %{
                      kind: :telemetry,
                      event: [:brain, :wm, :update],
                      measurements: meas,
                      meta: %{reason: :lifg_stage2}
                    }},
                   500

    assert is_map(meas)
    assert meas.added >= 1
    assert meas.size >= 1

    # 3) WM snapshot contains the committed id
    %{wm: wm} = Brain.snapshot_wm()
    ids = wm |> Enum.map(& &1.id) |> MapSet.new()
    assert "hello there|phrase|fallback" in ids
  end
end
