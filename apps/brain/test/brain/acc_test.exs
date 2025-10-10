defmodule Brain.ACCTest do
  use ExUnit.Case, async: true
  import Support.TelemetryHelpers

  setup do
    # Ensure ACC server is running so :record casts land in the rolling window.
    if Process.whereis(Brain.ACC) == nil do
      _pid = start_supervised!({Brain.ACC, []})
    end

    :ok
  end

  test "emits conflict telemetry and flags needy under thresholds" do
    si = %{tokens: [%{index: 0}, %{index: 1}]}

    # Two choices: one shaky (low margin, has alt) -> needy; one strong -> not needy
    choices = [
      %{
        token_index: 0,
        margin: 0.10,
        alt_ids: ["runner"],
        scores: %{"x|sense|1" => 0.55, "x|sense|2" => 0.45}
      },
      %{
        token_index: 1,
        margin: 0.60,
        alt_ids: [],
        scores: %{"y|sense|1" => 0.92, "y|sense|2" => 0.08}
      }
    ]

    {meas, meta} =
      capture(
        [:brain, :acc, :conflict],
        fn ->
          {:ok, resp} = Brain.ACC.assess(si, choices)
          assert is_float(resp.conflict)
          assert resp.conflict >= 0.0 and resp.conflict <= 1.0
          assert length(resp.needy) == 1
          assert resp.audit.stage == :acc
        end,
        300
      )

    assert meas[:n] == 2
    assert meas[:needy] == 1
    assert is_number(meas[:conflict])

    assert meta[:tau_m] >= 0.0
    assert meta[:p_min] >= 0.0
    assert is_map(meta[:weights])
  end

  test "records into rolling window and status returns last payload" do
    si = %{tokens: [%{index: 0}]}

    choices = [
      %{
        token_index: 0,
        margin: 0.05,
        alt_ids: ["alt"],
        scores: %{"z|sense|1" => 0.51, "z|sense|2" => 0.49}
      }
    ]

    {:ok, _} = Brain.ACC.assess(si, choices)

    # Give the async cast a tick to land
    :timer.sleep(10)

    state = Brain.ACC.status()
    assert state.region == :acc
    assert is_list(state.window)
    assert length(state.window) >= 1
    assert is_map(state.last)
    assert get_in(state.last, [:ev, :stage]) == :acc
  end
end
