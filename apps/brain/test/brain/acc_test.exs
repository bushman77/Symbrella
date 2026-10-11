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

    # One uncertain choice and one confident choice.
    choices = [
      %{
        token_index: 0,
        margin: 0.10,
        alt_ids: ["runner"],
        scores: %{
          "x|sense|1" => 0.55,
          "x|sense|2" => 0.45
        }
      },
      %{
        token_index: 1,
        margin: 0.60,
        alt_ids: [],
        scores: %{
          "y|sense|1" => 0.92,
          "y|sense|2" => 0.08
        }
      }
    ]

    event = [:brain, :acc, :conflict]
    probe_id = attach_probe([event])

    try do
      {:ok, resp} = Brain.ACC.assess(si, choices)

      # Verify ACC assessment results.
      assert is_float(resp.conflict)
      assert resp.conflict >= 0.0
      assert resp.conflict <= 1.0
      assert resp.needy == [0]
      assert resp.audit.stage == :acc

      expected_conflict = resp.conflict

      # Match only the assessment event.
      # Background :tick and :proposal events are ignored.
      assert_receive {
                       :telemetry,
                       ^event,
                       %{
                         n: 2,
                         needy: 1,
                         conflict: ^expected_conflict
                       },
                       %{cause: :assess} = meta
                     },
                     300

      # Validate telemetry metadata.
      assert is_number(meta[:tau_m])
      assert meta[:tau_m] >= 0.0

      assert is_number(meta[:p_min])
      assert meta[:p_min] >= 0.0

      assert is_map(meta[:weights])
      assert meta[:v] == 2
    after
      # Always detach, including when an assertion fails.
      detach_probe(probe_id)
    end
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
