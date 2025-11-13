defmodule Brain.MoodCoreTest do
  use ExUnit.Case, async: false

  @baseline %{da: 0.5, "5ht": 0.5, glu: 0.5, ne: 0.5}

  setup do
    # If MoodCore is already running (likely started by your app supervisor), just use it.
    # Otherwise, start it locally for tests.
    case Process.whereis(Brain.MoodCore) do
      nil ->
        start_supervised!({Brain.MoodCore, []})

      _pid ->
        :ok
    end

    # Deterministic, self-ticking tests; start levels at baseline.
    Brain.MoodCore.configure(
      clock: :self,
      baseline: @baseline,
      init: @baseline,
      half_life_ms: 1_000,
      max_delta_per_tick: 0.20,
      saturation_ticks: 2,
      shock_threshold: 0.10
    )

    # Ensure current levels match the (possibly new) baseline right now.
    Brain.MoodCore.reset()
    :ok
  end

  test "snapshot returns mood indices" do
    snap = Brain.MoodCore.snapshot()
    assert is_map(snap.mood)
    assert Map.has_key?(snap.mood, :exploration)
    assert Map.has_key?(snap.mood, :inhibition)
    assert Map.has_key?(snap.mood, :vigilance)
    assert Map.has_key?(snap.mood, :plasticity)
  end

  test "bump clamps to max_delta_per_tick" do
    s0 = Brain.MoodCore.snapshot()
    # cast; will clamp to +0.20
    Brain.MoodCore.bump(%{da: 1.0})
    s1 = Brain.MoodCore.snapshot()
    assert_in_delta s1.levels.da - s0.levels.da, 0.20, 1.0e-6
  end

  test "decay heads toward baseline on tick" do
    # raise NE above baseline
    Brain.MoodCore.bump(%{ne: 0.20})
    s0 = Brain.MoodCore.snapshot()
    Process.sleep(5)
    # trigger one decay step
    send(Brain.MoodCore, :tick)
    Process.sleep(5)
    s1 = Brain.MoodCore.snapshot()
    assert s1.levels.ne < s0.levels.ne
  end

  test "shock and saturation telemetry fire" do
    parent = self()

    upd_id = "test-update-#{System.unique_integer([:positive])}"

    :telemetry.attach(
      upd_id,
      [:brain, :mood, :update],
      fn _, _, _, _ -> send(parent, :update) end,
      nil
    )

    sat_id = "test-sat-#{System.unique_integer([:positive])}"

    :telemetry.attach(
      sat_id,
      [:brain, :mood, :saturation],
      fn _, _, _, _ -> send(parent, :sat) end,
      nil
    )

    shk_id = "test-shock-#{System.unique_integer([:positive])}"

    :telemetry.attach(
      shk_id,
      [:brain, :mood, :shock],
      fn _, _, _, _ -> send(parent, :shock) end,
      nil
    )

    on_exit(fn ->
      :telemetry.detach(upd_id)
      :telemetry.detach(sat_id)
      :telemetry.detach(shk_id)
    end)

    # --- Shock: large multi-axis jump (L2 = 0.4) exceeds 0.10 threshold ---
    Brain.MoodCore.bump(%{da: 0.20, "5ht": 0.20, glu: 0.20, ne: 0.20})
    assert_receive :shock, 100

    # --- Saturation: drive DA to upper boundary and keep emitting events ---
    drive_to_boundary(:da, +1)
    Brain.MoodCore.bump(%{da: 0.20})
    Brain.MoodCore.bump(%{da: 0.20})
    assert_receive :sat, 150

    # Also test lower boundary on 5HT
    drive_to_boundary(:"5ht", -1)
    Brain.MoodCore.bump(%{"5ht" => -0.20})
    Brain.MoodCore.bump(%{"5ht" => -0.20})
    assert_receive :sat, 150
  end

  # ---------- helpers ----------

  # Drive one neuromodulator to a boundary (0.0 or 1.0) using repeated clamped bumps.
  defp drive_to_boundary(key, dir) when dir in [+1, -1] do
    step = if dir > 0, do: 0.20, else: -0.20

    # From 0.5 baseline, 3 steps reach the clamp at the boundary.
    for _ <- 1..3 do
      Brain.MoodCore.bump(%{key => step})
      Process.sleep(2)
    end
  end
end
