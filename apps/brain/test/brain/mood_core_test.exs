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

  test "apply_appraisal synchronously raises vigilance for crisis language" do
    before = Brain.MoodCore.snapshot()

    after_ =
      %{sentence: "im going to hurt myself"}
      |> Brain.AffectiveAppraisal.appraise()
      |> Brain.MoodCore.apply_appraisal()

    assert after_.levels.ne > before.levels.ne
    assert after_.levels[:"5ht"] < before.levels[:"5ht"]
    assert after_.mood.vigilance > before.mood.vigilance
    assert after_.tone_hint in [:deescalate, :cautious, :neutral]
    assert after_.pressure_label in [:deescalation_pressure, :cautious_emergency_attention]
    assert [%{source: :appraisal, pressure_label: label} | _] = after_.mood_trace
    assert label in [:deescalation_pressure, :cautious_emergency_attention]
  end

  test "apply_intent is synchronous so immediate snapshots see the turn bump" do
    before = Brain.MoodCore.snapshot()

    returned = Brain.MoodCore.apply_intent(:question, 1.0)
    after_ = Brain.MoodCore.snapshot()

    assert returned.levels.da > before.levels.da
    assert returned.levels.ne > before.levels.ne
    assert after_.levels.da == returned.levels.da
    assert after_.levels.ne == returned.levels.ne
  end

  test "health support intent raises vigilance without saturating it" do
    Brain.MoodCore.configure(init: %{da: 0.5, "5ht": 0.5, glu: 0.81, ne: 0.87})

    after_ = Brain.MoodCore.apply_intent(:health_support, 1.0)

    assert after_.levels.ne > 0.87
    assert after_.levels.ne <= 0.88
    assert after_.levels.glu <= 0.82
    assert after_.levels.ne < 1.0
    assert after_.levels.glu < 1.0
  end

  test "activation and working-memory load nudges do not pin vigilance or plasticity" do
    Brain.MoodCore.configure(init: %{da: 0.5, "5ht": 0.5, glu: 0.81, ne: 0.85})

    cells = for n <- 1..200, into: %{}, do: {"cell-#{n}", 1.0}
    Brain.MoodCore.register_activation(cells)
    Brain.MoodCore.update_wm(Enum.to_list(1..20))

    Process.sleep(10)
    after_ = Brain.MoodCore.snapshot()

    assert after_.levels.ne <= 0.86
    assert after_.levels.glu <= 0.82
    assert after_.levels.ne < 1.0
    assert after_.levels.glu < 1.0
  end

  test "moderate vigilance with reduced inhibition yields cautious middle tone" do
    snap =
      Brain.MoodCore.configure(init: %{da: 0.5, "5ht": 0.48, glu: 0.5, ne: 0.53})

    assert snap.tone_hint == :cautious
    assert snap.pressure_label == :cautious_emergency_attention
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
