# apps/brain/test/brain/thalamus_acc_ofc_test.exs
defmodule Brain.ThalamusACC_OFC_Test do
  use ExUnit.Case, async: false

  @th_event [:brain, :thalamus, :curiosity, :decision]

  #
  # Use a module function for telemetry to avoid the "local function" warning.
  #
  def handle_decision(_event, meas, meta, pid) when is_pid(pid) do
    send(pid, {:decision, meas, meta})
    :ok
  end

  setup_all do
    # Ensure core Brain supervisor is present if your test environment expects it
    # (harmless if already started).
    case Process.whereis(Brain) do
      nil  -> start_supervised!(Brain)
      _pid -> :ok
    end

    # Per your requirement: assume the global Thalamus is already up.
    # If it isn't, fail fast with a helpful message.
    case Process.whereis(Brain.Thalamus) do
      nil ->
        flunk("""
        Brain.Thalamus is not running.

        Tests in this module assume a singleton Brain.Thalamus is started by your
        application/supervisor in the :test environment. Please ensure your
        Symbrella.Application (or equivalent) starts Brain.Thalamus for MIX_ENV=test.
        """)

      _pid ->
        :ok
    end

    :ok
  end

  defp attach_decision_handler!() do
    hid = "test-thalamus-#{System.unique_integer([:positive])}"

    :ok =
      :telemetry.attach(
        hid,
        @th_event,
        &__MODULE__.handle_decision/4,
        self()
      )

    on_exit(fn -> :telemetry.detach(hid) end)
    :ok
  end

  setup do
    # Snapshot current effective params so each test restores them afterward.
    base = Brain.Thalamus.get_params()
    :ok = Brain.Thalamus.set_params(base)

    on_exit(fn -> Brain.Thalamus.set_params(base) end)
    :ok
  end

  test "OFC weighting blends into proposal score (ACC off)" do
    attach_decision_handler!()

    # Configure the already-running singleton (no start/stop in tests).
    :ok = Brain.Thalamus.set_params(ofc_weight: 0.80, acc_alpha: 0.0)

    probe_id = "curiosity|probe|ofc_ok"

    # Feed OFC value for this probe, then a curiosity proposal.
    :telemetry.execute([:brain, :ofc, :value], %{value: 1.0}, %{probe_id: probe_id})

    :telemetry.execute(
      [:curiosity, :proposal],
      %{score: 0.20},
      %{probe: %{id: probe_id, source: :runtime}}
    )

    assert_receive {:decision, meas, meta}, 1_000
    assert is_number(meas[:score])
    assert meta[:ofc_blended?] == true
    assert_in_delta meta[:ofc_value], 1.0, 1.0e-6
    assert_in_delta meta[:ofc_weight], 0.80, 1.0e-6
    assert meta[:acc_applied?] == false
    assert meta[:v] == 2
  end

  test "ACC conflict attenuates effective score (OFC off)" do
    attach_decision_handler!()

    :ok = Brain.Thalamus.set_params(ofc_weight: 0.0, acc_alpha: 0.50)

    probe_id = "curiosity|probe|acc_brake"

    :telemetry.execute([:brain, :acc, :conflict], %{conflict: 1.0}, %{})

    :telemetry.execute(
      [:curiosity, :proposal],
      %{score: 0.30},
      %{probe: %{id: probe_id, source: :runtime}}
    )

    assert_receive {:decision, _meas, meta}, 1_000
    assert meta[:acc_applied?] == true
    assert_in_delta meta[:acc_conflict], 1.0, 1.0e-6
    assert_in_delta meta[:acc_alpha], 0.50, 1.0e-6
    assert meta[:ofc_blended?] == false
    assert meta[:v] == 2
  end

  # ── Guard-rail tests ────────────────────────────────────────────────────────

  test "ACC present but alpha=0 => not applied" do
    attach_decision_handler!()

    :ok = Brain.Thalamus.set_params(ofc_weight: 0.40, acc_alpha: 0.0)
    probe_id = "curiosity|probe|alpha_zero"

    # Conflict exists but alpha=0 disables brake.
    :telemetry.execute([:brain, :acc, :conflict], %{conflict: 1.0}, %{})

    :telemetry.execute(
      [:curiosity, :proposal],
      %{score: 0.55},
      %{probe: %{id: probe_id, source: :runtime}}
    )

    assert_receive {:decision, _meas, meta}, 1_000
    assert meta[:acc_applied?] == false
    assert_in_delta meta[:acc_alpha], 0.0, 1.0e-6
  end

  test "ACC alpha>0 but conflict==0 => not applied" do
    attach_decision_handler!()

    :ok = Brain.Thalamus.set_params(ofc_weight: 0.30, acc_alpha: 0.75)
    probe_id = "curiosity|probe|conflict_zero"

    # Explicitly set conflict to 0.0 so state is deterministic.
    :telemetry.execute([:brain, :acc, :conflict], %{conflict: 0.0}, %{})

    :telemetry.execute(
      [:curiosity, :proposal],
      %{score: 0.45},
      %{probe: %{id: probe_id, source: :runtime}}
    )

    assert_receive {:decision, _meas, meta}, 1_000
    assert meta[:acc_applied?] == false
    assert_in_delta meta[:acc_conflict], 0.0, 1.0e-6
  end

  test "No OFC value cached => ofc_blended? == false and ofc_value == nil" do
    attach_decision_handler!()

    :ok = Brain.Thalamus.set_params(ofc_weight: 1.0, acc_alpha: 0.0)
    probe_id = "curiosity|probe|no_ofc"

    # Do NOT send any [:brain, :ofc, :value] for this probe.
    :telemetry.execute(
      [:curiosity, :proposal],
      %{score: 0.25},
      %{probe: %{id: probe_id, source: :runtime}}
    )

    assert_receive {:decision, _meas, meta}, 1_000
    assert meta[:ofc_blended?] == false
    assert meta[:ofc_value] == nil
  end

  test "set_params clamps out-of-range to [0,1]" do
    # No telemetry handler needed here.
    :ok = Brain.Thalamus.set_params(ofc_weight: 1.7, acc_alpha: -0.2)

    params = Brain.Thalamus.get_params()
    assert_in_delta params.ofc_weight, 1.0, 1.0e-6
    assert_in_delta params.acc_alpha, 0.0, 1.0e-6
  end

  test "OFC cache evicts least-recently-used beyond 64 entries" do
    attach_decision_handler!()

    :ok = Brain.Thalamus.set_params(ofc_weight: 1.0, acc_alpha: 0.0)

    # Insert 70 distinct probe values; cache size max is 64.
    for i <- 0..69 do
      pid = "probe-#{i}"
      :telemetry.execute([:brain, :ofc, :value], %{value: 1.0}, %{probe_id: pid})
    end

    # Oldest entries (0..5) should be evicted; newest ones (6..69) should remain.

    # Check evicted: no blend for "probe-0"
    :telemetry.execute(
      [:curiosity, :proposal],
      %{score: 0.10},
      %{probe: %{id: "probe-0", source: :runtime}}
    )

    assert_receive {:decision, _meas0, meta0}, 1_000
    assert meta0[:ofc_blended?] == false

    # Check present: blend for "probe-69"
    :telemetry.execute(
      [:curiosity, :proposal],
      %{score: 0.10},
      %{probe: %{id: "probe-69", source: :runtime}}
    )

    assert_receive {:decision, _meas1, meta1}, 1_000
    assert meta1[:ofc_blended?] == true
  end
end

