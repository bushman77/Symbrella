defmodule Brain.ThalamusTelemetryContract_Test do
  use ExUnit.Case, async: false

  @th_event [:brain, :thalamus, :curiosity, :decision]

  # Use a module function for telemetry to avoid the "local function" warning.
  def handle_decision(_event, meas, meta, pid) when is_pid(pid) do
    send(pid, {:decision, meas, meta})
    :ok
  end

  setup_all do
    case Process.whereis(Brain) do
      nil -> start_supervised!(Brain)
      _pid -> :ok
    end

    case Process.whereis(Brain.Thalamus) do
      nil ->
        flunk("""
        Brain.Thalamus is not running.

        These tests assume the singleton is already started under the umbrella root.
        """)

      _pid ->
        :ok
    end

    :ok
  end

  setup ctx do
    # Fresh, deterministic params for each test
    :ok = Brain.Thalamus.set_params(ofc_weight: 0.5, acc_alpha: 0.5)

    id = "th-telemetry-contract-#{System.unique_integer([:positive])}"
    :ok = :telemetry.attach(id, @th_event, &__MODULE__.handle_decision/4, self())
    on_exit(fn -> :telemetry.detach(id) end)

    {:ok, ctx}
  end

  test "decision event includes required keys; OFC blended + ACC applied path" do
    probe_id = "probe|contract|1"

    # Prime OFC cache and ACC
    :telemetry.execute([:brain, :ofc, :value], %{value: 0.8}, %{probe_id: probe_id})
    :telemetry.execute([:brain, :acc, :conflict], %{conflict: 0.5}, %{})

    # Propose curiosity
    :telemetry.execute(
      [:curiosity, :proposal],
      %{score: 0.5},
      %{probe: %{id: probe_id, source: :test}}
    )

    assert_receive {:decision, meas, meta}, 500

    # Measurements
    assert is_map(meas)
    assert is_number(meas[:score])
    assert meas[:score] >= 0.0 and meas[:score] <= 1.0

    # Metadata contract
    for k <- [
          :decision,
          :probe,
          :ofc_blended?,
          :ofc_value,
          :ofc_weight,
          :acc_applied?,
          :acc_conflict,
          :acc_alpha,
          :v
        ] do
      assert Map.has_key?(meta, k), "missing meta key #{inspect(k)}"
    end

    # OFC blended and ACC applied flags
    assert meta[:ofc_blended?] == true
    assert_in_delta 0.8, meta[:ofc_value], 1.0e-6
    assert_in_delta 0.5, meta[:ofc_weight], 1.0e-6
    assert meta[:acc_applied?] == true
    assert_in_delta 0.5, meta[:acc_conflict], 1.0e-6
    assert_in_delta 0.5, meta[:acc_alpha], 1.0e-6

    # Telemetry version: accept 2 or 3 to allow for minor evolutions.
    assert meta[:v] in [2, 3]

    # Probe is echoed with a score reflecting blend + brake:
    # blended ≈ (1-w)*base + w*ofc = (0.5)*0.5 + 0.5*0.8 = 0.65
    # final   ≈ blended * (1 - alpha*conflict) = 0.65 * (1 - 0.25) = 0.4875
    #
    # The full pipeline (WM/BG/mood plumbing) can nudge this more than a percent
    # or two, so we allow a slightly wider but still tight tolerance here.
    assert_in_delta 0.4875, meta[:probe][:score], 3.0e-2
    assert meta[:probe][:id] == probe_id
    assert meta[:source] in [:test, "test"]
  end

  test "no OFC value → ofc_blended? false; ofc_value nil" do
    :ok = Brain.Thalamus.set_params(ofc_weight: 0.9, acc_alpha: 0.0)

    probe_id = "probe|no_ofc"

    :telemetry.execute(
      [:curiosity, :proposal],
      %{score: 0.4},
      %{probe: %{id: probe_id, source: :unit}}
    )

    assert_receive {:decision, _meas, meta}, 500
    refute meta[:ofc_blended?]
    assert meta[:ofc_value] in [nil]
    assert_in_delta 0.9, meta[:ofc_weight], 1.0e-6
    assert meta[:acc_applied?] == false
  end

  test "alpha = 0 or missing ACC → acc_applied? false" do
    :ok = Brain.Thalamus.set_params(ofc_weight: 0.0, acc_alpha: 0.0)

    # Make ACC state deterministic for this test
    :telemetry.execute([:brain, :acc, :conflict], %{conflict: 0.0}, %{})

    probe_id = "probe|no_acc"

    :telemetry.execute(
      [:curiosity, :proposal],
      %{score: 0.9},
      %{probe: %{id: probe_id, source: :unit}}
    )

    assert_receive {:decision, _meas, meta}, 500
    assert meta[:acc_applied?] == false
    assert_in_delta 0.0, meta[:acc_alpha], 1.0e-6
    # With ACC explicitly set to 0.0 above, it should echo 0.0 here.
    assert_in_delta 0.0, meta[:acc_conflict] || 0.0, 1.0e-6
  end
end
