defmodule Brain.ThalamusMathProps_Test do
  use ExUnit.Case, async: false

  @th_event [:brain, :thalamus, :curiosity, :decision]

  def handle_decision(_event, meas, meta, pid) when is_pid(pid) do
    send(pid, {:decision, meas, meta})
    :ok
  end

  # --- helpers ---------------------------------------------------------------

  # Drain any pending decisions before starting a fresh sequence
  defp drain! do
    receive do
      {:decision, _m, _meta} -> drain!()
    after
      10 -> :ok
    end
  end

  # Receive until the decision for a specific probe id arrives (or timeout)
  defp recv_for_probe!(probe_id, timeout_ms \\ 500) do
    receive do
      {:decision, _m, meta} ->
        id = meta[:probe][:id] || meta[:probe]["id"]

        if to_string(id) == probe_id do
          meta
        else
          recv_for_probe!(probe_id, max(timeout_ms - 5, 0))
        end
    after
      timeout_ms ->
        flunk("Timed out waiting for decision for #{probe_id}")
    end
  end

  # --- setup ----------------------------------------------------------------

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

  setup do
    id = "th-math-#{System.unique_integer([:positive])}"
    :ok = :telemetry.attach(id, @th_event, &__MODULE__.handle_decision/4, self())
    on_exit(fn -> :telemetry.detach(id) end)
    :ok
  end

  test "blend is monotonic in ofc_weight with OFC present (no ACC)" do
    # Disable mood influence so we test pure OFC blend behavior
    zero_mood = %{expl: 0.0, inhib: 0.0, vigil: 0.0, plast: 0.0}

    :ok =
      Brain.Thalamus.set_params(
        ofc_weight: 0.0,
        acc_alpha: 0.0,
        mood_cap: 0.0,
        mood_weights: zero_mood
      )

    probe_id = "probe|blend|mono"
    drain!()

    # Warm OFC cache
    :telemetry.execute([:brain, :ofc, :value], %{value: 1.0}, %{probe_id: probe_id})
    _ = Brain.Thalamus.get_params()

    weights = [0.0, 0.25, 0.5, 0.75, 1.0]

    scores =
      for w <- weights do
        :ok =
          Brain.Thalamus.set_params(
            ofc_weight: w,
            acc_alpha: 0.0,
            mood_cap: 0.0,
            mood_weights: zero_mood
          )

        :telemetry.execute(
          [:curiosity, :proposal],
          %{score: 0.0},
          %{probe: %{id: probe_id, source: :math}}
        )

        meta = recv_for_probe!(probe_id)

        assert meta[:ofc_blended?]
        s = meta[:probe][:score]

        # Always a valid probability-ish score
        assert s >= 0.0 - 1.0e-9
        assert s <= 1.0 + 1.0e-9

        s
      end

    # Monotonic non-decreasing in ofc_weight
    Enum.reduce(scores, nil, fn s, prev ->
      if prev != nil do
        assert s >= prev - 1.0e-6
      end

      s
    end)
  end

  test "ACC brake reduces score monotonically with alpha (no OFC)" do
    # Disable mood influence so we test pure ACC brake behavior
    zero_mood = %{expl: 0.0, inhib: 0.0, vigil: 0.0, plast: 0.0}

    :ok =
      Brain.Thalamus.set_params(
        ofc_weight: 0.0,
        acc_alpha: 0.0,
        mood_cap: 0.0,
        mood_weights: zero_mood
      )

    :telemetry.execute([:brain, :acc, :conflict], %{conflict: 1.0}, %{})

    probe_id = "probe|acc|mono"
    base = 0.9

    alphas = [0.0, 0.25, 0.5, 0.75, 1.0]

    scores =
      for alpha <- alphas do
        :ok = Brain.Thalamus.set_params(ofc_weight: 0.0, acc_alpha: alpha)

        :telemetry.execute(
          [:curiosity, :proposal],
          %{score: base},
          %{probe: %{id: probe_id, source: :math}}
        )

        meta = recv_for_probe!(probe_id)
        s = meta[:probe][:score]

        # Brake never increases score, and stays within [0, base]
        assert s <= base + 1.0e-6
        assert s >= 0.0 - 1.0e-6

        s
      end

    # Monotonic non-increasing in alpha
    Enum.reduce(scores, nil, fn s, prev ->
      if prev != nil do
        eps = 1.0e-4
        assert s <= prev + eps
      end

      s
    end)
  end

  test "final probe score is always clamped to [0,1]" do
    :ok = Brain.Thalamus.set_params(ofc_weight: 0.9, acc_alpha: 0.9)
    probe_id = "probe|clamp"

    :telemetry.execute([:brain, :ofc, :value], %{value: 10.0}, %{probe_id: probe_id})
    _ = Brain.Thalamus.get_params()

    :telemetry.execute(
      [:curiosity, :proposal],
      %{score: 10.0},
      %{probe: %{id: probe_id, source: :clamp}}
    )

    assert_receive {:decision, _m1, meta1}, 500
    assert meta1[:probe][:score] <= 1.0 + 1.0e-9
    assert meta1[:probe][:score] >= 0.0 - 1.0e-9

    :telemetry.execute([:brain, :acc, :conflict], %{conflict: 1.0}, %{})
    :ok = Brain.Thalamus.set_params(ofc_weight: 0.0, acc_alpha: 1.0)

    :telemetry.execute(
      [:curiosity, :proposal],
      %{score: 0.01},
      %{probe: %{id: "probe|clamp|min", source: :clamp}}
    )

    assert_receive {:decision, _m2, meta2}, 500
    assert meta2[:probe][:score] >= 0.0 - 1.0e-9
    assert meta2[:probe][:score] <= 1.0 + 1.0e-9
  end
end
