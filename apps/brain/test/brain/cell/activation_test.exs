defmodule Brain.Cell.ActivationTest do
  use ExUnit.Case, async: false

  alias Brain.Cell.Activation

  test "elapsed-time half-life is independent of refresh frequency" do
    t = 100_000
    half = Activation.half_life_ms()

    assert_in_delta Activation.decay(1.0, t, t + half), 0.5, 1.0e-8
    assert_in_delta Activation.decay(1.0, t, t + 2 * half), 0.25, 1.0e-8

    first = Activation.decay(1.0, t, t + div(half, 2))
    second = Activation.decay(first, t + div(half, 2), t + half)
    assert_in_delta second, Activation.decay(1.0, t, t + half), 1.0e-8
  end

  test "resting values are zero, future timestamps never increase activation" do
    assert Activation.decay(0.0, 0, 10_000) == 0.0
    assert Activation.decay(-0.5, 0, 10_000) == 0.0
    assert Activation.decay(0.1, 10_000, 9_000) == 0.1
    assert Activation.decay(0.1, 0, 100_000) == 0.0
    assert Activation.decay(:not_a_number, 0, 10_000) == 0.0
  end

  test "decayed report snapshots prune resting cells and their timestamps" do
    now = 100_000
    half = Activation.half_life_ms()

    {reports, timestamps} =
      Activation.decay_reports(
        %{"active|noun|1" => 0.8, "resting|noun|1" => 0.1, "invalid|noun|1" => -1.0},
        %{
          "active|noun|1" => now - half,
          "resting|noun|1" => now - 100_000,
          "invalid|noun|1" => now
        },
        now
      )

    assert_in_delta reports["active|noun|1"], 0.4, 1.0e-8
    assert timestamps == %{"active|noun|1" => now}
    refute Map.has_key?(reports, "resting|noun|1")
  end

  test "Brain.Cell process decays lazily and reactivation starts from the decayed value" do
    id = "cell_decay_#{System.unique_integer([:positive])}|noun|1"
    {:ok, pid} = Brain.Cell.start_link(id)

    GenServer.cast(pid, {:activate, %{delta: 1.0}})
    assert {:ok, %{activation: initial}} = GenServer.call(pid, :status)
    assert_in_delta initial, 1.0, 0.01

    # Backdate only this test cell's monotonic clock; no slow wall-clock sleeps.
    half = Activation.half_life_ms()

    :sys.replace_state(pid, fn state ->
      Map.update!(state, :last_activation_ms, &(&1 - half))
    end)

    assert {:ok, %{activation: decayed}} = GenServer.call(pid, :status)
    assert_in_delta decayed, 0.5, 0.01

    GenServer.cast(pid, {:activate, %{delta: 0.25}})
    assert {:ok, %{activation: reactivated}} = GenServer.call(pid, :status)
    assert_in_delta reactivated, 0.75, 0.01
  end

  test "Brain coordinator snapshots observe time decay and drop resting neurons" do
    id = "coordinator_decay_#{System.unique_integer([:positive])}|noun|1"
    now = System.monotonic_time(:millisecond)

    Brain.report_activation(id, 1.0, now - Activation.half_life_ms())
    snapshot = Brain.activation_snapshot()
    assert_in_delta Map.fetch!(snapshot, id), 0.5, 0.02

    # This is a real timed report, not synthetic dashboard-only activity.
    Brain.report_activation(id, 0.0)
    refute Map.has_key?(Brain.activation_snapshot(), id)
  end
end
