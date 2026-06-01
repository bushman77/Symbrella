defmodule Brain.CuriositySelfStateTest do
  use ExUnit.Case, async: false

  setup do
    case Process.whereis(Brain.Curiosity) do
      nil -> start_supervised!(Brain.Curiosity)
      _pid -> :ok
    end

    id = "curiosity-self-state-test-#{System.unique_integer([:positive])}"
    parent = self()

    :ok =
      :telemetry.attach(
        id,
        [:curiosity, :proposal],
        fn _event, measurements, metadata, pid ->
          send(pid, {:curiosity_proposal, measurements, metadata})
        end,
        parent
      )

    on_exit(fn -> :telemetry.detach(id) end)

    :ok
  end

  test "automatic score responds to supplied uncertainty and dopamine state" do
    :ok =
      Brain.Curiosity.nudge(
        self_state: %{
          uncertainty: 0.9,
          novelty: 0.7,
          mood: %{mood: %{exploration: 0.8}, levels: %{da: 0.7}}
        }
      )

    assert_receive {:curiosity_proposal, measurements, metadata}, 500

    assert measurements.score > 0.8
    assert measurements.uncertainty == 0.9
    assert measurements.novelty == 0.7
    assert metadata.reason == :self_state
    assert metadata.probe.score == measurements.score
  end
end
