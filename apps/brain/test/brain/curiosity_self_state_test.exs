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

  test "ordinary automatic nudges use learned bias rather than a fixed 0.55 score" do
    :ok = Brain.Curiosity.nudge(self_state: %{
      uncertainty: 0.5, novelty: 0.3,
      investigative_biases: %{v: 1, biases: %{missing_link: 0.12}}
    })

    assert_receive {:curiosity_proposal, measurements, _metadata}, 500
    assert measurements.score > 0.55
  end

  test "explicit caller score stays exact and records a compact policy reason" do
    :ok = Brain.Curiosity.nudge(
      score: 0.44,
      kind: :two_hop,
      self_state: %{uncertainty: 0.99, plasticity: 0.99,
        investigative_biases: %{v: 1, biases: %{two_hop: 0.12}}}
    )

    assert_receive {:curiosity_proposal, %{score: 0.44}, %{probe: probe}}, 500
    assert probe.kind == :two_hop
    assert probe.policy.mode == :explicit
    assert probe.policy.learned_bias == 0.12
  end
end
