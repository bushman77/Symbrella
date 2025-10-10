defmodule BrainWMEvictionDecayTest do
  use ExUnit.Case, async: true

  test "scores decay over elapsed time" do
    state = %{
      wm: [%{id: "a", score: 1.0, ts: 1}, %{id: "b", score: 0.5, ts: 1}],
      wm_cfg: %{capacity: 10},
      wm_last_ms: 1_000
    }

    # dt=3s
    state2 = :erlang.apply(Brain, :apply_decay, [state, 4_000])
    # k = exp(-lambda * 3.0)
    k = :math.exp(-Application.get_env(:brain, :wm_decay_lambda, 0.12) * 3.0)

    assert_in_delta Enum.at(state2.wm, 0).score, 1.0 * k, 1.0e-6
    assert_in_delta Enum.at(state2.wm, 1).score, 0.5 * k, 1.0e-6
  end

  test "eviction keeps highest score, tie-break by recency" do
    state = %{
      wm: [
        %{id: "old-high", score: 0.9, ts: 1000},
        %{id: "new-high", score: 0.9, ts: 2000},
        %{id: "mid", score: 0.7, ts: 3000}
      ],
      wm_cfg: %{capacity: 2}
    }

    state2 = :erlang.apply(Brain, :evict_if_needed, [state])
    ids = Enum.map(state2.wm, & &1.id)
    assert ids == ["new-high", "old-high"]
  end

  test "decay + eviction pipeline" do
    state = %{
      wm: [
        %{id: "x", score: 0.6, ts: 1000},
        %{id: "y", score: 0.4, ts: 1000},
        %{id: "z", score: 0.3, ts: 1000}
      ],
      wm_cfg: %{capacity: 2},
      wm_last_ms: 1_000
    }

    state2 = :erlang.apply(Brain, :decay_and_evict, [state, 5_000])
    assert length(state2.wm) == 2
    # After uniform decay, ordering by original score should remain
    assert Enum.map(state2.wm, & &1.id) == ["x", "y"]
  end
end
