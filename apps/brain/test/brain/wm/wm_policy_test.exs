defmodule Brain.WM.PolicyTest do
  use ExUnit.Case, async: true

  alias Brain.WM.Policy

  test "apply_decay updates wm_last_ms and decays scores" do
    state = %{wm: [%{id: "a", score: 1.0}], wm_last_ms: 0}

    out = Policy.apply_decay(state, 10)

    assert is_integer(out.wm_last_ms)
    assert [%{id: "a", score: score}] = out.wm
    assert score <= 1.0
  end

  test "evict_if_needed keeps highest-scoring entries within capacity" do
    state = %{wm: [%{id: 1, score: 0.2}, %{id: 2, score: 0.9}], wm_cfg: %{capacity: 1}}

    out = Policy.evict_if_needed(state)

    assert length(out.wm) == 1
    assert hd(out.wm).id == 2
  end

  test "decay_and_evict composes both steps" do
    state = %{
      wm: [%{id: 1, score: 0.2}, %{id: 2, score: 0.9}],
      wm_cfg: %{capacity: 1},
      wm_last_ms: 0
    }

    out = Policy.decay_and_evict(state, 10)

    assert length(out.wm) == 1
    assert is_integer(out.wm_last_ms)
  end
end
