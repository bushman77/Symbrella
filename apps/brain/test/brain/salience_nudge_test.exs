defmodule Brain.SalienceNudgeTest do
  use ExUnit.Case, async: true
  alias Brain.Salience

  test "strong novelty + conflict + wm load produce a meaningful boost" do
    out = Salience.salience_nudge(%{novelty: 1.0, acc_conflict: 0.9, wm_load: 0.8})
    assert is_map(out)
    assert is_number(out.salience_boost)
    assert out.salience_boost >= 0.5
    assert out.salience_boost <= 1.0
  end

  test "quiet conditions keep boost near zero" do
    out = Salience.salience_nudge(%{novelty: 0.0, acc_conflict: 0.0, wm_load: 0.0})
    assert is_number(out.salience_boost)
    assert out.salience_boost <= 0.05
  end

  test "inputs are clamped into 0..1 domain" do
    out = Salience.salience_nudge(%{novelty: 9.0, acc_conflict: -3.0, wm_load: 42.0})
    assert out.salience_boost >= 0.0
    assert out.salience_boost <= 1.0
  end
end
