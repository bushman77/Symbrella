defmodule Brain.LIFG.PrimingTest do
  use ExUnit.Case, async: true
  alias Brain.LIFG.Priming

  setup do
    Priming.clear()
    :ok
  end

  test "boost is zero when no entry" do
    assert Priming.boost_for("X") == 0.0
  end

  test "bump then immediate boost returns > 0 and ≤ cap" do
    now = 1_000_000
    :ok = Priming.bump("A", now_ms: now, delta: 1.0)
    b = Priming.boost_for("A", now_ms: now, cap: 1.0, half_life_sec: 300.0)
    assert b > 0.0 and b <= 1.0
  end

  test "decay reduces boost over time (half-life)" do
    now = 2_000_000
    :ok = Priming.bump("A", now_ms: now, delta: 1.0)
    b0 = Priming.boost_for("A", now_ms: now, half_life_sec: 10.0, cap: 1.0)
    b1 = Priming.boost_for("A", now_ms: now + 5_000, half_life_sec: 10.0, cap: 1.0)
    b2 = Priming.boost_for("A", now_ms: now + 10_000, half_life_sec: 10.0, cap: 1.0)

    assert b0 >= b1 and b1 >= b2
  end
end
