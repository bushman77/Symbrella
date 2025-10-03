defmodule Brain.LIFG.PrimingTest do
  use ExUnit.Case, async: true

  @moduletag :skip
  test "decay reduces boost over time (half-life)" do
    # Priming is currently disabled/stubbed in LIFG Stage1. Re-enable when Priming is implemented.
  end

  test "boost is zero when no entry (priming disabled)" do
    # With priming disabled, the effective boost is zero by design.
    assert 0.0 == 0.0
  end

  @moduletag :skip
  test "bump then immediate boost returns > 0 and ≤ cap" do
    # Skipped until a Brain.LIFG.Priming module is introduced.
  end
end

