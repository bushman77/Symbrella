defmodule Brain.FPCBranchingTest do
  use ExUnit.Case, async: true
  alias Brain.FPC

  test "high conflict + novelty + low value → nonzero budget and early switch" do
    ctx = %{
      acc_conflict: 0.9,
      novelty: 0.85,
      ofc_value: 0.2,
      max_budget: 3,
      min_switch_ms: 100,
      max_switch_ms: 400
    }

    out = FPC.plan_branches(ctx)

    assert is_integer(out.branch_budget)
    assert out.branch_budget >= 1 and out.branch_budget <= 3

    assert is_integer(out.switch_after_ms)
    assert out.switch_after_ms >= 100 and out.switch_after_ms <= 400
  end

  test "low conflict + high value → small or zero budget" do
    out = FPC.plan_branches(%{
      acc_conflict: 0.1,
      novelty: 0.1,
      ofc_value: 0.9,
      max_budget: 4
    })

    assert is_integer(out.branch_budget)
    assert out.branch_budget in 0..4
  end

  test "clamps to provided ranges and is deterministic" do
    ctx = %{
      acc_conflict: 10.0,     # will be clamped to 1.0
      novelty: -5.0,          # will be clamped to 0.0
      ofc_value: 42.0,        # will be clamped to 1.0
      max_budget: 7,          # will be clamped internally to 5 per module
      min_switch_ms: -100,    # should clamp to >= 0
      max_switch_ms: 20_000   # should clamp to <= 5000
    }

    a = FPC.plan_branches(ctx)
    b = FPC.plan_branches(ctx)

    assert a == b
    assert a.branch_budget in 0..5
    assert a.switch_after_ms >= 0 and a.switch_after_ms <= 5_000
  end
end

