defmodule Brain.VmPFCUtilityTest do
  use ExUnit.Case, async: true
  alias Brain.VmPFC

  test "low success & high cost → higher explore_rate, lower utility_prior" do
    bad =
      VmPFC.compute_utility(%{
        recent_success_rate: 0.2,
        avg_cost_ms: 600,
        cost_scale_ms: 400,
        prior: 0.5
      })

    good =
      VmPFC.compute_utility(%{
        recent_success_rate: 0.9,
        avg_cost_ms: 100,
        cost_scale_ms: 400,
        prior: 0.5
      })

    assert bad.explore_rate >= good.explore_rate
    assert bad.utility_prior <= good.utility_prior
    assert bad.utility_prior >= 0.0 and bad.utility_prior <= 1.0
    assert good.utility_prior >= 0.0 and good.utility_prior <= 1.0
  end

  test "clamps outputs into 0..1 domain even with extreme inputs" do
    o =
      VmPFC.compute_utility(%{
        recent_success_rate: 42.0,
        avg_cost_ms: 10_000,
        cost_scale_ms: 1,
        prior: -7.0
      })

    assert o.utility_prior >= 0.0 and o.utility_prior <= 1.0
    assert o.explore_rate >= 0.0 and o.explore_rate <= 1.0
  end
end

