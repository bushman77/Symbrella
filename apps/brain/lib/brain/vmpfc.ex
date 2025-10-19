defmodule Brain.VmPFC do
  @moduledoc """
  Ventromedial PFC — integrates recent reward/success with effort cost
  to produce longer-horizon control knobs.

  Pure function:
    • `compute_utility/1` → `%{utility_prior, explore_rate}`

  Inputs in `ctx` (all optional):
    :recent_success_rate  — 0.0..1.0   (validation hit-rate over a recent window)
    :avg_cost_ms          — >= 0       (mean effort/time per parse/attempt)
    :cost_scale_ms        — > 0        (normalizer for cost; default 400)
    :prior                — 0.0..1.0   (sticky baseline utility; default 0.5)

  Outputs:
    :utility_prior  — 0.0..1.0 (smoothed utility baseline used by other regions)
    :explore_rate   — 0.0..1.0 (tendency to explore; higher when utility is low)

  Telemetry:
    [:brain, :vmpfc, :utility]
      measurements: %{success: f, cost_ms: integer(), prior: f}
      metadata:     %{utility_prior: f, explore_rate: f}
  """

  @type ctx :: %{
          optional(:recent_success_rate) => number(),
          optional(:avg_cost_ms) => non_neg_integer(),
          optional(:cost_scale_ms) => pos_integer(),
          optional(:prior) => number()
        }

  @spec compute_utility(ctx) :: %{utility_prior: float(), explore_rate: float()}
  def compute_utility(ctx) when is_map(ctx) do
    success    = clamp01(Map.get(ctx, :recent_success_rate, 0.5))
    cost_ms    = max(0, Map.get(ctx, :avg_cost_ms, 200))
    cost_scale = max(1, Map.get(ctx, :cost_scale_ms, 400))
    prior      = clamp01(Map.get(ctx, :prior, 0.5))

    # Normalize cost into 0..1, where 1.0 ~ “expensive”
    norm_cost = min(1.0, cost_ms / cost_scale)

    # Blend prior with (success - weighted cost). Weight cost to avoid overreacting.
    raw_util       = max(0.0, success - 0.5 * norm_cost)
    utility_prior  = clamp01(0.5 * prior + 0.5 * raw_util)

    # Explore more when utility is low; soften with 0.6 factor for stability.
    explore_rate   = clamp01(0.6 * (1.0 - utility_prior))

    :telemetry.execute(
      [:brain, :vmpfc, :utility],
      %{success: success, cost_ms: cost_ms, prior: prior},
      %{utility_prior: utility_prior, explore_rate: explore_rate}
    )

    %{utility_prior: utility_prior, explore_rate: explore_rate}
  end

  # ─────────────── helpers ───────────────

  defp clamp01(x) when is_number(x), do: max(0.0, min(1.0, x))
  defp clamp01(_), do: 0.5
end

