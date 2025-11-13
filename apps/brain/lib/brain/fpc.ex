defmodule Brain.FPC do
  @moduledoc """
  Frontopolar Cortex (BA10) — pure planner for parallel branching and timed switching.

  Turns a `ctx` of normalized knobs into a small plan:

    • `branch_budget`     — 0..max_budget (how many alternates to keep/try)
    • `switch_after_ms`   — when to consider switching to the next branch

  This module is **pure** (no timers, no state). It’s safe to call from LIFG Stage-1
  or any decision path. If you later promote FPC to a region (GenServer), keep this
  file as the stateless planner and wrap it.
  """

  @type ctx :: %{
          # 0.0..1.0 (higher = more conflict)
          optional(:acc_conflict) => number(),
          # 0.0..1.0 (Hippocampus novelty/PE)
          optional(:novelty) => number(),
          # 0.0..1.0 (immediate EV of current parse; lower → explore)
          optional(:ofc_value) => number(),
          # cap for branch_budget (default 2)
          optional(:max_budget) => non_neg_integer(),
          # earliest switch time (default 120)
          optional(:min_switch_ms) => non_neg_integer(),
          # latest  switch time (default 500)
          optional(:max_switch_ms) => non_neg_integer()
        }

  @spec plan_branches(ctx) :: %{
          branch_budget: non_neg_integer(),
          switch_after_ms: non_neg_integer()
        }
  def plan_branches(ctx) when is_map(ctx) do
    acc_conflict = clamp01(Map.get(ctx, :acc_conflict, 0.0))
    novelty = clamp01(Map.get(ctx, :novelty, 0.0))
    ofc_value = clamp01(Map.get(ctx, :ofc_value, 0.5))

    max_budget = ensure_int_range(Map.get(ctx, :max_budget, 2), 0, 5)
    min_ms = ensure_int_range(Map.get(ctx, :min_switch_ms, 120), 0, 5_000)
    max_ms = ensure_int_range(Map.get(ctx, :max_switch_ms, 500), min_ms, 5_000)

    # Budget grows with conflict & novelty, shrinks with high immediate value.
    raw_budget = acc_conflict * 0.6 + novelty * 0.5 + (1.0 - ofc_value) * 0.7
    branch_budget = scale_to_int(raw_budget, max_budget)

    # Switch sooner when conflict/novelty are high.
    # t ~ 0 when both high → near min_ms; t ~ 1 when both low → near max_ms
    t = 1.0 - (acc_conflict * 0.5 + novelty * 0.5)

    switch_after_ms =
      min_ms + round((max_ms - min_ms) * clamp01(t))

    :telemetry.execute(
      [:brain, :fpc, :branch],
      %{acc_conflict: acc_conflict, novelty: novelty, ofc_value: ofc_value},
      %{budget: branch_budget, switch_after_ms: switch_after_ms}
    )

    %{branch_budget: branch_budget, switch_after_ms: switch_after_ms}
  end

  # ─────────────── helpers ───────────────

  defp clamp01(x) when is_number(x), do: max(0.0, min(1.0, x))
  defp clamp01(_), do: 0.0

  defp ensure_int_range(x, lo, hi) when is_integer(x) and is_integer(lo) and is_integer(hi) do
    cond do
      x < lo -> lo
      x > hi -> hi
      true -> x
    end
  end

  defp ensure_int_range(_, lo, _hi), do: lo

  # Map a score roughly in 0..2 into 0..max_budget (rounded).
  defp scale_to_int(score_0_to_2, max_budget) do
    v = max(0.0, min(2.0, score_0_to_2))
    round(v / 2.0 * max_budget)
  end
end
