defmodule Brain.Salience do
  @moduledoc """
  Salience nudge (aINS ↔ ACC): computes a brief explore boost from
  novelty, conflict, and WM load. Pure and stateless.

  Public API:
    • `salience_nudge/1` → `%{salience_boost: 0.0..1.0}`

  Inputs (ctx, all optional and ideally normalized 0..1):
    :novelty       — Hippocampus novelty / prediction error
    :acc_conflict  — ACC conflict level
    :wm_load       — working-memory load (how “busy” WM is)

  Telemetry:
    [:brain, :salience, :boost]
      measurements: %{novelty: f, acc_conflict: f, wm_load: f}
      metadata:     %{salience_boost: f}
  """

  @type ctx :: %{
          optional(:novelty) => number(),
          optional(:acc_conflict) => number(),
          optional(:wm_load) => number()
        }

  @spec salience_nudge(ctx) :: %{salience_boost: float()}
  def salience_nudge(ctx) when is_map(ctx) do
    novelty  = clamp01(Map.get(ctx, :novelty, 0.0))
    conflict = clamp01(Map.get(ctx, :acc_conflict, 0.0))
    wm_load  = clamp01(Map.get(ctx, :wm_load, 0.0))

    # Weighted sum favors simultaneous spikes; then squash for nicer dynamics.
    raw   = novelty * 0.6 + conflict * 0.5 + wm_load * 0.3
    boost = raw |> nonlin_boost() |> clamp01()

    :telemetry.execute(
      [:brain, :salience, :boost],
      %{novelty: novelty, acc_conflict: conflict, wm_load: wm_load},
      %{salience_boost: boost}
    )

    %{salience_boost: boost}
  end

  # ─────────────── helpers ───────────────

  defp nonlin_boost(x) when is_number(x) do
    # Squash without reaching 1.0; 0.5→~0.58, 1.0→~0.76
    x / (1.0 + 0.3 * x)
  end

  defp clamp01(x) when is_number(x), do: max(0.0, min(1.0, x))
  defp clamp01(_), do: 0.0
end

