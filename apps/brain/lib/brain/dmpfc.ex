defmodule Brain.DmPFC do
  @moduledoc """
  Dorsomedial PFC — confidence calibration & performance monitoring.

  Pure function:
    • `calibrate_confidence/1` → `%{confidence_scale, acc_conflict_gain}`

  Inputs in `ctx` (all optional, normalized where applicable):
    :predicted_confidence  — 0.0..1.0 (e.g., from LIFG margin/entropy)
    :actual_outcome        — 0.0 or 1.0 (downstream validation success)
    :gain                  — 0.0..1.0 (update strength; default 0.4)

  Outputs:
    :confidence_scale  — 0.5..1.5  (<1.0 if overconfident errors; >1.0 for underconfident wins)
    :acc_conflict_gain — 0.5..1.5  (>1.0 when recent calibration error is high)

  Telemetry:
    [:brain, :dmpfc, :calibration]
      measurements: %{pred: f, actual: f, brier: f}
      metadata:     %{confidence_scale: f, acc_conflict_gain: f}
  """

  @type ctx :: %{
          optional(:predicted_confidence) => number(),
          optional(:actual_outcome) => number(),
          optional(:gain) => number()
        }

  @spec calibrate_confidence(ctx) :: %{confidence_scale: float(), acc_conflict_gain: float()}
  def calibrate_confidence(ctx) when is_map(ctx) do
    pred = clamp01(Map.get(ctx, :predicted_confidence, 0.5))
    actual = clamp01(Map.get(ctx, :actual_outcome, 0.5))
    gain = clamp01(Map.get(ctx, :gain, 0.4))

    # Brier-like error captures calibration quality
    brier = :math.pow(pred - actual, 2)

    # Penalize overconfident errors; reward underconfident successes (gently)
    overconf_error = max(0.0, pred - actual)
    underconf_gain = max(0.0, actual - pred)

    confidence_scale =
      clamp_range(1.0 - gain * overconf_error + 0.3 * underconf_gain, 0.5, 1.5)

    # Increase sensitivity to ACC conflict when errors are high vs a target (~0.15)
    acc_conflict_gain =
      clamp_range(1.0 + gain * (brier - 0.15), 0.5, 1.5)

    :telemetry.execute(
      [:brain, :dmpfc, :calibration],
      %{pred: pred, actual: actual, brier: brier},
      %{confidence_scale: confidence_scale, acc_conflict_gain: acc_conflict_gain}
    )

    %{confidence_scale: confidence_scale, acc_conflict_gain: acc_conflict_gain}
  end

  # ─────────────── helpers ───────────────

  defp clamp01(x) when is_number(x), do: max(0.0, min(1.0, x))
  defp clamp01(_), do: 0.5

  defp clamp_range(x, lo, hi) when is_number(x), do: max(lo, min(hi, x))
end
