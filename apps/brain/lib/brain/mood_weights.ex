defmodule Brain.MoodWeights do
  @moduledoc """
  MoodWeights — pure helpers to turn MoodCore snapshots into score biases.

  Keys match LIFG Stage-1 defaults:
    weights = %{expl: +0.02, inhib: -0.03, vigil: +0.02, plast: 0.00}
    cap     = 0.05

  We center each mood index around 0.5 and sum deltas * weights, clamped to ±cap.
  """

  @type mood_idx :: %{
          exploration: float(),
          inhibition: float(),
          vigilance: float(),
          plasticity: float()
        }

  @doc """
  Compute a single scalar bias for use in scoring.

  Example:
      snap = Brain.MoodCore.snapshot()
      bias = Brain.MoodWeights.bias(snap.mood, %{expl: 0.02, inhib: -0.03, vigil: 0.02, plast: 0.00}, 0.05)
      score = clamp(base + bias, 0.0, 1.0)
  """
  @spec bias(mood_idx, map(), float()) :: float()
  def bias(
        %{exploration: e, inhibition: i, vigilance: v, plasticity: p},
        weights \\ %{expl: 0.02, inhib: -0.03, vigil: 0.02, plast: 0.00},
        cap \\ 0.05
      )
      when is_map(weights) and is_number(cap) do
    expl = (e - 0.5) * Map.get(weights, :expl, 0.0)
    inhib = (i - 0.5) * Map.get(weights, :inhib, 0.0)
    vigil = (v - 0.5) * Map.get(weights, :vigil, 0.0)
    plast = (p - 0.5) * Map.get(weights, :plast, 0.0)

    clamp(expl + inhib + vigil + plast, -abs(cap), +abs(cap))
  end

  @doc "Return centered axes (index - 0.5) for UI/debug."
  @spec axes(mood_idx) :: %{expl: float(), inhib: float(), vigil: float(), plast: float()}
  def axes(%{exploration: e, inhibition: i, vigilance: v, plasticity: p}) do
    %{expl: e - 0.5, inhib: i - 0.5, vigil: v - 0.5, plast: p - 0.5}
  end

  defp clamp(x, a, b), do: max(a, min(b, x))
end
