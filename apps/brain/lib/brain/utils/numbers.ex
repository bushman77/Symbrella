# lib/brain/utils/numbers.ex
defmodule Brain.Utils.Numbers do
  @moduledoc """
  Numeric helpers for decay/clamp and bounds used across Brain.
  """

  @spec wm_decay_tau_ms() :: float()
  def wm_decay_tau_ms do
    Application.get_env(:brain, :wm_decay_tau_ms, 25_000.0 / 3.0) * 1.0
  end

  @spec wm_score_bounds() :: {float(), float()}
  def wm_score_bounds do
    {
      Application.get_env(:brain, :wm_score_min, 0.0) * 1.0,
      Application.get_env(:brain, :wm_score_max, 1.0) * 1.0
    }
  end

  @spec clamp01(number()) :: float()
  def clamp01(x) do
    {lo, hi} = wm_score_bounds()
    x = if is_number(x), do: x * 1.0, else: 0.0
    # sanitize NaN
    x = if x == x, do: x, else: 0.0

    cond do
      x < lo -> lo
      x > hi -> hi
      true -> x
    end
  end

  @spec decay_factor_ms(non_neg_integer(), number()) :: float()
  def decay_factor_ms(dt_ms, tau_ms \\ wm_decay_tau_ms())
      when is_number(dt_ms) and dt_ms >= 0 and is_number(tau_ms) and tau_ms > 0 do
    :math.exp(-(dt_ms * 1.0) / tau_ms)
  end
end
