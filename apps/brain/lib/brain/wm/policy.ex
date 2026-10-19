defmodule Brain.WM.Policy do
  @moduledoc """
  Working-memory retention policy.

  Applies bounded decay, capacity eviction and retention ordering to items
  that have *already* passed the canonical Brain.BasalGanglia admission gate.
  This module does not score or admit new candidates.
  """

  alias Brain.Utils.Numbers

  @type wm_entry :: map()
  @type wm_state :: %{
          required(:wm) => [wm_entry()],
          optional(:wm_cfg) => map(),
          optional(:wm_last_ms) => integer() | nil
        }

  # Public API: Retention

  @spec apply_decay(wm_state(), integer()) :: wm_state()
  def apply_decay(%{wm: wm} = state, now_ms) when is_list(wm) and is_integer(now_ms) do
    dt = elapsed_ms(state, now_ms)
    decay_factor = Numbers.decay_factor_ms(dt)

    state
    |> Map.put(:wm, decay_wm_entries(wm, decay_factor))
    |> Map.put(:wm_last_ms, now_ms)
  end

  def apply_decay(state, _now_ms), do: state

  @spec evict_if_needed(wm_state()) :: wm_state()
  def evict_if_needed(%{wm: wm, wm_cfg: %{capacity: cap}} = state)
      when is_list(wm) and is_integer(cap) and cap >= 0 do
    if length(wm) <= cap do
      state
    else
      Map.put(state, :wm, keep_best_entries(wm, cap))
    end
  end

  def evict_if_needed(state), do: state

  @spec decay_and_evict(wm_state(), integer()) :: wm_state()
  def decay_and_evict(state, now_ms) do
    state
    |> apply_decay(now_ms)
    |> evict_if_needed()
  end

  # Retention helpers

  defp elapsed_ms(state, now_ms) do
    case Map.get(state, :wm_last_ms) do
      last_ms when is_integer(last_ms) -> max(now_ms - last_ms, 0)
      _ -> 0
    end
  end

  defp decay_wm_entries(wm, decay_factor) do
    Enum.map(wm, &decay_entry(&1, decay_factor))
  end

  defp decay_entry(%{score: score} = entry, decay_factor) when is_number(score) do
    %{entry | score: Numbers.clamp01(score * decay_factor)}
  end

  defp decay_entry(entry, _decay_factor), do: entry

  defp keep_best_entries(wm, cap) do
    wm
    |> Enum.sort_by(&entry_sort_key/1, :desc)
    |> Enum.take(cap)
  end

  defp entry_sort_key(entry) do
    {Map.get(entry, :score, 0.0), Map.get(entry, :ts, 0)}
  end
end
