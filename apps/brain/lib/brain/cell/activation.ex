defmodule Brain.Cell.Activation do
  @moduledoc """
  Elapsed-time decay for individual runtime neuron activations.

  A BrainCell's lexical identity and learned connections are permanent; its
  runtime excitation is not. This module is shared by Brain.Cell and the
  coordinator's activation snapshots, ensuring the Brain dashboard and STM
  agree without spawning one timer per neuron.

  Monotonic millisecond timestamps belong to the current BEAM runtime only.
  No activity is persisted to brain_cells.activation by this module.
  """

  @default_half_life_ms 5_000
  @default_rest_threshold 0.001

  @doc "Configurable half-life (ms) for a neuron's transient activation."
  @spec half_life_ms() :: pos_integer()
  def half_life_ms do
    case Application.get_env(:brain, :cell_activation_half_life_ms, @default_half_life_ms) do
      ms when is_integer(ms) and ms > 0 -> ms
      _ -> @default_half_life_ms
    end
  end

  @doc "Activity below this threshold counts as resting (zero)."
  @spec rest_threshold() :: float()
  def rest_threshold do
    case Application.get_env(:brain, :cell_activation_rest_threshold, @default_rest_threshold) do
      x when is_number(x) and x > 0 and x < 1 -> x * 1.0
      _ -> @default_rest_threshold
    end
  end

  @doc """
  Decay one activation over elapsed monotonic milliseconds.

  Uses a half-life rather than a multiplier per message or refresh; idle time
  and refresh frequency therefore cannot change the decay rate.
  """
  @spec decay(number(), integer(), integer()) :: float()
  def decay(value, since_ms, now_ms)
      when is_number(value) and is_integer(since_ms) and is_integer(now_ms) do
    if value <= 0 do
      0.0
    else
      elapsed = max(now_ms - since_ms, 0)
      decayed = value * :math.pow(0.5, elapsed / half_life_ms())
      if decayed < rest_threshold(), do: 0.0, else: decayed * 1.0
    end
  end

  def decay(_, _, _), do: 0.0

  @doc """
  Decay and prune coordinator reports and their matching timestamps.

  A missing timestamp is treated as current (legacy in-memory state) rather
  than pretending an unknown interval has elapsed.
  """
  @spec decay_reports(map(), map(), integer()) :: {map(), map()}
  def decay_reports(reports, timestamps, now_ms)
      when is_map(reports) and is_map(timestamps) and is_integer(now_ms) do
    Enum.reduce(reports, {%{}, %{}}, fn {id, value}, {active, times} ->
      since = Map.get(timestamps, id, now_ms)
      current = decay(value, since, now_ms)

      if current > 0 do
        {Map.put(active, id, current), Map.put(times, id, now_ms)}
      else
        {active, times}
      end
    end)
  end
end
