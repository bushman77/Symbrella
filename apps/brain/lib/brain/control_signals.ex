defmodule Brain.ControlSignals do
  @moduledoc """
  Shared control knobs produced by prefrontal-like modules (DLPFC, FPC, VmPFC, DmPFC, Salience)
  and consumed by LIFG, Basal Ganglia, Thalamus, etc.

  Provides:
    • `t:t/0` – the struct (all fields optional; `nil` means "no opinion")
    • `merge/1` – right-biased merge of partial maps/structs
    • `clamp/1` – clamp numeric fields into sane ranges
    • `combine/1` – convenience: merge then clamp
  """

  @type policy :: :strict_mwe | :loose_single | :balanced

  @enforce_keys []
  defstruct [
    # DLPFC
    # policy()
    :policy,
    # pos_integer()
    :max_retries,
    # pos_integer()
    :top_k,

    # FPC
    # non_neg_integer()
    :branch_budget,
    # non_neg_integer()
    :switch_after_ms,

    # VmPFC
    # 0.0..1.0
    :utility_prior,
    # 0.0..1.0
    :explore_rate,

    # DmPFC
    # 0.5..1.5
    :confidence_scale,
    # 0.5..1.5
    :acc_conflict_gain,

    # Salience
    # 0.0..1.0
    :salience_boost
  ]

  @type t :: %__MODULE__{
          policy: policy() | nil,
          max_retries: pos_integer() | nil,
          top_k: pos_integer() | nil,
          branch_budget: non_neg_integer() | nil,
          switch_after_ms: non_neg_integer() | nil,
          utility_prior: float() | nil,
          explore_rate: float() | nil,
          confidence_scale: float() | nil,
          acc_conflict_gain: float() | nil,
          salience_boost: float() | nil
        }

  @known_keys [
    :policy, :max_retries, :top_k, :branch_budget, :switch_after_ms,
    :utility_prior, :explore_rate, :confidence_scale, :acc_conflict_gain,
    :salience_boost
  ]
  @numeric_keys [
    :max_retries, :top_k, :branch_budget, :switch_after_ms,
    :utility_prior, :explore_rate, :confidence_scale, :acc_conflict_gain,
    :salience_boost
  ]
  @integer_keys [:max_retries, :top_k, :branch_budget, :switch_after_ms]
  @policies [:strict_mwe, :loose_single, :balanced]

  @doc """
  Keep only recognized, correctly typed, non-nil planner recommendations.
  Nil means *no opinion* and cannot erase an earlier planner's recommendation.
  Unknown or malformed keys are ignored, never promoted into the control contract.
  """
  @spec normalize_part(map() | t() | keyword() | term()) :: map()
  def normalize_part(part) do
    values =
      cond do
        is_struct(part) -> Map.from_struct(part)
        is_map(part) -> part
        is_list(part) ->
          Enum.reduce(part, %{}, fn
            {key, value}, acc when is_atom(key) -> Map.put(acc, key, value)
            _, acc -> acc
          end)
        true -> %{}
      end

    Enum.reduce(@known_keys, %{}, fn key, acc ->
      case Map.fetch(values, key) do
        {:ok, value} ->
          if valid_part_value?(key, value),
            do: Map.put(acc, key, value),
            else: acc

        :error -> acc
      end
    end)
  end

  @doc "Right-biased merge of valid non-nil recommendations into a canonical struct."
  @spec merge([map() | t()]) :: t()
  def merge(parts) when is_list(parts) do
    merged =
      Enum.reduce(parts, %{}, fn part, acc ->
        Map.merge(acc, normalize_part(part))
      end)

    struct!(__MODULE__, merged)
  end

  defp valid_part_value?(:policy, value), do: value in @policies
  defp valid_part_value?(key, value) when key in @integer_keys,
    do: is_integer(value)
  defp valid_part_value?(key, value) when key in @numeric_keys,
    do: is_number(value)
  defp valid_part_value?(_key, _value), do: false

  @doc """
  Clamp numeric fields into sane ranges:

    • utility_prior: 0.0..1.0
    • explore_rate:  0.0..1.0
    • confidence_scale: 0.5..1.5
    • acc_conflict_gain: 0.5..1.5
    • salience_boost: 0.0..1.0
    • max_retries: 1..5
    • top_k: 1..10
    • branch_budget: 0..5
    • switch_after_ms: 0..5_000
  """
  @spec clamp(t()) :: t()
  def clamp(%__MODULE__{} = s) do
    %__MODULE__{
      s
      | utility_prior: clamp01(s.utility_prior),
        explore_rate: clamp01(s.explore_rate),
        confidence_scale: clamp_range(s.confidence_scale, 0.5, 1.5),
        acc_conflict_gain: clamp_range(s.acc_conflict_gain, 0.5, 1.5),
        salience_boost: clamp01(s.salience_boost),
        max_retries: clamp_int_minmax_or_nil(s.max_retries, 1, 5),
        top_k: clamp_int_minmax_or_nil(s.top_k, 1, 10),
        branch_budget: clamp_int_minmax_or_nil(s.branch_budget, 0, 5),
        switch_after_ms: clamp_int_minmax_or_nil(s.switch_after_ms, 0, 5_000)
    }
  end

  @doc "Convenience: merge then clamp."
  @spec combine([map() | t()]) :: t()
  def combine(parts), do: parts |> merge() |> clamp()

  # --- helpers ---

  defp clamp01(nil), do: nil
  defp clamp01(x) when is_number(x), do: clamp_range(x, 0.0, 1.0)

  defp clamp_range(nil, _lo, _hi), do: nil

  defp clamp_range(x, lo, hi) when is_number(x) and is_number(lo) and is_number(hi) do
    cond do
      x < lo -> lo
      x > hi -> hi
      true -> x
    end
  end

  defp clamp_int_minmax_or_nil(nil, _lo, _hi), do: nil

  defp clamp_int_minmax_or_nil(x, lo, hi) when is_integer(x) do
    cond do
      x < lo -> lo
      x > hi -> hi
      true -> x
    end
  end
end
