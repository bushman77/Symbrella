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
    :policy,             # policy()
    :max_retries,        # pos_integer()
    :top_k,              # pos_integer()

    # FPC
    :branch_budget,      # non_neg_integer()
    :switch_after_ms,    # non_neg_integer()

    # VmPFC
    :utility_prior,      # 0.0..1.0
    :explore_rate,       # 0.0..1.0

    # DmPFC
    :confidence_scale,   # 0.5..1.5
    :acc_conflict_gain,  # 0.5..1.5

    # Salience
    :salience_boost      # 0.0..1.0
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

  @doc "Right-biased merge of partial maps/structs into a %Brain.ControlSignals{}"
  @spec merge([map() | t()]) :: t()
  def merge(parts) when is_list(parts) do
    base = %__MODULE__{} |> Map.from_struct()

    merged =
      Enum.reduce(parts, base, fn part, acc ->
        part_map =
          cond do
            is_struct(part) -> Map.from_struct(part)
            is_map(part)    -> part
            is_list(part)   -> Map.new(part)
            true            -> %{}
          end

        Map.merge(acc, part_map, fn _k, _v1, v2 -> v2 end)
      end)

    struct!(__MODULE__, merged)
  end

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

