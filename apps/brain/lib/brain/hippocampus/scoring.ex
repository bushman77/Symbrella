defmodule Brain.Hippocampus.Scoring do
  @moduledoc "Set similarity and recency decay for Hippocampus recall."

  @doc """
  Jaccard similarity for two sets.
  """
  @spec jaccard(MapSet.t(), MapSet.t()) :: float()
  def jaccard(%MapSet{} = a, %MapSet{} = b) do
    ai = MapSet.size(MapSet.intersection(a, b))
    au = MapSet.size(a) + MapSet.size(b) - ai
    if au <= 0, do: 0.0, else: ai / au
  end

  @doc """
  Exponential half-life decay factor.

  Returns `1.0` for non-positive ages; otherwise computes `0.5^(age_ms / half_life_ms)`.
  """
  @spec recency_factor(non_neg_integer(), pos_integer()) :: float()
  def recency_factor(age_ms, _half_life_ms) when age_ms <= 0, do: 1.0

  def recency_factor(age_ms, half_life_ms) when is_integer(half_life_ms) and half_life_ms > 0 do
    :math.pow(0.5, age_ms / half_life_ms)
  end
end
