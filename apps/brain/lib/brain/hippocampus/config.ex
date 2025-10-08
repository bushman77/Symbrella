defmodule Brain.Hippocampus.Config do
  @moduledoc "Defaults and normalizers for Hippocampus."

  @default_keep 300
  @default_half_life 300_000
  @default_recall_limit 3
  @default_min_jaccard 0.0

  @spec defaults() :: %{
          window_keep: pos_integer(),
          half_life_ms: pos_integer(),
          recall_limit: pos_integer(),
          min_jaccard: float()
        }
  def defaults do
    %{
      window_keep: @default_keep,
      half_life_ms: @default_half_life,
      recall_limit: @default_recall_limit,
      min_jaccard: @default_min_jaccard
    }
  end

  @spec normalize_keep(any()) :: pos_integer()
  def normalize_keep(k) when is_integer(k) and k > 0, do: k
  def normalize_keep(_), do: @default_keep

  @spec normalize_half_life(any()) :: pos_integer()
  def normalize_half_life(h) when is_integer(h) and h > 0, do: h
  def normalize_half_life(_), do: @default_half_life

  @spec normalize_limit(any()) :: pos_integer()
  def normalize_limit(k) when is_integer(k) and k > 0, do: k
  def normalize_limit(_), do: @default_recall_limit

  @spec normalize_min_jaccard(any()) :: float()
  def normalize_min_jaccard(x) when is_number(x) and x >= 0 and x <= 1, do: x * 1.0
  def normalize_min_jaccard(_), do: @default_min_jaccard

  @spec test_env?() :: boolean()
  def test_env? do
    mix_env = (Code.ensure_loaded?(Mix) and function_exported?(Mix, :env, 0) and Mix.env()) || :prod
    mix_env == :test
  end
end

