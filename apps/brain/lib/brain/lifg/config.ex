defmodule Brain.LIFG.Config do
  @moduledoc """
  LIFG runtime/config helpers.

  Centralizes:
    • Stage-1 default weights (merged with `config :brain, :lifg_stage1_weights`)
    • Effective options materialized from env + per-call overrides

  This keeps `Brain.LIFG` slim while preserving the exact behavior you had before.
  """

  @default_weights %{
    lex_fit: 0.40,
    rel_prior: 0.30,
    activation: 0.20,
    intent_bias: 0.10
  }

  @doc """
  Base Stage-1 weights, merging application env over the internal defaults.
  """
  @spec lifg_weights() :: map()
  def lifg_weights do
    env = Application.get_env(:brain, :lifg_stage1_weights, %{})
    Map.merge(@default_weights, env || %{})
  end

  @doc """
  Build the effective LIFG option set that `status/0` and the server state expose.

  Accepts a map of overrides (typically the GenServer init options or a merged
  runtime map) and returns the fully-materialized option struct used by LIFG.
  """
  @spec effective_opts(map()) :: map()
  def effective_opts(overrides) when is_map(overrides) do
    %{
      weights:
        lifg_weights()
        |> Map.merge(Map.new(Map.get(overrides, :weights, %{}))),

      # What probability map to emit from Stage-1: :all | :top2 | :none
      scores:
        Map.get(
          overrides,
          :scores,
          Application.get_env(:brain, :lifg_stage1_scores_mode, :all)
        ),

      # Decision thresholds
      margin_threshold: Map.get(overrides, :margin_threshold, 0.15),
      min_margin: Application.get_env(:brain, :lifg_min_margin, 0.05),

      # pMTG integration
      pmtg_mode: Map.get(overrides, :pmtg_mode, Application.get_env(:brain, :pmtg_mode, :boost)),
      pmtg_window_keep: Application.get_env(:brain, :pmtg_window_keep, 50),

      # ACC gate
      acc_conflict_tau: Application.get_env(:brain, :acc_conflict_tau, 0.50),

      # MWE fallback behavior
      mwe_fallback: Application.get_env(:brain, :lifg_stage1_mwe_fallback, true)
    }
  end
end

