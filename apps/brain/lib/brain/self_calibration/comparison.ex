defmodule Brain.SelfCalibration.Comparison do
  @moduledoc """
  Compares baseline and candidate self-calibration evaluations.

  This module is a diagnostic gate. It ranks advisory model output against the
  baseline by evaluation error, but it does not authorize downstream behavior
  changes by itself.
  """

  alias Brain.SelfCalibration.Evaluation

  @type winner :: :baseline | :candidate | :tie

  @type t :: %__MODULE__{
          baseline_mae: float(),
          candidate_mae: float(),
          delta: float(),
          winner: winner(),
          advisory?: boolean(),
          reason: atom(),
          baseline_source: atom(),
          candidate_source: atom(),
          baseline_model_version: String.t() | nil,
          candidate_model_version: String.t() | nil,
          feature_schema_v: pos_integer(),
          meta: map(),
          v: pos_integer()
        }

  defstruct baseline_mae: 0.0,
            candidate_mae: 0.0,
            delta: 0.0,
            winner: :tie,
            advisory?: true,
            reason: :equal_mae,
            baseline_source: :baseline,
            candidate_source: :candidate,
            baseline_model_version: nil,
            candidate_model_version: nil,
            feature_schema_v: 1,
            meta: %{},
            v: 1

  @spec compare(Evaluation.t(), Evaluation.t(), keyword()) :: {:ok, t()} | {:error, term()}
  def compare(baseline, candidate, opts \\ [])

  def compare(%Evaluation{} = baseline, %Evaluation{} = candidate, opts) do
    delta = candidate.mae - baseline.mae
    tolerance = Keyword.get(opts, :tolerance, 0.0)
    {winner, reason} = winner(delta, tolerance)

    {:ok,
     %__MODULE__{
       baseline_mae: baseline.mae,
       candidate_mae: candidate.mae,
       delta: delta,
       winner: winner,
       advisory?: true,
       reason: reason,
       baseline_source: baseline.source,
       candidate_source: candidate.source,
       baseline_model_version: baseline.model_version,
       candidate_model_version: candidate.model_version,
       feature_schema_v: candidate.feature_schema_v,
       meta: %{
         baseline_feature_schema_v: baseline.feature_schema_v,
         candidate_feature_schema_v: candidate.feature_schema_v,
         tolerance: tolerance
       }
     }}
  end

  def compare(_baseline, _candidate, _opts), do: {:error, :invalid_comparison_input}

  defp winner(delta, tolerance) when delta < -tolerance, do: {:candidate, :lower_mae}
  defp winner(delta, tolerance) when delta > tolerance, do: {:baseline, :lower_mae}
  defp winner(_delta, _tolerance), do: {:tie, :equal_mae}
end
