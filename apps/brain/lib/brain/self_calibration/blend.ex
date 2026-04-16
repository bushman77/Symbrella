defmodule Brain.SelfCalibration.Blend do
  @moduledoc """
  Explicit rule/model blend policy for self-calibration predictions.

  The default mode is observe-only. The policy can report that a candidate model
  would be preferred, but it does not mutate Brain.SelfModel.
  """

  alias Brain.SelfCalibration.Comparison
  alias Brain.SelfCalibration.Prediction

  @event [:brain, :self_calibration, :blend]

  @type mode :: :observe_only | :candidate

  @type t :: %__MODULE__{
          accepted?: boolean(),
          mode: mode(),
          reason: atom(),
          baseline_source: atom(),
          candidate_source: atom(),
          winner: Comparison.winner(),
          delta: float(),
          proposed: Prediction.t() | nil,
          applied: Prediction.t() | nil,
          meta: map(),
          v: pos_integer()
        }

  defstruct accepted?: false,
            mode: :observe_only,
            reason: :advisory_only,
            baseline_source: :baseline,
            candidate_source: :candidate,
            winner: :tie,
            delta: 0.0,
            proposed: nil,
            applied: nil,
            meta: %{},
            v: 1

  @spec decide(Prediction.t(), Prediction.t(), Comparison.t(), keyword()) ::
          {:ok, t()} | {:error, term()}
  def decide(baseline, candidate, comparison, opts \\ [])

  def decide(
        %Prediction{} = baseline,
        %Prediction{} = candidate,
        %Comparison{} = comparison,
        opts
      ) do
    mode = Keyword.get(opts, :mode, :observe_only)
    decision = build_decision(mode, baseline, candidate, comparison)

    emit_decision(decision)

    {:ok, decision}
  end

  def decide(_baseline, _candidate, _comparison, _opts), do: {:error, :invalid_blend_input}

  defp build_decision(
         :candidate,
         baseline,
         candidate,
         %Comparison{winner: :candidate} = comparison
       ) do
    %__MODULE__{
      accepted?: true,
      mode: :candidate,
      reason: :candidate_lower_mae,
      baseline_source: baseline.source,
      candidate_source: candidate.source,
      winner: comparison.winner,
      delta: comparison.delta,
      proposed: candidate,
      applied: candidate,
      meta: comparison_meta(comparison)
    }
  end

  defp build_decision(:candidate, baseline, candidate, %Comparison{} = comparison) do
    %__MODULE__{
      accepted?: false,
      mode: :observe_only,
      reason: :candidate_not_better,
      baseline_source: baseline.source,
      candidate_source: candidate.source,
      winner: comparison.winner,
      delta: comparison.delta,
      proposed: candidate,
      applied: nil,
      meta: comparison_meta(comparison)
    }
  end

  defp build_decision(_mode, baseline, candidate, %Comparison{} = comparison) do
    %__MODULE__{
      accepted?: false,
      mode: :observe_only,
      reason: :advisory_only,
      baseline_source: baseline.source,
      candidate_source: candidate.source,
      winner: comparison.winner,
      delta: comparison.delta,
      proposed: candidate,
      applied: nil,
      meta: comparison_meta(comparison)
    }
  end

  defp comparison_meta(%Comparison{} = comparison) do
    %{
      baseline_mae: comparison.baseline_mae,
      candidate_mae: comparison.candidate_mae,
      baseline_model_version: comparison.baseline_model_version,
      candidate_model_version: comparison.candidate_model_version,
      comparison_reason: comparison.reason
    }
  end

  defp emit_decision(%__MODULE__{} = decision) do
    :telemetry.execute(
      @event,
      %{
        count: 1,
        accepted: if(decision.accepted?, do: 1, else: 0),
        delta: decision.delta
      },
      %{
        mode: decision.mode,
        reason: decision.reason,
        baseline_source: decision.baseline_source,
        candidate_source: decision.candidate_source,
        winner: decision.winner,
        v: decision.v
      }
    )
  end
end
