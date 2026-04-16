defmodule Brain.SelfCalibration.Evaluator do
  @moduledoc """
  Compares advisory self-calibration predictions against observed labels.
  """

  alias Brain.SelfCalibration.Evaluation
  alias Brain.SelfCalibration.Prediction
  alias Brain.SelfCalibration.Sample

  @event [:brain, :self_calibration, :evaluation]
  @required_labels [:confidence, :uncertainty, :stability]

  @spec evaluate(Prediction.t(), map() | Sample.t(), keyword()) ::
          {:ok, Evaluation.t()} | {:error, term()}
  def evaluate(prediction, labels, opts \\ [])

  def evaluate(%Prediction{} = prediction, %Sample{labels: labels}, opts) do
    evaluate(prediction, labels, opts)
  end

  def evaluate(%Prediction{} = prediction, labels, opts) when is_map(labels) do
    with {:ok, actual} <- normalize_labels(labels) do
      confidence_error = abs(prediction.confidence - actual.confidence)
      uncertainty_error = abs(prediction.uncertainty - actual.uncertainty)
      stability_error = abs(prediction.stability - actual.stability)

      mae = (confidence_error + uncertainty_error + stability_error) / 3.0

      evaluation = %Evaluation{
        confidence_error: confidence_error,
        uncertainty_error: uncertainty_error,
        stability_error: stability_error,
        mae: mae,
        source: Keyword.get(opts, :source, prediction.source),
        model_version: prediction.model_version,
        feature_schema_v: prediction.feature_schema_v,
        meta: %{
          labels: actual,
          prediction_v: prediction.v
        }
      }

      emit_evaluation(evaluation)

      {:ok, evaluation}
    end
  end

  def evaluate(_prediction, _labels, _opts), do: {:error, :invalid_evaluation_input}

  defp normalize_labels(labels) do
    if Enum.all?(@required_labels, &present?(labels, &1)) do
      {:ok,
       %{
         confidence: number(Map.get(labels, :confidence, Map.get(labels, "confidence"))),
         uncertainty: number(Map.get(labels, :uncertainty, Map.get(labels, "uncertainty"))),
         stability: number(Map.get(labels, :stability, Map.get(labels, "stability")))
       }}
    else
      {:error, :missing_labels}
    end
  end

  defp emit_evaluation(%Evaluation{} = evaluation) do
    :telemetry.execute(
      @event,
      %{
        count: 1,
        confidence_error: evaluation.confidence_error,
        uncertainty_error: evaluation.uncertainty_error,
        stability_error: evaluation.stability_error,
        mae: evaluation.mae
      },
      %{
        source: evaluation.source,
        model_version: evaluation.model_version,
        feature_schema_v: evaluation.feature_schema_v,
        v: evaluation.v
      }
    )
  end

  defp present?(labels, key) do
    Map.has_key?(labels, key) or Map.has_key?(labels, to_string(key))
  end

  defp number(value) when is_integer(value), do: value * 1.0
  defp number(value) when is_float(value), do: value

  defp number(value) when is_binary(value) do
    case Float.parse(value) do
      {parsed, ""} -> parsed
      _ -> 0.0
    end
  end

  defp number(_), do: 0.0
end
