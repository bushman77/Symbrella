defmodule Brain.SelfCalibration.Predictor do
  @moduledoc """
  Runtime prediction boundary for self-calibration.

  This is intentionally model-agnostic. Today it can produce a deterministic
  baseline from dataset rows; later it can delegate to an Axon model.
  """

  alias Brain.SelfCalibration.Prediction

  @event [:brain, :self_calibration, :prediction]

  @spec predict(map(), keyword()) :: {:ok, Prediction.t()} | {:error, term()}
  def predict(batch, opts \\ [])

  def predict(%{x: x, feature_names: feature_names} = batch, opts)
      when is_list(feature_names) do
    source = Keyword.get(opts, :source, :baseline)

    with {:ok, row} <- first_feature_row(x),
         prediction <- baseline_prediction(row, feature_names, source) do
      emit_prediction(prediction, batch)
      {:ok, prediction}
    end
  end

  def predict(_batch, _opts), do: {:error, :invalid_batch}

  defp first_feature_row(x) when is_list(x) do
    case x do
      [row | _] when is_list(row) -> {:ok, row}
      _ -> {:error, :empty_batch}
    end
  end

  defp first_feature_row(x) do
    if Code.ensure_loaded?(Nx) and function_exported?(Nx, :to_list, 1) do
      case Nx.to_list(x) do
        [row | _] when is_list(row) -> {:ok, row}
        _ -> {:error, :empty_batch}
      end
    else
      {:error, :nx_unavailable}
    end
  end

  defp baseline_prediction(row, feature_names, source) do
    features =
      feature_names
      |> Enum.zip(row)
      |> Map.new(fn {key, value} -> {key, number(value)} end)

    cognitive_load = Map.get(features, :cognitive_load, 0.0)
    attribution_confidence = Map.get(features, :attribution_confidence, 0.0)
    recent_error_count = Map.get(features, :recent_error_count, 0.0)

    uncertainty =
      0.35 + cognitive_load * 0.25 + min(recent_error_count, 5.0) * 0.05 -
        attribution_confidence * 0.10

    uncertainty = clamp01(uncertainty)
    confidence = clamp01(1.0 - uncertainty)
    stability = clamp01(0.75 - cognitive_load * 0.20 - min(recent_error_count, 5.0) * 0.05)

    %Prediction{
      confidence: confidence,
      uncertainty: uncertainty,
      stability: stability,
      source: source,
      model_version: "baseline-v1",
      feature_schema_v: 1,
      meta: %{
        cognitive_load: cognitive_load,
        attribution_confidence: attribution_confidence,
        recent_error_count: recent_error_count
      }
    }
  end

  defp emit_prediction(%Prediction{} = prediction, batch) do
    :telemetry.execute(
      @event,
      %{
        count: 1,
        confidence: prediction.confidence,
        uncertainty: prediction.uncertainty,
        stability: prediction.stability
      },
      %{
        source: prediction.source,
        model_version: prediction.model_version,
        feature_schema_v: prediction.feature_schema_v,
        feature_count: length(Map.get(batch, :feature_names, [])),
        label_count: length(Map.get(batch, :label_names, [])),
        v: prediction.v
      }
    )
  end

  defp number(value) when is_integer(value), do: value * 1.0
  defp number(value) when is_float(value), do: value
  defp number(_), do: 0.0

  defp clamp01(value) when is_number(value), do: value |> max(0.0) |> min(1.0)
end
