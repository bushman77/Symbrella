defmodule Brain.SelfCalibration.AxonPredictor do
  @moduledoc """
  Converts advisory Axon calibration artifacts into Prediction structs.

  This module performs inference only. It does not train, compare, or blend
  output into Brain.SelfModel.
  """

  alias Brain.SelfCalibration.AxonModel
  alias Brain.SelfCalibration.Prediction
  alias Brain.SelfCalibration.Training

  @spec predict(Training.t(), map()) :: {:ok, Prediction.t()} | {:error, term()}
  def predict(%Training{model: model, params: params} = artifact, batch) do
    with :ok <- validate_artifact(artifact),
         :ok <- validate_batch(batch),
         {:ok, row} <- first_output_row(model, params, batch.x),
         {:ok, fields} <- output_fields(row) do
      {:ok,
       %Prediction{
         confidence: fields.confidence,
         uncertainty: fields.uncertainty,
         stability: fields.stability,
         source: :axon,
         model_version: artifact.model_version,
         feature_schema_v: artifact.feature_schema_v,
         meta: %{
           training_status: artifact.status,
           input_name: artifact.input_name,
           label_names: artifact.label_names
         }
       }}
    end
  end

  def predict(_artifact, _batch), do: {:error, :invalid_axon_prediction_input}

  defp validate_artifact(%Training{model: nil}), do: {:error, :missing_model}
  defp validate_artifact(%Training{params: nil}), do: {:error, :missing_params}

  defp validate_artifact(%Training{feature_names: feature_names, label_names: label_names}) do
    if feature_names == AxonModel.feature_names() and label_names == AxonModel.label_names() do
      :ok
    else
      {:error, :schema_mismatch}
    end
  end

  defp validate_batch(%{feature_names: feature_names, label_names: label_names}) do
    if feature_names == AxonModel.feature_names() and label_names == AxonModel.label_names() do
      :ok
    else
      {:error, :schema_mismatch}
    end
  end

  defp validate_batch(_batch), do: {:error, :invalid_batch}

  defp first_output_row(model, params, x) do
    {_init_fn, predict_fn} = Axon.build(model)
    output = predict_fn.(params, x)

    case Nx.to_list(output) do
      [row | _] when is_list(row) -> {:ok, row}
      _ -> {:error, :empty_prediction}
    end
  end

  defp output_fields(row) do
    fields =
      AxonModel.label_names()
      |> Enum.zip(row)
      |> Map.new(fn {name, value} -> {name, clamp01(number(value))} end)

    with {:ok, confidence} <- required_field(fields, :confidence),
         {:ok, uncertainty} <- required_field(fields, :uncertainty),
         {:ok, stability} <- required_field(fields, :stability) do
      {:ok,
       %{
         confidence: confidence,
         uncertainty: uncertainty,
         stability: stability
       }}
    end
  end

  defp required_field(fields, key) do
    case Map.fetch(fields, key) do
      {:ok, value} -> {:ok, value}
      :error -> {:error, {:missing_output, key}}
    end
  end

  defp number(value) when is_integer(value), do: value * 1.0
  defp number(value) when is_float(value), do: value
  defp number(_), do: 0.0

  defp clamp01(value) when is_number(value), do: value |> max(0.0) |> min(1.0)
end
