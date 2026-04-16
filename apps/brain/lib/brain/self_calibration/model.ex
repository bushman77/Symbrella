defmodule Brain.SelfCalibration.Model do
  @moduledoc """
  Advisory model boundary for self-calibration.

  The current tiny linear model is an Axon-ready stand-in. It produces the same
  bounded Prediction contract as the baseline predictor, but it is not wired
  into Brain.SelfModel or response behavior.
  """

  alias Brain.SelfCalibration.Prediction

  @type output_name :: :confidence | :uncertainty | :stability

  @type t :: %__MODULE__{
          kind: :linear,
          weights: %{optional(output_name()) => %{optional(atom()) => float()}},
          bias: %{optional(output_name()) => float()},
          source: atom(),
          model_version: String.t(),
          feature_schema_v: pos_integer(),
          meta: map(),
          v: pos_integer()
        }

  defstruct kind: :linear,
            weights: %{},
            bias: %{},
            source: :tiny_model,
            model_version: "tiny-linear-v1",
            feature_schema_v: 1,
            meta: %{},
            v: 1

  @spec tiny_linear() :: t()
  def tiny_linear do
    %__MODULE__{
      weights: %{
        confidence: %{
          appraisal_valence: 0.05,
          attribution_confidence: 0.15,
          cognitive_load: -0.15,
          recent_error_count: -0.04,
          mood_plasticity: 0.05
        },
        uncertainty: %{
          appraisal_arousal: 0.05,
          attribution_confidence: -0.10,
          cognitive_load: 0.20,
          recent_error_count: 0.05,
          mood_vigilance: 0.05
        },
        stability: %{
          cognitive_load: -0.12,
          recent_error_count: -0.04,
          mood_plasticity: 0.05,
          mood_inhibition: -0.05
        }
      },
      bias: %{confidence: 0.55, uncertainty: 0.35, stability: 0.65}
    }
  end

  @spec predict(map(), t()) :: {:ok, Prediction.t()} | {:error, term()}
  def predict(batch, model \\ tiny_linear())

  def predict(%{x: x, feature_names: feature_names}, %__MODULE__{} = model)
      when is_list(feature_names) do
    with {:ok, row} <- first_feature_row(x) do
      features =
        feature_names
        |> Enum.zip(row)
        |> Map.new(fn {key, value} -> {key, number(value)} end)

      {:ok,
       %Prediction{
         confidence: predict_output(:confidence, features, model),
         uncertainty: predict_output(:uncertainty, features, model),
         stability: predict_output(:stability, features, model),
         source: model.source,
         model_version: model.model_version,
         feature_schema_v: model.feature_schema_v,
         meta:
           Map.merge(model.meta, %{
             kind: model.kind,
             cognitive_load: Map.get(features, :cognitive_load, 0.0),
             attribution_confidence: Map.get(features, :attribution_confidence, 0.0),
             recent_error_count: Map.get(features, :recent_error_count, 0.0)
           })
       }}
    end
  end

  def predict(_batch, %__MODULE__{}), do: {:error, :invalid_batch}
  def predict(_batch, _model), do: {:error, :invalid_model}

  defp predict_output(name, features, %__MODULE__{} = model) do
    weights = Map.get(model.weights, name, %{})
    bias = Map.get(model.bias, name, 0.5)

    weights
    |> Enum.reduce(bias, fn {feature_name, weight}, acc ->
      acc + Map.get(features, feature_name, 0.0) * weight
    end)
    |> clamp01()
  end

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

  defp number(value) when is_integer(value), do: value * 1.0
  defp number(value) when is_float(value), do: value
  defp number(_), do: 0.0

  defp clamp01(value) when is_number(value), do: value |> max(0.0) |> min(1.0)
end
