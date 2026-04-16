defmodule Brain.SelfCalibration.AxonModel do
  @moduledoc """
  Axon graph definition for self-calibration.

  This defines a tiny model that predicts the same bounded labels as the
  baseline predictor: confidence, uncertainty, and stability. It is advisory
  only and is not wired into Brain.SelfModel.
  """

  alias Brain.SelfCalibration.Dataset

  @input_name "features"
  @model_version "axon-tiny-v1"

  @spec build(keyword()) :: Axon.t()
  def build(opts \\ []) do
    hidden_units = Keyword.get(opts, :hidden_units, 8)

    @input_name
    |> Axon.input(shape: {nil, feature_count()})
    |> Axon.dense(hidden_units, activation: :relu)
    |> Axon.dense(output_count(), activation: :sigmoid)
  end

  def input_name, do: @input_name
  def model_version, do: @model_version
  def feature_names, do: Dataset.feature_names()
  def label_names, do: Dataset.label_names()
  def feature_count, do: length(feature_names())
  def output_count, do: length(label_names())
end
