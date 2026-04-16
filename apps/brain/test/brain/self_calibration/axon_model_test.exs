defmodule Brain.SelfCalibration.AxonModelTest do
  use ExUnit.Case, async: true

  alias Brain.SelfCalibration.AxonModel
  alias Brain.SelfCalibration.Dataset

  test "defines tiny axon calibration graph with stable input and output widths" do
    model = AxonModel.build()

    assert AxonModel.input_name() == "features"
    assert AxonModel.model_version() == "axon-tiny-v1"
    assert AxonModel.feature_names() == Dataset.feature_names()
    assert AxonModel.label_names() == Dataset.label_names()
    assert AxonModel.feature_count() == 10
    assert AxonModel.output_count() == 3

    assert inspect(model) =~ "#Axon<"
    assert inspect(model) =~ ~s("features")
  end

  test "allows hidden layer size to be configured without changing contract widths" do
    model = AxonModel.build(hidden_units: 4)

    assert AxonModel.feature_count() == 10
    assert AxonModel.output_count() == 3
    assert inspect(model) =~ "#Axon<"
  end
end
