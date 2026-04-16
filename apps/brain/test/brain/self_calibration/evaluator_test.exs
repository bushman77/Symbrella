defmodule Brain.SelfCalibration.EvaluatorTest do
  use ExUnit.Case, async: true

  alias Brain.SelfCalibration.Evaluation
  alias Brain.SelfCalibration.Evaluator
  alias Brain.SelfCalibration.Prediction
  alias Brain.SelfCalibration.Sample

  test "evaluates prediction against label map" do
    prediction = %Prediction{
      confidence: 0.7,
      uncertainty: 0.3,
      stability: 0.8,
      source: :baseline,
      model_version: "baseline-v1",
      feature_schema_v: 1
    }

    labels = %{confidence: 0.8, uncertainty: 0.25, stability: 0.75}

    assert {:ok, %Evaluation{} = evaluation} = Evaluator.evaluate(prediction, labels)

    assert Float.round(evaluation.confidence_error, 4) == 0.1
    assert Float.round(evaluation.uncertainty_error, 4) == 0.05
    assert Float.round(evaluation.stability_error, 4) == 0.05
    assert Float.round(evaluation.mae, 4) == 0.0667

    assert evaluation.source == :baseline
    assert evaluation.model_version == "baseline-v1"
    assert evaluation.feature_schema_v == 1
  end

  test "evaluates prediction against sample labels" do
    prediction = %Prediction{confidence: 0.6, uncertainty: 0.4, stability: 0.7}

    sample = %Sample{
      labels: %{confidence: 0.5, uncertainty: 0.5, stability: 0.8}
    }

    assert {:ok, evaluation} = Evaluator.evaluate(prediction, sample)

    assert Float.round(evaluation.confidence_error, 4) == 0.1
    assert Float.round(evaluation.uncertainty_error, 4) == 0.1
    assert Float.round(evaluation.stability_error, 4) == 0.1
    assert Float.round(evaluation.mae, 4) == 0.1
  end

  test "accepts string label keys" do
    prediction = %Prediction{confidence: 0.9, uncertainty: 0.1, stability: 0.5}

    labels = %{
      "confidence" => "0.8",
      "uncertainty" => "0.2",
      "stability" => "0.7"
    }

    assert {:ok, evaluation} = Evaluator.evaluate(prediction, labels)

    assert Float.round(evaluation.confidence_error, 4) == 0.1
    assert Float.round(evaluation.uncertainty_error, 4) == 0.1
    assert Float.round(evaluation.stability_error, 4) == 0.2
    assert Float.round(evaluation.mae, 4) == 0.1333
  end

  test "returns an error when required labels are missing" do
    prediction = %Prediction{}

    assert {:error, :missing_labels} =
             Evaluator.evaluate(prediction, %{confidence: 0.5})
  end
end
