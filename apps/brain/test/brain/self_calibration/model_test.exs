defmodule Brain.SelfCalibration.ModelTest do
  use ExUnit.Case, async: true

  alias Brain.SelfCalibration.Dataset
  alias Brain.SelfCalibration.Evaluation
  alias Brain.SelfCalibration.Evaluator
  alias Brain.SelfCalibration.Model
  alias Brain.SelfCalibration.Prediction
  alias Brain.SelfCalibration.Predictor
  alias Brain.SelfCalibration.Sample
  alias Brain.SelfCalibration.Tensor

  test "tiny linear model predicts bounded advisory self-state values" do
    assert {:ok, batch} = sample_batch()
    assert {:ok, %Prediction{} = prediction} = Model.predict(batch)

    assert prediction.source == :tiny_model
    assert prediction.model_version == "tiny-linear-v1"
    assert prediction.feature_schema_v == 1
    assert prediction.meta.kind == :linear

    assert_bounded(prediction.confidence)
    assert_bounded(prediction.uncertainty)
    assert_bounded(prediction.stability)
  end

  test "tiny model can be compared against baseline with evaluator" do
    sample = sample()
    rows = Dataset.to_rows([sample])

    assert {:ok, batch} = Tensor.from_rows(rows)
    assert {:ok, %Prediction{} = baseline_prediction} = Predictor.predict(batch)
    assert {:ok, %Prediction{} = model_prediction} = Model.predict(batch)

    assert {:ok, %Evaluation{} = baseline_eval} =
             Evaluator.evaluate(baseline_prediction, sample)

    assert {:ok, %Evaluation{} = model_eval} =
             Evaluator.evaluate(model_prediction, sample)

    assert baseline_eval.source == :baseline
    assert model_eval.source == :tiny_model

    assert_bounded(baseline_eval.mae)
    assert_bounded(model_eval.mae)
  end

  defp sample_batch do
    sample()
    |> then(&Dataset.to_rows([&1]))
    |> Tensor.from_rows()
  end

  defp sample do
    %Sample{
      features: %{
        appraisal_valence: -0.2,
        appraisal_arousal: 0.5,
        appraisal_dominance: -0.1,
        attribution_confidence: 0.9,
        lifg_choices_count: 2,
        cognitive_load: 0.4,
        recent_error_count: 1,
        mood_vigilance: 0.6,
        mood_plasticity: 0.7,
        mood_inhibition: 0.3
      },
      labels: %{confidence: 0.8, uncertainty: 0.2, stability: 0.75}
    }
  end

  defp assert_bounded(value) do
    assert is_number(value)
    assert value >= 0.0
    assert value <= 1.0
  end
end
