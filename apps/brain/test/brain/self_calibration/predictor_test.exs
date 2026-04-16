defmodule Brain.SelfCalibration.PredictorTest do
  use ExUnit.Case, async: true

  alias Brain.SelfCalibration.Dataset
  alias Brain.SelfCalibration.Prediction
  alias Brain.SelfCalibration.Predictor
  alias Brain.SelfCalibration.Sample
  alias Brain.SelfCalibration.Tensor

  test "predicts advisory bounded self-state values from tensor batch" do
    rows =
      Dataset.to_rows([
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
      ])

    assert {:ok, batch} = Tensor.from_rows(rows)
    assert {:ok, %Prediction{} = prediction} = Predictor.predict(batch, source: :baseline)

    assert prediction.source == :baseline
    assert prediction.model_version == "baseline-v1"
    assert prediction.confidence >= 0.0 and prediction.confidence <= 1.0
    assert prediction.uncertainty >= 0.0 and prediction.uncertainty <= 1.0
    assert prediction.stability >= 0.0 and prediction.stability <= 1.0
    assert Float.round(prediction.meta.cognitive_load, 4) == 0.4
    assert Float.round(prediction.meta.attribution_confidence, 4) == 0.9
    assert Float.round(prediction.meta.recent_error_count, 4) == 1.0
  end

  test "emits prediction telemetry" do
    id = "self-calibration-prediction-test-#{System.unique_integer([:positive])}"
    parent = self()

    :ok =
      :telemetry.attach(
        id,
        [:brain, :self_calibration, :prediction],
        fn _event, measurements, metadata, pid ->
          send(pid, {:prediction, measurements, metadata})
        end,
        parent
      )

    on_exit(fn -> :telemetry.detach(id) end)

    rows =
      Dataset.to_rows([
        %Sample{
          features: %{attribution_confidence: 0.8, cognitive_load: 0.2},
          labels: %{confidence: 0.8, uncertainty: 0.2, stability: 0.8}
        }
      ])

    assert {:ok, batch} = Tensor.from_rows(rows)
    assert {:ok, _prediction} = Predictor.predict(batch)

    assert_receive {:prediction, %{count: 1, confidence: confidence}, meta}, 500
    assert is_number(confidence)
    assert meta.source == :baseline
    assert meta.model_version == "baseline-v1"
    assert meta.feature_schema_v == 1
    assert meta.v == 1
  end
end
