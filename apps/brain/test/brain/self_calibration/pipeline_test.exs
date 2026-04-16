defmodule Brain.SelfCalibration.PipelineTest do
  use ExUnit.Case, async: true

  alias Brain.SelfCalibration.Comparison
  alias Brain.SelfCalibration.Dataset
  alias Brain.SelfCalibration.Evaluation
  alias Brain.SelfCalibration.Evaluator
  alias Brain.SelfCalibration.Model
  alias Brain.SelfCalibration.Prediction
  alias Brain.SelfCalibration.Predictor
  alias Brain.SelfCalibration.Sample
  alias Brain.SelfCalibration.Tensor

  test "runs the pre-axon calibration path through advisory comparison" do
    telemetry_id = "self-calibration-pipeline-test-#{System.unique_integer([:positive])}"
    parent = self()

    :ok =
      :telemetry.attach_many(
        telemetry_id,
        [
          [:brain, :self_calibration, :prediction],
          [:brain, :self_calibration, :evaluation]
        ],
        fn event, measurements, metadata, pid ->
          send(pid, {:calibration_event, event, measurements, metadata})
        end,
        parent
      )

    on_exit(fn -> :telemetry.detach(telemetry_id) end)

    sample = %Sample{
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

    rows = Dataset.to_rows([sample])

    assert rows.feature_names == Dataset.feature_names()
    assert rows.label_names == Dataset.label_names()

    assert {:ok, batch} = Tensor.from_rows(rows)
    assert {:ok, %Prediction{} = prediction} = Predictor.predict(batch)
    assert {:ok, %Evaluation{} = evaluation} = Evaluator.evaluate(prediction, sample)

    assert_bounded(prediction.confidence)
    assert_bounded(prediction.uncertainty)
    assert_bounded(prediction.stability)

    assert_bounded(evaluation.confidence_error)
    assert_bounded(evaluation.uncertainty_error)
    assert_bounded(evaluation.stability_error)
    assert_bounded(evaluation.mae)

    assert_receive {:calibration_event, [:brain, :self_calibration, :prediction],
                    %{count: 1, confidence: confidence}, prediction_meta},
                   500

    assert is_number(confidence)
    assert prediction_meta.source == :baseline
    assert prediction_meta.model_version == "baseline-v1"
    assert prediction_meta.feature_schema_v == 1
    assert prediction_meta.v == 1

    assert_receive {:calibration_event, [:brain, :self_calibration, :evaluation],
                    %{count: 1, mae: mae}, evaluation_meta},
                   500

    assert is_number(mae)
    assert evaluation_meta.source == :baseline
    assert evaluation_meta.model_version == "baseline-v1"
    assert evaluation_meta.feature_schema_v == 1
    assert evaluation_meta.v == 1
    assert {:ok, %Prediction{} = candidate_prediction} = Model.predict(batch)

    assert {:ok, %Evaluation{} = candidate_evaluation} =
             Evaluator.evaluate(candidate_prediction, sample)

    assert {:ok, %Comparison{} = comparison} =
             Comparison.compare(evaluation, candidate_evaluation)

    assert comparison.baseline_source == :baseline
    assert comparison.candidate_source == :tiny_model
    assert comparison.baseline_model_version == "baseline-v1"
    assert comparison.candidate_model_version == "tiny-linear-v1"
    assert comparison.advisory? == true
    assert comparison.winner in [:baseline, :candidate, :tie]
    assert comparison.reason in [:lower_mae, :equal_mae]
    assert is_number(comparison.delta)
  end

  defp assert_bounded(value) do
    assert is_number(value)
    assert value >= 0.0
    assert value <= 1.0
  end
end
