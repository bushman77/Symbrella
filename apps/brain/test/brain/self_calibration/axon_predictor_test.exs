defmodule Brain.SelfCalibration.AxonPredictorTest do
  use ExUnit.Case, async: true

  alias Brain.SelfCalibration.AxonPredictor
  alias Brain.SelfCalibration.Comparison
  alias Brain.SelfCalibration.Dataset
  alias Brain.SelfCalibration.Evaluation
  alias Brain.SelfCalibration.Evaluator
  alias Brain.SelfCalibration.Prediction
  alias Brain.SelfCalibration.Predictor
  alias Brain.SelfCalibration.Sample
  alias Brain.SelfCalibration.Tensor
  alias Brain.SelfCalibration.Training

  test "predicts bounded advisory values from initialized axon artifact" do
    sample = sample()
    assert {:ok, batch} = sample_batch(sample)
    assert {:ok, artifact} = Training.initialize(batch, hidden_units: 4)

    assert {:ok, %Prediction{} = prediction} = AxonPredictor.predict(artifact, batch)

    assert prediction.source == :axon
    assert prediction.model_version == "axon-tiny-v1"
    assert prediction.feature_schema_v == 1
    assert prediction.meta.training_status == :initialized
    assert prediction.meta.label_names == Dataset.label_names()

    assert_bounded(prediction.confidence)
    assert_bounded(prediction.uncertainty)
    assert_bounded(prediction.stability)
  end

  test "predicts from trained axon artifact and compares against baseline" do
    sample = sample()
    assert {:ok, batch} = sample_batch(sample)

    assert {:ok, baseline_prediction} = Predictor.predict(batch)

    assert {:ok, %Evaluation{} = baseline_eval} =
             Evaluator.evaluate(baseline_prediction, sample)

    assert {:ok, artifact} = Training.train(batch, epochs: 1, hidden_units: 4)
    assert {:ok, axon_prediction} = AxonPredictor.predict(artifact, batch)

    assert axon_prediction.source == :axon
    assert axon_prediction.meta.training_status == :trained

    assert {:ok, %Evaluation{} = axon_eval} =
             Evaluator.evaluate(axon_prediction, sample)

    assert {:ok, comparison} = Comparison.compare(baseline_eval, axon_eval)

    assert comparison.baseline_source == :baseline
    assert comparison.candidate_source == :axon
    assert comparison.candidate_model_version == "axon-tiny-v1"
    assert comparison.advisory? == true
    assert comparison.winner in [:baseline, :candidate, :tie]
  end

  test "rejects artifact with mismatched schema" do
    sample = sample()
    assert {:ok, batch} = sample_batch(sample)
    assert {:ok, artifact} = Training.initialize(batch)

    bad_artifact = %{artifact | feature_names: [:wrong]}

    assert AxonPredictor.predict(bad_artifact, batch) == {:error, :schema_mismatch}
  end

  defp sample_batch(sample) do
    sample
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
