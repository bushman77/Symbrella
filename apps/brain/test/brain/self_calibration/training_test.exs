defmodule Brain.SelfCalibration.TrainingTest do
  use ExUnit.Case, async: true

  alias Brain.SelfCalibration.AxonModel
  alias Brain.SelfCalibration.Dataset
  alias Brain.SelfCalibration.Sample
  alias Brain.SelfCalibration.Tensor
  alias Brain.SelfCalibration.Training

  test "initializes advisory axon artifact from tensor batch" do
    assert {:ok, batch} = sample_batch()
    assert {:ok, %Training{} = artifact} = Training.initialize(batch, hidden_units: 4)

    assert artifact.status == :initialized
    assert artifact.source == :axon
    assert artifact.model_version == "axon-tiny-v1"
    assert artifact.input_name == "features"
    assert artifact.feature_names == Dataset.feature_names()
    assert artifact.label_names == Dataset.label_names()
    assert artifact.feature_schema_v == 1
    assert artifact.meta.batch_size == 1
    assert artifact.meta.hidden_units == 4
    assert artifact.meta.trained? == false
    assert artifact.params != nil
    assert inspect(artifact.model) =~ "#Axon<"
  end

  test "rejects batches with mismatched schema" do
    assert {:ok, batch} = sample_batch()

    bad_batch = %{batch | feature_names: [:wrong]}

    assert Training.initialize(bad_batch) == {:error, :schema_mismatch}
  end

  test "trains tiny advisory axon artifact for one epoch" do
    assert {:ok, batch} = sample_batch()
    assert {:ok, %Training{} = artifact} = Training.train(batch, epochs: 1, hidden_units: 4)

    assert artifact.status == :trained
    assert artifact.source == :axon
    assert artifact.model_version == AxonModel.model_version()
    assert artifact.metrics.epochs == 1
    assert artifact.meta.trained? == true
    assert artifact.params != nil
  end

  defp sample_batch do
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
    |> then(&Dataset.to_rows([&1]))
    |> Tensor.from_rows()
  end
end
