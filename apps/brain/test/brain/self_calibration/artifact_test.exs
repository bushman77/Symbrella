defmodule Brain.SelfCalibration.ArtifactTest do
  use ExUnit.Case, async: true

  alias Brain.SelfCalibration.Artifact
  alias Brain.SelfCalibration.AxonModel
  alias Brain.SelfCalibration.Dataset
  alias Brain.SelfCalibration.Sample
  alias Brain.SelfCalibration.Tensor
  alias Brain.SelfCalibration.Training

  test "saves and loads a trained calibration artifact" do
    path =
      Path.join(
        System.tmp_dir!(),
        "symbrella-self-calibration-artifact-#{System.unique_integer([:positive])}.bin"
      )

    on_exit(fn -> File.rm(path) end)

    assert {:ok, batch} = sample_batch()
    assert {:ok, %Training{} = artifact} = Training.train(batch, epochs: 1, hidden_units: 4)

    assert :ok = Artifact.save(artifact, path)
    assert {:ok, %Training{} = loaded} = Artifact.load(path)

    assert loaded.status == :trained
    assert loaded.source == :axon
    assert loaded.model_version == AxonModel.model_version()
    assert loaded.feature_names == Dataset.feature_names()
    assert loaded.label_names == Dataset.label_names()
    assert loaded.feature_schema_v == 1
    assert loaded.metrics.epochs == 1
    assert loaded.meta.trained? == true
    assert loaded.params != nil
    assert inspect(loaded.model) =~ "#Axon<"
  end

  test "load/1 rejects invalid artifact files" do
    path =
      Path.join(
        System.tmp_dir!(),
        "symbrella-invalid-artifact-#{System.unique_integer([:positive])}.bin"
      )

    on_exit(fn -> File.rm(path) end)

    File.write!(path, "not an artifact")

    assert Artifact.load(path) == {:error, :invalid_artifact_binary}
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
