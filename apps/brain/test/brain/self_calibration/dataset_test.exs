defmodule Brain.SelfCalibration.DatasetTest do
  use ExUnit.Case, async: true

  alias Brain.SelfCalibration.AxonModel
  alias Brain.SelfCalibration.Dataset
  alias Brain.SelfCalibration.Sample

  test "converts samples into stable feature and label rows" do
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
      labels: %{
        confidence: 0.8,
        uncertainty: 0.2,
        stability: 0.75
      }
    }

    rows = Dataset.to_rows([sample])

    assert rows.feature_names == [
             :appraisal_valence,
             :appraisal_arousal,
             :appraisal_dominance,
             :attribution_confidence,
             :lifg_choices_count,
             :cognitive_load,
             :recent_error_count,
             :mood_vigilance,
             :mood_plasticity,
             :mood_inhibition
           ]

    assert rows.label_names == [:confidence, :uncertainty, :stability]

    assert rows.x == [[-0.2, 0.5, -0.1, 0.9, 2.0, 0.4, 1.0, 0.6, 0.7, 0.3]]
    assert rows.y == [[0.8, 0.2, 0.75]]
  end

  test "loads JSONL samples and skips invalid lines" do
    path =
      Path.join(
        System.tmp_dir!(),
        "symbrella-self-calibration-dataset-#{System.unique_integer([:positive])}.jsonl"
      )

    on_exit(fn -> File.rm(path) end)

    sample = %Sample{
      features: %{appraisal_valence: -0.2, attribution_confidence: 0.9},
      labels: %{confidence: 0.8},
      source: :test,
      meta: %{feature_schema_v: 1},
      v: 1
    }

    encoded = sample |> Map.from_struct() |> Jason.encode!()

    File.write!(path, encoded <> "\nnot-json\n\n")

    assert {:ok, [loaded]} = Dataset.load_jsonl(path)
    assert loaded.source == :test
    assert loaded.features.appraisal_valence == -0.2
    assert loaded.features.attribution_confidence == 0.9
    assert loaded.labels.confidence == 0.8

    rows = Dataset.to_rows([loaded])

    assert rows.x == [[-0.2, 0.0, 0.0, 0.9, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]]
    assert rows.y == [[0.8, 0.0, 0.0]]
  end

  test "loads synthetic JSONL source without rewriting provenance" do
    path =
      Path.join(
        System.tmp_dir!(),
        "symbrella-self-calibration-synthetic-#{System.unique_integer([:positive])}.jsonl"
      )

    on_exit(fn -> File.rm(path) end)

    encoded =
      Jason.encode!(%{
        v: 1,
        source: "synthetic",
        features: %{
          appraisal_valence: 0.1,
          appraisal_arousal: 0.2,
          appraisal_dominance: 0.3,
          attribution_confidence: 0.4,
          lifg_choices_count: 2,
          cognitive_load: 0.5,
          recent_error_count: 1,
          mood_vigilance: 0.6,
          mood_plasticity: 0.7,
          mood_inhibition: 0.8
        },
        labels: %{confidence: 0.9, uncertainty: 0.1, stability: 0.85},
        meta: %{feature_schema_v: 1, label_source: "synthetic_symbrella_guided"}
      })

    File.write!(path, encoded <> "\n")

    assert {:ok, [loaded]} = Dataset.load_jsonl(path)
    assert loaded.source == :synthetic
    assert loaded.meta.label_source == "synthetic_symbrella_guided"

    rows = Dataset.to_rows([loaded])

    assert rows.x == [[0.1, 0.2, 0.3, 0.4, 2.0, 0.5, 1.0, 0.6, 0.7, 0.8]]
    assert rows.y == [[0.9, 0.1, 0.85]]
  end

  test "missing values default to zero rows" do
    rows = Dataset.to_rows([%Sample{}])

    assert rows.x == [[0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]]
    assert rows.y == [[0.0, 0.0, 0.0]]
  end

  test "feature and label schemas stay aligned with the Axon boundary" do
    assert Dataset.feature_names() == AxonModel.feature_names()
    assert Dataset.label_names() == AxonModel.label_names()
    assert length(Dataset.feature_names()) == AxonModel.feature_count()
    assert length(Dataset.label_names()) == AxonModel.output_count()
  end
end
