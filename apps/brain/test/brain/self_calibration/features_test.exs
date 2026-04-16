defmodule Brain.SelfCalibration.FeaturesTest do
  use ExUnit.Case, async: true

  alias Brain.SelfCalibration.Features
  alias Brain.SelfCalibration.Sample

  test "builds a versioned calibration sample from self-model evidence" do
    self_model = %Brain.SelfModel{
      confidence: 0.7,
      uncertainty: 0.3,
      stability: 0.8,
      cognitive_load: 0.4,
      recent_errors: [%{kind: :lifg_payload_gaps, count: 1}],
      mood: %{
        mood: %{vigilance: 0.6, plasticity: 0.5, inhibition: 0.2}
      },
      last_appraisal: %{
        valence: -0.4,
        arousal: 0.7,
        dominance: -0.1,
        evidence: %{
          attribution: %{
            target: :assistant,
            source: :second_person,
            confidence: 0.9
          }
        }
      },
      last_lifg: %{choices_count: 2},
      v: 1
    }

    sample = Features.build_sample(self_model, source: :test)

    assert %Sample{} = sample
    assert sample.v == 1
    assert sample.source == :test
    assert sample.meta.feature_schema_v == 1
    assert sample.meta.self_model_v == 1

    assert sample.features.appraisal_valence == -0.4
    assert sample.features.appraisal_arousal == 0.7
    assert sample.features.appraisal_dominance == -0.1
    assert sample.features.attribution_confidence == 0.9
    assert sample.features.lifg_choices_count == 2
    assert sample.features.cognitive_load == 0.4
    assert sample.features.recent_error_count == 1
    assert sample.features.mood_vigilance == 0.6
    assert sample.features.mood_plasticity == 0.5
    assert sample.features.mood_inhibition == 0.2

    assert sample.labels.confidence == 0.7
    assert sample.labels.uncertainty == 0.3
    assert sample.labels.stability == 0.8
  end

  test "uses numeric defaults when evidence is sparse" do
    sample = Features.build_sample(%Brain.SelfModel{}, source: :test)

    assert sample.features.appraisal_valence == 0.0
    assert sample.features.attribution_confidence == 0.0
    assert sample.features.lifg_choices_count == 0
    assert sample.features.recent_error_count == 0

    assert sample.labels.confidence == 0.5
    assert sample.labels.uncertainty == 0.5
    assert sample.labels.stability == 0.5
  end
end
