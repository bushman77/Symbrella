defmodule Brain.SelfCalibration.TensorTest do
  use ExUnit.Case, async: true

  alias Brain.SelfCalibration.Dataset
  alias Brain.SelfCalibration.Sample
  alias Brain.SelfCalibration.Tensor

  test "converts dataset rows into Nx tensors when Nx is available" do
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
          labels: %{
            confidence: 0.8,
            uncertainty: 0.2,
            stability: 0.75
          }
        }
      ])

    if Tensor.nx_available?() do
      assert {:ok, batch} = Tensor.from_rows(rows)

      assert batch.feature_names == Dataset.feature_names()
      assert batch.label_names == Dataset.label_names()
      assert apply(Nx, :shape, [batch.x]) == {1, 10}
      assert apply(Nx, :shape, [batch.y]) == {1, 3}

      assert Enum.map(apply(Nx, :to_flat_list, [batch.x]), &Float.round(&1, 4)) ==
               [-0.2, 0.5, -0.1, 0.9, 2.0, 0.4, 1.0, 0.6, 0.7, 0.3]

      assert Enum.map(apply(Nx, :to_flat_list, [batch.y]), &Float.round(&1, 4)) ==
               [0.8, 0.2, 0.75]
    else
      assert Tensor.from_rows(rows) == {:error, :nx_unavailable}
    end
  end

  test "rejects mismatched row counts" do
    rows = %{
      x: [[1.0]],
      y: [[1.0], [0.0]],
      feature_names: [:a],
      label_names: [:b]
    }

    assert Tensor.from_rows(rows) == {:error, :row_count_mismatch}
  end

  test "rejects non-rectangular rows" do
    rows = %{
      x: [[1.0], [1.0, 2.0]],
      y: [[1.0], [0.0]],
      feature_names: [:a],
      label_names: [:b]
    }

    assert Tensor.from_rows(rows) == {:error, :invalid_feature_rows}
  end
end
