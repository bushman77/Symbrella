defmodule Core.TrainingArchive.RuntimeObservationTest do
  use ExUnit.Case, async: true

  alias Core.TrainingArchive.RuntimeObservation

  test "projects real raw and derived MoodCore values without inventing self-model state" do
    snapshot = %{
      levels: %{da: 0.62, "5ht": 0.48, glu: 0.83, ne: 0.72},
      mood: %{exploration: 0.68, inhibition: 0.51, vigilance: 0.74, plasticity: 0.43}
    }

    observed = RuntimeObservation.from_snapshot(snapshot)
    assert observed.source == "Brain.MoodCore.snapshot"
    assert observed.status == "captured"
    assert observed.levels == %{
             "da" => 0.62, "5ht" => 0.48, "glu" => 0.83, "ne" => 0.72
           }
    assert observed.mood["exploration"] == 0.68
    assert observed.mood["plasticity"] == 0.43
    assert is_binary(observed.captured_at)
    refute Map.has_key?(observed, :self_model)
  end

  test "partial snapshots explicitly retain missing measurements" do
    partial = RuntimeObservation.from_snapshot(%{
      levels: %{"da" => 0.0, "5ht" => 0.51},
      mood: %{"exploration" => 0.32}
    })

    assert partial.status == "partial"
    assert partial.levels["da"] == 0.0
    assert partial.levels["glu"] == nil
    assert partial.levels["ne"] == nil
    assert partial.mood["exploration"] == 0.32
    assert partial.mood["vigilance"] == nil
  end

  test "invalid snapshot is unavailable, not an invented zero baseline" do
    assert %{status: "unavailable", levels: %{}, mood: %{}} =
             RuntimeObservation.from_snapshot(nil)
  end
end
