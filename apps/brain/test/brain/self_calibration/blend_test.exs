defmodule Brain.SelfCalibration.BlendTest do
  use ExUnit.Case, async: true

  alias Brain.SelfCalibration.Blend
  alias Brain.SelfCalibration.Comparison
  alias Brain.SelfCalibration.Prediction

  test "defaults to observe-only even when candidate wins" do
    baseline = prediction(:baseline, "baseline-v1")
    candidate = prediction(:axon, "axon-tiny-v1")
    comparison = comparison(:candidate, -0.03)

    assert {:ok, %Blend{} = decision} = Blend.decide(baseline, candidate, comparison)

    assert decision.accepted? == false
    assert decision.mode == :observe_only
    assert decision.reason == :advisory_only
    assert decision.proposed == candidate
    assert decision.applied == nil
    assert decision.winner == :candidate
    assert decision.delta == -0.03
  end

  test "accepts candidate only when candidate mode is explicit and candidate wins" do
    baseline = prediction(:baseline, "baseline-v1")
    candidate = prediction(:axon, "axon-tiny-v1")
    comparison = comparison(:candidate, -0.03)

    assert {:ok, decision} = Blend.decide(baseline, candidate, comparison, mode: :candidate)

    assert decision.accepted? == true
    assert decision.mode == :candidate
    assert decision.reason == :candidate_lower_mae
    assert decision.proposed == candidate
    assert decision.applied == candidate
  end

  test "does not accept candidate mode when baseline wins" do
    baseline = prediction(:baseline, "baseline-v1")
    candidate = prediction(:axon, "axon-tiny-v1")
    comparison = comparison(:baseline, 0.04)

    assert {:ok, decision} = Blend.decide(baseline, candidate, comparison, mode: :candidate)

    assert decision.accepted? == false
    assert decision.mode == :observe_only
    assert decision.reason == :candidate_not_better
    assert decision.applied == nil
  end

  test "emits blend telemetry" do
    id = "self-calibration-blend-test-#{System.unique_integer([:positive])}"
    parent = self()

    :ok =
      :telemetry.attach(
        id,
        [:brain, :self_calibration, :blend],
        &__MODULE__.handle_blend_event/4,
        parent
      )

    on_exit(fn -> :telemetry.detach(id) end)

    baseline = prediction(:baseline, "baseline-v1")
    candidate = prediction(:axon, "axon-tiny-v1")
    comparison = comparison(:candidate, -0.03)

    assert {:ok, _decision} = Blend.decide(baseline, candidate, comparison, mode: :candidate)

    assert_receive {:blend_event, measurements, metadata}, 500

    assert measurements.count == 1
    assert measurements.accepted == 1
    assert Float.round(measurements.delta, 4) == -0.03
    assert metadata.mode == :candidate
    assert metadata.reason == :candidate_lower_mae
    assert metadata.baseline_source == :baseline
    assert metadata.candidate_source == :axon
    assert metadata.winner == :candidate
    assert metadata.v == 1
  end

  test "rejects invalid inputs" do
    assert Blend.decide(%{}, %Prediction{}, %Comparison{}) == {:error, :invalid_blend_input}
  end

  def handle_blend_event(_event, measurements, metadata, pid) do
    send(pid, {:blend_event, measurements, metadata})
  end

  defp prediction(source, model_version) do
    %Prediction{
      confidence: 0.7,
      uncertainty: 0.3,
      stability: 0.8,
      source: source,
      model_version: model_version,
      feature_schema_v: 1
    }
  end

  defp comparison(winner, delta) do
    %Comparison{
      baseline_mae: 0.12,
      candidate_mae: 0.09,
      delta: delta,
      winner: winner,
      reason: if(winner == :tie, do: :equal_mae, else: :lower_mae),
      baseline_source: :baseline,
      candidate_source: :axon,
      baseline_model_version: "baseline-v1",
      candidate_model_version: "axon-tiny-v1",
      feature_schema_v: 1
    }
  end
end
