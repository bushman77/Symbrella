defmodule Brain.SelfCalibration.BlendAdvisoryContractTest do
  use ExUnit.Case, async: true

  alias Brain.SelfCalibration.{Blend, Comparison, Prediction}

  test "a better neural candidate remains advisory unless explicitly enabled" do
    baseline = %Prediction{source: :baseline, model_version: "baseline-v1", feature_schema_v: 1}
    axon = %Prediction{source: :axon, model_version: "axon-tiny-v1", feature_schema_v: 1}
    comparison = %Comparison{
      winner: :candidate,
      baseline_source: :baseline,
      candidate_source: :axon,
      baseline_mae: 0.2,
      candidate_mae: 0.1,
      delta: -0.1,
      feature_schema_v: 1
    }

    for mode <- [:observe_only, :unknown, nil] do
      assert {:ok, decision} = Blend.decide(baseline, axon, comparison, mode: mode)
      assert decision.mode == :observe_only
      refute decision.accepted?
      assert decision.applied == nil
      assert decision.proposed == axon
      assert decision.winner == :candidate
    end

    assert {:ok, allowed} = Blend.decide(baseline, axon, comparison, mode: :candidate)
    assert allowed.applied == axon
    assert allowed.accepted?
  end

  test "neural candidates losing evaluation cannot be applied even in candidate mode" do
    baseline = %Prediction{source: :baseline}
    axon = %Prediction{source: :axon}

    for winner <- [:baseline, :tie] do
      comparison = %Comparison{winner: winner, baseline_source: :baseline, candidate_source: :axon}

      assert {:ok, decision} = Blend.decide(baseline, axon, comparison, mode: :candidate)
      refute decision.accepted?
      assert decision.applied == nil
    end
  end
end
