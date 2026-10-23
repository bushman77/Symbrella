defmodule Brain.Neural.SignalReceptorTest do
  use ExUnit.Case, async: false

  alias Brain.Neural.SignalReceptor

  @prediction [:brain, :neural, :state_prediction]
  @evaluation [:brain, :neural, :state_evaluation]

  setup do
    if is_nil(Process.whereis(SignalReceptor)) do
      start_supervised!(SignalReceptor)
    end

    :ok
  end

  defp meta do
    %{
      source: :axon_residual_v4,
      mode: :shadow_only,
      verification: :model_prediction_not_verified,
      deployment_authorized?: false,
      model_version: "state-transition-residual-v4",
      model_sha256: String.duplicate("a", 64),
      horizon: :next_completed_turn_in_same_session,
      checkpoint_assessment: :not_ready,
      observed: %{mood_da: 0.3, confidence: 0.4},
      forecast: %{mood_da: 0.32, confidence: 0.38},
      predicted_change: %{mood_da: 0.02, confidence: -0.02}
    }
  end

  test "receives real model-format neural telemetry as bounded non-authoritative observations" do
    before_count = SignalReceptor.status().predictions

    :telemetry.execute(@prediction, %{count: 1, observed_features: 2}, meta())

    status = SignalReceptor.status()
    assert status.predictions == before_count + 1
    assert status.status == :receiving_shadow_signals
    assert status.influence == :observation_only
    assert status.deployment_authorized? == false

    assert %{event: @prediction, meta: received, measurements: measurements} =
      SignalReceptor.latest()

    assert measurements.observed_features == 2
    assert received.observed.mood_da == 0.3
    assert received.forecast.confidence == 0.38
    assert received.predicted_change.mood_da == 0.02
    assert received.checkpoint_assessment == :not_ready
    assert received.verification == :model_prediction_not_verified
    refute Map.has_key?(received, :user_text)
    refute Map.has_key?(received, :assistant_text)
    assert length(SignalReceptor.history(3)) <= 3
  end

  test "receives delayed evaluation without confusing error measurements with live mood" do
    before_count = SignalReceptor.status().evaluations

    :telemetry.execute(@evaluation, %{count: 1, valid_feature_pairs: 9},
      meta()
      |> Map.take([:source, :mode, :verification, :deployment_authorized?,
        :model_version, :model_sha256, :checkpoint_assessment])
      |> Map.merge(%{verdict: "persistence_better",
        residual_mse: 0.03, persistence_mse: 0.02}))

    status = SignalReceptor.status()
    assert status.evaluations == before_count + 1
    assert status.latest.event == @evaluation
    assert status.latest.meta.verdict == "persistence_better"
    assert status.latest.meta.deployment_authorized? == false
  end

  test "rejects any synthetic or authoritative-looking event" do
    before_count = SignalReceptor.status().predictions

    for altered <- [
      %{meta() | mode: :production},
      %{meta() | source: :unverified_external},
      %{meta() | deployment_authorized?: true},
      %{meta() | verification: :verified_fact}
    ] do
      :telemetry.execute(@prediction, %{count: 1}, altered)
    end

    :telemetry.execute(@prediction, %{count: 0}, meta())
    assert SignalReceptor.status().predictions == before_count
  end
end
