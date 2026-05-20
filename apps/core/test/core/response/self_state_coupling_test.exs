defmodule Core.Response.SelfStateCouplingTest do
  use ExUnit.Case, async: false

  alias Core.Response

  setup do
    if Code.ensure_loaded?(Brain.MoodCore) and is_nil(Process.whereis(Brain.MoodCore)) do
      start_supervised!(Brain.MoodCore)
    end

    :ok
  end

  test "plan/2 exposes self-state effects and emits mode selection telemetry" do
    handler_id = "response-mode-selected-#{System.unique_integer([:positive])}"
    parent = self()

    :ok =
      :telemetry.attach(
        handler_id,
        [:brain, :response, :mode_selected],
        fn event, meas, meta, _ ->
          send(parent, {:telemetry, event, meas, meta})
        end,
        nil
      )

    {_tone, _text, meta} =
      Response.plan(
        %{
          intent: :command,
          confidence: 0.9,
          text: "fix the response planner",
          self_model: %{
            v: 1,
            uncertainty: 0.84,
            confidence: 0.4,
            stability: 0.6,
            cognitive_load: 0.2,
            focus: :clarify
          }
        },
        %{mood: %{vigilance: 0.2, inhibition: 0.2, exploration: 0.3, plasticity: 0.5}}
      )

    assert meta.mode == :coach
    assert meta.action == :offer_options
    assert :self_state_clarify in meta.overrides
    assert :hedge_under_uncertainty in meta.self_state_effects

    assert_receive {:telemetry, [:brain, :response, :mode_selected], %{count: 1}, telemetry_meta},
                   200

    assert telemetry_meta.v == 1
    assert telemetry_meta.self_model_v == 1
    assert telemetry_meta.focus == :clarify
    assert :hedge_under_uncertainty in telemetry_meta.effects

    :telemetry.detach(handler_id)
  end
end
