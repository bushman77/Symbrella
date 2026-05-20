defmodule Brain.BlackboardTelemetryBridgeTest do
  use ExUnit.Case, async: true

  alias Brain.Blackboard
  alias Brain.Bus

  setup do
    # Ensure Blackboard is running (it usually is under the app supervisor)
    if is_nil(Process.whereis(Brain.Blackboard)) do
      start_supervised!({Brain.Blackboard, []})
    end

    :ok = ensure_ready()
    :ok = Bus.subscribe(Blackboard.topic())

    :ok
  end

  test "bridges [:brain, :wm, :update] telemetry onto brain:blackboard" do
    :telemetry.execute(
      [:brain, :wm, :update],
      %{size: 2, added: 1, removed: 0, capacity: 7},
      %{reason: :test}
    )

    assert_receive {:blackboard,
                    %{
                      kind: :telemetry,
                      event: [:brain, :wm, :update],
                      measurements: meas,
                      meta: meta
                    }},
                   500

    assert meas.size == 2
    assert meas.added == 1
    assert meas.capacity == 7
    assert meta.reason == :test
  end

  test "bridges [:brain, :pipeline, :lifg_stage1, :stop] telemetry onto brain:blackboard" do
    :telemetry.execute(
      [:brain, :pipeline, :lifg_stage1, :stop],
      %{duration_ms: 12},
      %{winners: 1, boosts: 1, inhibitions: 0}
    )

    assert_receive {:blackboard,
                    %{
                      kind: :telemetry,
                      event: [:brain, :pipeline, :lifg_stage1, :stop],
                      measurements: %{duration_ms: 12},
                      meta: %{winners: 1, boosts: 1, inhibitions: 0}
                    }},
                   500
  end

  test "bridges [:brain, :self_portrait, :monitor] telemetry onto brain:blackboard" do
    :telemetry.execute(
      [:brain, :self_portrait, :monitor],
      %{count: 1},
      %{issue: :lifg_payload_gap, severity: :warning}
    )

    assert_receive {:blackboard,
                    %{
                      kind: :telemetry,
                      event: [:brain, :self_portrait, :monitor],
                      measurements: %{count: 1},
                      meta: %{issue: :lifg_payload_gap, severity: :warning}
                    }},
                   500
  end

  test "bridges [:core, :response, :prompt] telemetry onto brain:blackboard" do
    :telemetry.execute(
      [:core, :response, :prompt],
      %{system_chars: 120, user_chars: 37},
      %{
        response_profile: "self_state_boundary",
        simulated_affect: "label=steady_care",
        system_prompt: "You are Symbrella.\nResponse profile: self_state_boundary.",
        system_truncated?: false
      }
    )

    assert_receive {:blackboard,
                    %{
                      kind: :telemetry,
                      event: [:core, :response, :prompt],
                      measurements: %{system_chars: 120, user_chars: 37},
                      meta: %{
                        response_profile: "self_state_boundary",
                        simulated_affect: "label=steady_care",
                        system_prompt: prompt,
                        system_truncated?: false
                      }
                    }},
                   500

    assert prompt =~ "Response profile: self_state_boundary"
  end

  test "bridges [:core, :response, :complete] telemetry onto brain:blackboard" do
    :telemetry.execute(
      [:core, :response, :complete],
      %{assistant_chars: 18, user_chars: 12},
      %{
        user_text: "close loop",
        assistant_text: "Loop is closed.",
        tone: :warm,
        mode: :explainer,
        response_profile: :brain_explainer,
        symbolic_frame: %{intent: :command}
      }
    )

    assert_receive {:blackboard,
                    %{
                      kind: :telemetry,
                      event: [:core, :response, :complete],
                      measurements: %{assistant_chars: 18, user_chars: 12},
                      meta: %{
                        assistant_text: "Loop is closed.",
                        response_profile: :brain_explainer,
                        symbolic_frame: %{intent: :command}
                      }
                    }},
                   500
  end

  defp ensure_ready do
    wait_until(fn ->
      s = Blackboard.state()
      s[:telemetry_attached?] == true
    end)
  end

  defp wait_until(fun, timeout_ms \\ 500, step_ms \\ 20) when is_function(fun, 0) do
    t0 = System.monotonic_time(:millisecond)

    cond do
      fun.() ->
        :ok

      System.monotonic_time(:millisecond) - t0 > timeout_ms ->
        {:error, :timeout}

      true ->
        Process.sleep(step_ms)
        wait_until(fun, timeout_ms, step_ms)
    end
  end
end
