defmodule Core.Agency.AutonomyLoopTest do
  use ExUnit.Case, async: false

  alias Core.Agency.AutonomyLoop
  alias Core.Agency.Command
  alias Core.Agency.Decision

  setup do
    case Process.whereis(Brain.GoalStack) do
      nil -> start_supervised!(Brain.GoalStack)
      _pid -> :ok
    end

    :ok = Brain.GoalStack.reset()
    :ok
  end

  test "endogenous impulse creates a goal, self-checks, and completes the goal" do
    trace_id = "agency|test|#{System.unique_integer([:positive])}"
    handler_id = "autonomy-loop-test-#{System.unique_integer([:positive])}"

    assert :ok =
             :telemetry.attach(
               handler_id,
               [:core, :agency, :autonomy, :transition],
               fn _event, measurements, metadata, pid ->
                 send(pid, {:autonomy_transition, measurements, metadata})
               end,
               self()
             )

    on_exit(fn -> :telemetry.detach(handler_id) end)

    result =
      AutonomyLoop.run_impulse(
        %{idle_ms: 61_000, salience_score: 0.9},
        %{trace_id: trace_id, reason: :endogenous, salience: %{lifg_anomalies: 1}},
        record_commands?: false
      )

    assert result.trace_id == trace_id
    assert %{status: :executed, reason: :goal_set} = result.set_goal_result
    assert %Decision{selected_action: :self_check, trace_id: ^trace_id} = result.decision
    assert [%Command{type: :run_self_check}] = result.commands
    assert [%{type: :run_self_check, status: :executed}] = result.command_results
    assert %{status: :executed, reason: :goal_completed} = result.complete_result
    assert [] = Brain.GoalStack.active_goals()

    assert_receive {:autonomy_transition, _, %{trace_id: ^trace_id, step: :salience_detected}}
    assert_receive {:autonomy_transition, _, %{trace_id: ^trace_id, step: :goal_created}}
    assert_receive {:autonomy_transition, _, %{trace_id: ^trace_id, step: :action_selected}}
    assert_receive {:autonomy_transition, _, %{trace_id: ^trace_id, step: :command_executed}}
    assert_receive {:autonomy_transition, _, %{trace_id: ^trace_id, step: :goal_completed}}
  end
end
