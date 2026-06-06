defmodule Core.Agency.DecisionCommandExecutorTest do
  use ExUnit.Case, async: false

  alias Core.Agency.Command
  alias Core.Agency.Decision
  alias Core.Agency.Executor

  setup do
    case Process.whereis(Brain.GoalStack) do
      nil -> start_supervised!(Brain.GoalStack)
      _pid -> :ok
    end

    :ok = Brain.GoalStack.reset()
    :ok
  end

  test "builds a canonical decision and permission-gated memory command" do
    decision =
      Decision.from_action_result(
        %{
          version: "action_selector.test",
          selected: :store_memory,
          selected_candidate: %{action: :store_memory, score: 0.86, reason: :explicit_memory_write},
          candidates: [%{action: :store_memory, score: 0.86, reason: :explicit_memory_write}],
          confidence: 0.86,
          safety_gate: :approved,
          safety: %{decision: :approved, reason: :text_internal_action}
        },
        %{intent: :memory_write, session_id: "test-session"}
      )

    assert %Decision{} = decision
    assert decision.selected_action == :store_memory
    assert decision.permission.required?
    assert decision.risk == :medium
    assert is_binary(decision.trace_id)

    assert [%Command{} = command] = Command.from_decision(decision)
    assert command.type == :write_memory
    assert command.requires_permission?
    assert command.decision_trace_id == decision.trace_id
  end

  test "executor defers permission-gated memory writes by default" do
    command = %Command{
      id: "cmd-memory",
      type: :write_memory,
      requires_permission?: true,
      payload: %{text: "remember this"}
    }

    assert %{
             command_id: "cmd-memory",
             type: :write_memory,
             status: :deferred,
             reason: :permission_required
           } = Executor.execute(command, record?: false)
  end

  test "executor can set and complete safe internal goals" do
    decision = %Decision{trace_id: "trace-goal", selected_action: :self_check}

    set_goal =
      Command.new(:set_goal, decision,
        reason: :user_task,
        payload: %{
          goal: %{
            id: "goal|agency-test",
            label: "agency test goal",
            priority: 0.8,
            tension: 0.4,
            source: :test
          }
        }
      )

    assert %{status: :executed, reason: :goal_set} = Executor.execute(set_goal, record?: false)
    assert [%{id: "goal|agency-test"}] = Brain.GoalStack.active_goals()

    complete =
      Command.new(:complete_goal, decision,
        reason: :done,
        payload: %{id: "goal|agency-test", reason: :completed}
      )

    assert %{status: :executed, reason: :goal_completed} =
             Executor.execute(complete, record?: false)

    assert [] = Brain.GoalStack.active_goals()
  end

  test "executor rejects unknown commands" do
    command = %Command{id: "cmd-unknown", type: :move_body}

    assert %{status: :rejected, reason: :unknown_command} =
             Executor.execute(command, record?: false)
  end
end
