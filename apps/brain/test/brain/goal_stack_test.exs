defmodule Brain.GoalStackTest do
  use ExUnit.Case, async: false

  setup do
    case Process.whereis(Brain.GoalStack) do
      nil -> start_supervised!(Brain.GoalStack)
      _pid -> :ok
    end

    :ok = Brain.GoalStack.reset()
    :ok
  end

  test "orders active goals by priority and tension" do
    assert {:ok, _} =
             Brain.GoalStack.push(%{
               id: "low",
               label: "low priority",
               priority: 0.2,
               tension: 0.9
             })

    assert {:ok, _} =
             Brain.GoalStack.push(%{
               id: "high",
               label: "high priority",
               priority: 0.8,
               tension: 0.4
             })

    assert [%{id: "high"}, %{id: "low"}] = Brain.GoalStack.active_goals(2)
  end

  test "high uncertainty creates a reduction goal and low uncertainty clears it" do
    assert :ok = Brain.GoalStack.reduce_uncertainty(%Brain.SelfModel{uncertainty: 0.82})

    assert [%{id: "self|goal|reduce_uncertainty", reason: :high_uncertainty}] =
             Brain.GoalStack.active_goals()

    assert :ok = Brain.GoalStack.reduce_uncertainty(%Brain.SelfModel{uncertainty: 0.2})
    assert [] = Brain.GoalStack.active_goals()
  end
end
