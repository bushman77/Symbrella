defmodule Brain.ControlSignalsTest do
  use ExUnit.Case, async: true

  alias Brain.ControlSignals

  test "nil opinions do not erase valid earlier controls" do
    signals =
      ControlSignals.combine([
        %{top_k: 4, branch_budget: 2, policy: :balanced},
        %{top_k: nil, branch_budget: 1, policy: nil}
      ])

    assert %ControlSignals{top_k: 4, branch_budget: 1, policy: :balanced} = signals
  end

  test "unknown and incorrectly typed fields cannot enter the canonical contract" do
    normalized =
      ControlSignals.normalize_part(%{
        top_k: "unbounded",
        branch_budget: 99,
        policy: :invented,
        utility_prior: 0.7,
        arbitrary_runtime_command: :execute
      })

    assert normalized == %{branch_budget: 99, utility_prior: 0.7}
    assert %ControlSignals{branch_budget: 5, utility_prior: 0.7,
                           policy: nil, top_k: nil} = ControlSignals.combine([normalized])
  end

  test "later valid non-nil recommendations retain deterministic precedence" do
    first = %{top_k: 3, utility_prior: 0.2}
    second = %{top_k: 6, utility_prior: nil}

    assert %ControlSignals{top_k: 6, utility_prior: 0.2} =
             ControlSignals.combine([first, second])
    assert %ControlSignals{top_k: 3, utility_prior: 0.2} =
             ControlSignals.combine([second, first])
  end

  test "a missing planner is a no-opinion input" do
    assert %ControlSignals{policy: :balanced, top_k: 2} =
             ControlSignals.combine([nil, %{policy: :balanced, top_k: 2}, %{}])
  end
end
