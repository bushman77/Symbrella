defmodule SymbrellaWeb.BrainRuntimeTest do
  use ExUnit.Case, async: true

  alias SymbrellaWeb.BrainRuntime

  test "runtime snapshot helpers return fallback-safe shapes" do
    assert is_map(BrainRuntime.cycle_snapshot())
    assert is_map(BrainRuntime.mood_snapshot())
    assert is_map(BrainRuntime.self_model_snapshot())
    assert is_map(BrainRuntime.self_portrait_snapshot())
  end

  test "region status returns a plain status map for unknown or unavailable regions" do
    status = BrainRuntime.region_status(:definitely_not_a_region)

    assert is_map(status)
    assert Map.get(status, :status) in [:down, nil]
  end

  test "blackboard and DB helpers tolerate unavailable services" do
    assert is_list(BrainRuntime.blackboard_history(5))
    assert BrainRuntime.brain_cells_for_choices([], []) == []
    assert BrainRuntime.brain_cells_for_choices(:bad, :input) == []
  end
end
