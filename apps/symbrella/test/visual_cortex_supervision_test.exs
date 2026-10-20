defmodule Symbrella.VisualCortexSupervisionTest do
  use ExUnit.Case, async: false

  test "cortex is supervised even without a camera configured" do
    assert is_pid(Process.whereis(Brain.Visual.Cortex))

    assert Enum.any?(Supervisor.which_children(Symbrella.Supervisor), fn
      {Brain.Visual.Cortex, pid, :worker, _modules} when is_pid(pid) -> true
      _ -> false
    end)

    # The test environment intentionally keeps the region's sensor listener
    # inert; its supervisor and diagnostic API must still be available.
    assert %{enabled?: false, count: 0} = Brain.Visual.Cortex.status()
    assert Brain.Visual.Cortex.latest() == nil
  end
end
