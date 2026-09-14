# apps/brain/test/brain/recall_gating_test.exs
defmodule Brain.RecallGatingTest do
  use ExUnit.Case, async: false

  def handle_gate(_event, meas, meta, pid) when is_pid(pid) do
    send(pid, {:gate, meas, meta})
    :ok
  end

  setup do
    id = "recall-gating-test-#{System.unique_integer([:positive])}"
    :ok = :telemetry.attach(id, [:brain, :gate, :decision], &__MODULE__.handle_gate/4, self())
    on_exit(fn -> :telemetry.detach(id) end)
    _ = Brain.defocus(fn _ -> true end)
    :ok
  end

  test "focus_from_recall gates into WM and returns WM" do
    wm = Brain.focus_from_recall(%{winners: ["alpha"]})
    assert is_list(wm)
    assert Enum.any?(wm, &(&1.source == :ltm))

    assert_receive {:gate, _meas, %{gate: :basal_ganglia, source: :ltm, decision: decision}}

    assert decision in [:allow, :boost]
  end
end
