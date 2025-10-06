# apps/brain/test/brain/recall_gating_test.exs
defmodule Brain.RecallGatingTest do
  use ExUnit.Case, async: false

  test "focus_from_recall gates into WM and returns WM" do
    wm = Brain.focus_from_recall(%{winners: ["alpha"]})
    assert is_list(wm)
    assert Enum.any?(wm, &(&1.source == :ltm))
  end
end

