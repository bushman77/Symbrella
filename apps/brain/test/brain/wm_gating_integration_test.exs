defmodule Brain.WMGatingIntegrationTest do
  use ExUnit.Case, async: false

  setup do
    Process.whereis(Brain) || start_supervised!(Brain)
    Process.whereis(Brain.Hippocampus) || start_supervised!(Brain.Hippocampus)
    Brain.Hippocampus.reset()
    :ok
  end

  test "lifg choices can be gated into WM" do
    si = %{
      tokens: [%{index: 0, phrase: "this"}],
      active_cells: [
        %{
          token_index: 0,
          lemma: "this",
          chosen_id: "THIS/strong",
          scores: %{"THIS/strong" => 0.6}
        }
      ],
      trace: []
    }

    {:ok, _out} =
      GenServer.call(Brain, {:lifg_stage1, si, [], [gate_into_wm: true]}, :infinity)

    %{wm: wm} = Brain.snapshot_wm()
    assert Enum.any?(wm, &(&1.id == "THIS/strong" and &1.source == :lifg))
  end

  test "hippocampus recall can be gated into WM" do
    Brain.Hippocampus.encode(%{winners: [%{lemma: "alpha"}]}, %{})
    Brain.focus_from_recall(%{winners: [%{lemma: "alpha"}]}, limit: 1)

    %{wm: wm} = Brain.snapshot_wm()
    assert Enum.any?(wm, &(&1.source == :ltm))
  end
end
