# apps/brain/test/brain/lifg_gating_test.exs
defmodule Brain.LIFGGatingTest do
  use ExUnit.Case, async: false

  test "lifg choices cross gate into WM with min_score" do
    si = %{
      active_cells: [
        %{token_index: 0, scores: %{"THIS/strong" => 0.01, "THIS/weak" => 0.009}}
      ],
      tokens: [%{index: 0, phrase: "this"}]
    }

    {:ok, _} = Brain.lifg_stage1(si, [0.0], gate_into_wm: true, lifg_min_score: 0.6)

    %{wm: wm} = Brain.snapshot_wm()
    assert Enum.any?(wm, &(&1.id == "THIS/strong" and &1.source == :lifg))
  end
end

