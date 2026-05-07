# apps/brain/test/brain/lifg/stage1_mwe_wiring_test.exs
defmodule Brain.LIFG.Stage1MWEWiringTest do
  use ExUnit.Case, async: true

  alias Brain.LIFG.Stage1

  test "Stage1 wires conservative MWE fallback so content MWEs get a phrase|fallback candidate" do
    si = %{
      sentence: "really bad drugs",
      tokens: [
        %{index: 0, phrase: "really", span: {0, 6}, n: 1, mw: false},
        %{index: 1, phrase: "bad", span: {7, 10}, n: 1, mw: false},
        %{index: 2, phrase: "drugs", span: {11, 16}, n: 1, mw: false},
        %{index: 3, phrase: "really bad drugs", span: {0, 16}, n: 3, mw: true}
      ],
      sense_candidates: %{},
      active_cells: []
    }

    assert {:ok, %{choices: choices}} = Stage1.run(si, mwe_fallback: true)

    assert Enum.any?(choices, fn c ->
             chosen = Map.get(c, :chosen_id) || Map.get(c, "chosen_id")
             chosen == "really bad drugs|phrase|fallback"
           end)
  end
end
