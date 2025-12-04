# apps/brain/test/brain/lifg/stage1_mwe_wiring_test.exs
defmodule Brain.LIFG.Stage1MWEWiringTest do
  use ExUnit.Case, async: true

  alias Brain.LIFG.Stage1

  test "Stage1 wires MWE fallback so MWEs get a phrase|fallback candidate" do
    si = %{
      sentence: "kick the bucket",
      tokens: [
        %{index: 0, phrase: "kick", span: {0, 4}, n: 1, mw: false},
        %{index: 1, phrase: "the", span: {5, 8}, n: 1, mw: false},
        %{index: 2, phrase: "bucket", span: {9, 15}, n: 1, mw: false},
        %{index: 3, phrase: "kick the bucket", span: {0, 15}, n: 3, mw: true}
      ],
      sense_candidates: %{},
      active_cells: []
    }

    assert {:ok, %{choices: choices}} = Stage1.run(si, mwe_fallback: true)

    assert Enum.any?(choices, fn c ->
             chosen = Map.get(c, :chosen_id) || Map.get(c, "chosen_id")
             chosen == "kick the bucket|phrase|fallback"
           end)
  end
end

