defmodule Brain.LIFG.InputTest do
  use ExUnit.Case, async: true

  alias Brain.LIFG.Input

  test "active_cells lemma-only entries synthesize a POS-tagged id" do
    cands =
      Input.lifg_candidates!(%{
        active_cells: [
          %{token_index: 0, word: "symbrella"}
        ]
      })

    assert [%{token_index: 0, id: "symbrella|proper_noun|fallback"}] = cands
  end

  test "active_cells keeps explicit id when present" do
    cands =
      Input.lifg_candidates!(%{
        active_cells: [
          %{token_index: 0, id: "symbrella|proper_noun|0", score: 0.9}
        ]
      })

    assert [%{token_index: 0, id: "symbrella|proper_noun|0", score: score}] = cands
    assert is_number(score)
  end
end

