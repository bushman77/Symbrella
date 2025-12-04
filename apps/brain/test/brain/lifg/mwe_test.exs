defmodule Brain.LIFG.MWETest do
  use ExUnit.Case, async: true

  alias Brain.LIFG.MWE

  test "heads_for_indices works with {start,end_exclusive} spans" do
    si = %{
      tokens: [
        %{phrase: "hello", n: 1, span: {0, 5}},
        %{phrase: "symbrella", n: 1, span: {6, 14}},
        %{phrase: "hello symbrella", n: 2, mw: true, span: {0, 14}}
      ]
    }

    assert MWE.heads_for_indices(si, [2]) == %{2 => ["hello", "symbrella"]}
  end

  test "heads_for_indices works with {start,len} spans" do
    si = %{
      tokens: [
        %{phrase: "hello", n: 1, span: {0, 5}},
        %{phrase: "symbrella", n: 1, span: {6, 8}},   # len-form for "symbrella"
        %{phrase: "hello symbrella", n: 2, mw: true, span: {0, 14}} # len-form when start=0 is fine
      ]
    }

    assert MWE.heads_for_indices(si, [2]) == %{2 => ["hello", "symbrella"]}
  end

  test "unigram backfill synthesizes an id when active_cell is missing :id (proper_noun self-name)" do
    si = %{
      tokens: [%{phrase: "Symbrella", n: 1, mw: false}],
      active_cells: [%{norm: "symbrella", pos: "proper noun"}]
    }

    out = MWE.backfill_unigrams_from_active_cells(si, [])
    [cand | _] = out.sense_candidates[0]

    assert cand.id == "symbrella|proper_noun|fallback"
    assert cand.pos == "proper_noun"
  end
end

