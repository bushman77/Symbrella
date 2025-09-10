defmodule BrainAnalyzeTest do
  use ExUnit.Case, async: true

  test "analyze updates state and returns result" do
    res = Brain.analyze("Hello brave-world 42")
    assert is_map(res)
    assert Enum.any?(res.tokens, & &1.text == "brave-world")

    snap = Brain.snapshot()
    assert snap.turn_seq >= 1
    assert map_size(snap.token_counts) > 0
  end
end

