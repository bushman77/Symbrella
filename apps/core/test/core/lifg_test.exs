defmodule Core.LIFGTest do
  use ExUnit.Case, async: true
  alias Core.LIFG

  test "attach builds nodes and edges without crashing on missing keys" do
    si = %{
      sentence: "Hello there",
      tokens: [
        %{phrase: "Hello", span: {0, 1}, mw: false, pos: "interjection"},
        %{phrase: "there", span: {1, 2}, mw: false, pos: "adverb"}
      ],
      # intentionally only :db_cells to prove flexibility
      db_cells: [                                                     %{id: "hello|interjection|0", word: "Hello", norm: "hello", pos: "interjection", status: :known, activation: 2.0},
        %{id: "there|adverb|0", word: "there", norm: "there", pos: "adverb", status: :known, activation: 2.0}
      ]
    }

    out = LIFG.attach(si)
    assert is_map(out.lifg)
    assert out.intent_candidates == []
    assert out.phrase_matches == []
                                                                  lifg = out.lifg
    assert lifg.stats.token_count == 2
    assert lifg.stats.cell_count == 2
    assert lifg.stats.edges > 0

    # token adjacency exists
    assert Enum.any?(lifg.edges, fn {f, t, ty, _} -> String.starts_with?(f, "tok:") and String.starts_with?(t, "tok:") and ty == :adjacent end)
    # token -> cell evidence exists
    assert Enum.any?(lifg.edges, fn {f, t, ty, _} -> String.starts_with?(f, "tok:") and String.starts_with?(t, "cell:") and ty == :evidence end)
  end
end
