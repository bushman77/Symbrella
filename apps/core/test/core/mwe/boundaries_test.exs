defmodule Core.MWE.BoundariesTest do
  use ExUnit.Case, async: true

  alias Core.MWE.Boundaries

  test "rejects punctuation between words, including standalone separators" do
    for marker <- [".", "!", "?", ",", ";", ":"] do
      refute Boundaries.valid_window?([
               %{phrase: "turn" <> marker, index: 0},
               %{phrase: "off", index: 1}
             ])

      refute Boundaries.valid_window?([
               %{phrase: "turn", index: 0},
               %{phrase: marker <> "off", index: 1}
             ])
    end
  end

  test "allows phrase terminal punctuation and contractions" do
    assert Boundaries.valid_window?([
             %{phrase: "as", index: 0},
             %{phrase: "far", index: 1},
             %{phrase: "as", index: 2},
             %{phrase: "I", index: 3},
             %{phrase: "know,", index: 4}
           ])

    assert Boundaries.valid_window?([
             %{phrase: "don't", index: 0},
             %{phrase: "give", index: 1},
             %{phrase: "up!", index: 2}
           ])
  end

  test "canonical Phase 1 word indices cannot bridge source punctuation" do
    for sentence <- ["turn. off", "turn, off", "turn—off", "turn… off",
                     "turn; off", "turn! off", "turn? off", "turn\noff"] do
      si = Core.Pipeline.Input.prepare(sentence, max_wordgram_n: 3)
      assert si.tokenization.valid?
      assert Enum.map(si.tokens, & &1.index) == [0, 1]
      refute Boundaries.valid_window?(si.tokens, si), sentence
    end
  end

  test "canonical source-aligned spans admit horizontal whitespace, not line breaks" do
    for text <- ["turn off", "turn  off", "Café turn off."] do
      si = Core.Pipeline.Input.prepare(text, max_wordgram_n: 3)
      segment = Enum.take(si.tokens, -2)
      assert Boundaries.valid_window?(segment, si), text
    end

    si = Core.Pipeline.Input.prepare("turn\noff")
    refute Boundaries.valid_window?(si.tokens, si)
  end

  test "refuses index gaps and invalid inputs" do
    refute Boundaries.valid_window?([
             %{phrase: "turn", index: 0},
             %{phrase: "off", index: 2}
           ])

    refute Boundaries.valid_window?([])
    refute Boundaries.valid_window?([%{phrase: "turn"}])
    refute Boundaries.valid_window?([%{phrase: "turn"}, %{phrase: ""}])
  end
end
