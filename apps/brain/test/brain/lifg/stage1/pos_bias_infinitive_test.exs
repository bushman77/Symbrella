defmodule Brain.LIFG.Stage1.PosBiasInfinitiveTest do
  use ExUnit.Case, async: true

  alias Brain.LIFG.Stage1.PosBias

  test "recognizes infinitive followed by an object determiner" do
    tokens = [
      %{index: 0, phrase: "to"},
      %{index: 1, phrase: "get"},
      %{index: 2, phrase: "some"},
      %{index: 3, phrase: "cash"}
    ]

    ctx = %{si: %{tokens: tokens}}
    target = Enum.at(tokens, 1)

    assert PosBias.window(ctx, target, "verb") == 0.30
    assert PosBias.window(ctx, target, "noun") == -0.15
    assert PosBias.window(ctx, target, "proper_noun") == -0.15
  end

  test "does not penalize an ordinary prepositional noun" do
    tokens = [
      %{index: 0, phrase: "to"},
      %{index: 1, phrase: "the"},
      %{index: 2, phrase: "bank"}
    ]

    ctx = %{si: %{tokens: tokens}}

    assert PosBias.window(ctx, Enum.at(tokens, 2), "noun") == 0.0
  end

  test "does not assume every word after to is a verb" do
    tokens = [
      %{index: 0, phrase: "to"},
      %{index: 1, phrase: "school"}
    ]

    ctx = %{si: %{tokens: tokens}}

    assert PosBias.window(ctx, Enum.at(tokens, 1), "noun") == 0.0
  end
end
