defmodule Brain.LIFG.Stage1ClosedClassTest do
  use ExUnit.Case, async: true

  alias Brain.LIFG.Stage1

  test "injects a pronoun candidate so you does not resolve to the rare verb sense" do
    si = %{
      sentence: "you",
      tokens: [
        %{index: 0, phrase: "you", n: 1, mw: false, span: {0, 3}}
      ],
      sense_candidates: %{
        0 => [
          %{id: "you|noun|0", pos: "noun", norm: "you", activation: 0.5},
          %{id: "you|verb|0", pos: "verb", norm: "you", activation: 0.5}
        ]
      }
    }

    assert {:ok, %{choices: [choice]}} = Stage1.run(si, scores: :all)

    assert choice.chosen_id == "you|pronoun|0"
    assert Map.has_key?(choice.scores, "you|verb|0")
    assert choice.scores["you|pronoun|0"] > choice.scores["you|verb|0"]
    assert choice.scores["you|pronoun|0"] > choice.scores["you|noun|0"]
  end

  test "upgrades an existing pronoun candidate so you does not resolve to the rare verb sense" do
    si = %{
      sentence: "you",
      tokens: [
        %{index: 0, phrase: "you", n: 1, mw: false, span: {0, 3}}
      ],
      sense_candidates: %{
        0 => [
          %{id: "you|noun|0", pos: "noun", norm: "you", activation: 0.5},
          %{id: "you|pronoun|0", pos: "pronoun", norm: "you", activation: 0.1},
          %{id: "you|verb|0", pos: "verb", norm: "you", activation: 0.5}
        ]
      }
    }

    assert {:ok, %{choices: [choice]}} = Stage1.run(si, scores: :all)

    assert choice.chosen_id == "you|pronoun|0"
    assert Map.has_key?(choice.scores, "you|verb|0")
    assert choice.scores["you|pronoun|0"] > choice.scores["you|verb|0"]
    assert choice.scores["you|pronoun|0"] > choice.scores["you|noun|0"]
  end
end
