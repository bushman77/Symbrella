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

  test "prefers conversational function senses for first-person health disclosure tokens" do
    si = %{
      sentence: "i forgot my quetiapine and i have been able to sleep",
      tokens: [
        %{index: 0, phrase: "i", n: 1, mw: false, span: {0, 1}},
        %{index: 1, phrase: "forgot", n: 1, mw: false, span: {2, 6}},
        %{index: 2, phrase: "my", n: 1, mw: false, span: {9, 2}},
        %{index: 3, phrase: "quetiapine", n: 1, mw: false, span: {12, 10}},
        %{index: 4, phrase: "and", n: 1, mw: false, span: {23, 3}},
        %{index: 5, phrase: "i", n: 1, mw: false, span: {27, 1}},
        %{index: 6, phrase: "have", n: 1, mw: false, span: {29, 4}},
        %{index: 7, phrase: "been", n: 1, mw: false, span: {34, 4}},
        %{index: 8, phrase: "able", n: 1, mw: false, span: {39, 4}},
        %{index: 9, phrase: "to", n: 1, mw: false, span: {44, 2}},
        %{index: 10, phrase: "sleep", n: 1, mw: false, span: {47, 5}}
      ],
      sense_candidates: %{
        0 => [%{id: "i|noun|0", pos: "noun", norm: "i", activation: 0.9}],
        2 => [%{id: "my|possessive|0", pos: "possessive", norm: "my", activation: 0.9}],
        4 => [%{id: "and|noun|0", pos: "noun", norm: "and", activation: 0.9}],
        5 => [%{id: "i|noun|0", pos: "noun", norm: "i", activation: 0.9}],
        6 => [%{id: "have|noun|0", pos: "noun", norm: "have", activation: 0.9}],
        7 => [%{id: "been|verb|0", pos: "verb", norm: "been", activation: 0.9}],
        9 => [%{id: "to|noun|0", pos: "noun", norm: "to", activation: 0.9}]
      }
    }

    assert {:ok, %{choices: choices}} = Stage1.run(si, scores: :all)

    by_index = Map.new(choices, &{&1.token_index, &1})

    assert by_index[0].chosen_id == "i|pronoun|0"
    assert by_index[2].chosen_id == "my|determiner|0"
    assert by_index[4].chosen_id == "and|conjunction|0"
    assert by_index[5].chosen_id == "i|pronoun|0"
    assert by_index[6].chosen_id == "have|auxiliary|0"
    assert by_index[7].chosen_id == "been|auxiliary|0"
    assert by_index[9].chosen_id == "to|preposition|0"
  end

  test "prefers temporal, modal, negation, and medication entity senses in sleep disclosure" do
    si = %{
      sentence: "i forgot my quetiapine and now i can not sleep",
      tokens: [
        %{index: 0, phrase: "i", n: 1, mw: false, span: {0, 1}},
        %{index: 1, phrase: "forgot", n: 1, mw: false, span: {2, 6}},
        %{index: 2, phrase: "my", n: 1, mw: false, span: {9, 2}},
        %{index: 3, phrase: "quetiapine", n: 1, mw: false, span: {12, 10}},
        %{index: 4, phrase: "and", n: 1, mw: false, span: {23, 3}},
        %{index: 5, phrase: "now", n: 1, mw: false, span: {27, 3}},
        %{index: 6, phrase: "i", n: 1, mw: false, span: {31, 1}},
        %{index: 7, phrase: "can", n: 1, mw: false, span: {33, 3}},
        %{index: 8, phrase: "not", n: 1, mw: false, span: {37, 3}},
        %{index: 9, phrase: "sleep", n: 1, mw: false, span: {41, 5}}
      ],
      sense_candidates: %{
        0 => [%{id: "i|noun|0", pos: "noun", norm: "i", activation: 0.9}],
        2 => [%{id: "my|possessive|0", pos: "possessive", norm: "my", activation: 0.9}],
        4 => [%{id: "and|noun|0", pos: "noun", norm: "and", activation: 0.9}],
        5 => [%{id: "now|verb|0", pos: "verb", norm: "now", activation: 0.9}],
        6 => [%{id: "i|noun|0", pos: "noun", norm: "i", activation: 0.9}],
        7 => [%{id: "can|verb|0", pos: "verb", norm: "can", activation: 0.9}],
        8 => [%{id: "not|noun|0", pos: "noun", norm: "not", activation: 0.9}]
      }
    }

    assert {:ok, %{choices: choices}} = Stage1.run(si, scores: :all)

    by_index = Map.new(choices, &{&1.token_index, &1})

    assert by_index[3].chosen_id == "quetiapine|entity|medication"
    assert by_index[5].chosen_id == "now|adverb|0"
    assert by_index[7].chosen_id == "can|auxiliary|0"
    assert by_index[8].chosen_id == "not|particle|0"
  end
end
