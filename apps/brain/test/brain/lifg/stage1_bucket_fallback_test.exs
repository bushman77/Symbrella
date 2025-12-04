defmodule Brain.LIFG.Stage1BucketFallbackTest do
  use ExUnit.Case, async: true

  alias Brain.LIFG.Stage1

  test "Stage1 fills missing token buckets from candidates_by_token when sense_candidates is partial" do
    si = %{
      sentence: "good evening symbrella",
      intent: :greet,
      confidence: 0.7,
      tokens: [
        %{index: 0, span: {0, 22}, n: 3, phrase: "good evening symbrella", mw: true},
        %{index: 1, span: {0, 12}, n: 2, phrase: "good evening", mw: true},
        %{index: 2, span: {0, 4}, n: 1, phrase: "good", mw: false},
        %{index: 3, span: {5, 22}, n: 2, phrase: "evening symbrella", mw: true},
        %{index: 4, span: {5, 12}, n: 1, phrase: "evening", mw: false},
        %{index: 5, span: {13, 22}, n: 1, phrase: "symbrella", mw: false}
      ],

      # Partial slate (typical of "winners-only" promotion)
      sense_candidates: %{
        0 => [%{id: "good evening symbrella|phrase|fallback", norm: "good evening symbrella", pos: "phrase", activation: 0.3}],
        1 => [%{id: "good evening|phrase|fallback", norm: "good evening", pos: "phrase", activation: 0.3}],
        3 => [%{id: "evening symbrella|phrase|fallback", norm: "evening symbrella", pos: "phrase", activation: 0.3}],
        4 => [
          %{id: "evening|noun|0", norm: "evening", pos: "noun", activation: 0.25},
          %{id: "evening|verb|0", norm: "evening", pos: "verb", activation: 0.25}
        ]
      },

      # The missing unigrams exist *somewhere else* (legacy/alternate source)
      candidates_by_token: %{
        2 => [%{id: "good|adj|0", norm: "good", pos: "adj", activation: 0.2}],
        5 => [%{id: "symbrella|noun|0", norm: "symbrella", pos: "noun", activation: 0.2}]
      }
    }

    assert {:ok, %{choices: choices}} = Stage1.run(si, scores: :all, mwe_fallback: true)

    idxs =
      choices
      |> Enum.map(& &1.token_index)
      |> Enum.sort()

    assert idxs == [0, 1, 2, 3, 4, 5]
  end
end

