defmodule Brain.ATL.AttachSenseCandidatesMergeTest do
  use ExUnit.Case, async: true

  alias Brain.ATL

  test "attach_sense_candidates/3 merges into existing sense_candidates (does not delete unigrams like good)" do
    si =
      %{
        tokens: [
          %{index: 0, span: {0, 22}, n: 3, phrase: "good evening symbrella", mw: true},
          %{index: 1, span: {0, 12}, n: 2, phrase: "good evening", mw: true},
          %{index: 2, span: {0, 4}, n: 1, phrase: "good", mw: false},
          %{index: 3, span: {5, 22}, n: 2, phrase: "evening symbrella", mw: true},
          %{index: 4, span: {5, 12}, n: 1, phrase: "evening", mw: false},
          %{index: 5, span: {13, 22}, n: 1, phrase: "symbrella", mw: false}
        ],
        # Core/PMTG already prepared these; ATL must not wipe them out
        sense_candidates: %{
          2 => [%{id: "good|adj|0", score: 0.77, from: :core}],
          5 => [%{id: "symbrella|noun|0", score: 0.66, from: :core}]
        }
      }

    slate =
      %{
        winners: [
          %{
            id: "good evening|phrase|fallback",
            token_index: 1,
            lemma: "good evening",
            norm: "good evening",
            score: 1.0,
            margin: 1.0,
            raw: %{scores: %{"good evening|phrase|fallback" => 1.0}, features: %{}}
          }
        ]
      }

    si2 = ATL.attach_sense_candidates(si, slate, top_k: 3, margin_window: 0.05)

    assert Map.has_key?(si2.sense_candidates, 2)
    assert Enum.any?(si2.sense_candidates[2], &((&1.id || &1[:id]) == "good|adj|0"))

    assert Map.has_key?(si2.sense_candidates, 5)
    assert Enum.any?(si2.sense_candidates[5], &((&1.id || &1[:id]) == "symbrella|noun|0"))

    assert Map.has_key?(si2.sense_candidates, 1)
    assert Enum.any?(si2.sense_candidates[1], &((&1.id || &1[:id]) == "good evening|phrase|fallback"))
  end
end

