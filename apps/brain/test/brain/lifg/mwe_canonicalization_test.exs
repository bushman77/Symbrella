defmodule Brain.LIFG.MWECanonicalizationTest do
  use ExUnit.Case, async: true

  alias Brain.LIFG.MWE

  test "backfilled unigram candidates canonicalize spaced POS labels" do
    si = %{
      tokens: [
        %{index: 0, phrase: "Ada", span: {0, 3}, n: 1, mw: false}
      ],
      sense_candidates: %{},
      active_cells: [
        %{
          id: nil,
          word: "Ada",
          norm: "ada",
          pos: "proper noun",
          activation: 0.7
        }
      ]
    }

    out = MWE.backfill_unigrams_from_active_cells(si, [])

    assert %{0 => [candidate]} = out.sense_candidates
    assert candidate.pos == "proper_noun"
    assert candidate.id == "ada|proper_noun|fallback"
  end
end
