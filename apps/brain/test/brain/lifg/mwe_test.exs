# apps/brain/test/brain/lifg_mwe_test.exs
defmodule Brain.LIFG.MWETest do
  use ExUnit.Case, async: true

  alias Brain.LIFG.MWE

  test "heads_for_indices works with {start,end_exclusive} spans" do
    si = %{
      tokens: [
        %{index: 0, phrase: "hello", n: 1, span: {0, 5}},
        %{index: 1, phrase: "symbrella", n: 1, span: {6, 14}},
        %{index: 2, phrase: "hello symbrella", n: 2, mw: true, span: {0, 14}}
      ]
    }

    assert MWE.heads_for_indices(si, [2]) == %{2 => ["hello", "symbrella"]}
  end

  test "heads_for_indices works with {start,len} spans (disambiguated via surface length)" do
    si = %{
      tokens: [
        %{index: 0, phrase: "hello", n: 1, span: {0, 5}},
        # len-form for "symbrella" (length 8)
        %{index: 1, phrase: "symbrella", n: 1, span: {6, 8}},
        # parent uses end-excl form here; either form is supported
        %{index: 2, phrase: "hello symbrella", n: 2, mw: true, span: {0, 14}}
      ]
    }

    assert MWE.heads_for_indices(si, [2]) == %{2 => ["hello", "symbrella"]}
  end

  test "unigram backfill synthesizes an id when active_cell is missing :id (proper_noun self-name)" do
    si = %{
      tokens: [%{index: 0, phrase: "Symbrella", n: 1, mw: false}],
      active_cells: [%{norm: "symbrella", pos: "proper noun"}],
      sense_candidates: %{}
    }

    out = MWE.backfill_unigrams_from_active_cells(si, db_backfill?: false)
    [cand | _] = out.sense_candidates[0]

    assert cand.id == "symbrella|proper_noun|fallback"
    assert cand.pos == "proper_noun"
  end

  test "ensure_mwe_candidates buckets by token :index and suppresses function-word-edge MWEs" do
    si = %{
      tokens: [
        %{index: 1, n: 1, mw: false, phrase: "how", span: {0, 3}},
        %{index: 2, n: 1, mw: false, phrase: "are", span: {4, 7}},
        # list position = 2, token index = 5 (historical mis-bucketing trap)
        %{index: 5, n: 2, mw: true, phrase: "Good   Evening", span: {8, 20}},
        # function-word edge: must not emit fallback
        %{index: 6, n: 2, mw: true, phrase: "the bucket", span: {21, 31}}
      ],
      sense_candidates: %{
        1 => [%{id: "how|verb|0", score: 0.30}],
        2 => [%{id: "are|verb|0", score: 0.30}]
      },
      active_cells: []
    }

    out = MWE.ensure_mwe_candidates(si, mwe_fallback: true, db_backfill?: false)
    sc = out.sense_candidates

    assert Map.has_key?(sc, 5)
    assert Enum.any?(sc[5], fn c -> (c[:id] || c["id"]) == "good evening|phrase|fallback" end)

    # Critical: must NOT land under token index 2 ("are")
    refute Enum.any?(Map.get(sc, 2, []), fn c ->
             (c[:id] || c["id"]) == "good evening|phrase|fallback"
           end)

    # And must suppress function-word-edge fallbacks
    refute Map.has_key?(sc, 6)
  end

  test "unigram backfill uses token :index and canonicalizes pos for synthesized ids" do
    si = %{
      tokens: [
        # list position = 0, token index = 9
        %{index: 9, n: 1, mw: false, phrase: "Bradley", span: {0, 7}}
      ],
      sense_candidates: %{},
      active_cells: [
        # nil id forces synthesized id; pos forces canonicalization
        %{id: nil, norm: "bradley", word: "Bradley", pos: "proper noun", activation: 0.70}
      ]
    }

    out = MWE.backfill_unigrams_from_active_cells(si, db_backfill?: false)
    sc = out.sense_candidates

    assert Map.has_key?(sc, 9)
    refute Map.has_key?(sc, 0)

    [cand | _] = sc[9]
    assert (cand[:id] || cand["id"]) == "bradley|proper_noun|fallback"
    assert (cand[:pos] || cand["pos"]) == "proper_noun"
  end
end
