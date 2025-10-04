defmodule Core.SemanticInputSenseCandidatesTest do
  use ExUnit.Case, async: true

  alias Core.SemanticInput, as: SI

  @si %{}

  describe "emit_sense_candidates/5" do
    test "keeps near-winners within margin and applies top_k (sorted desc)" do
      scored = [
        {"a|noun|0", 1.00},
        {"b|noun|0", 0.90},
        {"c|noun|0", 0.89},
        {"e|noun|0", 0.86},
        {"d|noun|0", 0.70}
      ]

      si =
        SI.emit_sense_candidates(@si, 0, scored, "alpha",
          margin: 0.15, # keep >= 1.00 - 0.15 => 0.85
          top_k: 3
        )

      cands = SI.get_sense_candidates(si, 0)
      assert Enum.map(cands, & &1.id) == ["a|noun|0", "b|noun|0", "c|noun|0"]
      assert Enum.map(cands, & &1.score) == [1.00, 0.90, 0.89]
      assert Enum.uniq(Enum.map(cands, & &1.lemma)) == ["alpha"]
    end

    test "dedups by id (keep highest score) and merges with existing up to top_k" do
      si1 =
        SI.emit_sense_candidates(@si, 2, [
          {"b|verb|1", 0.90},
          {"c|verb|1", 0.89}
        ], "beta",
          margin: 0.25, top_k: 3
        )

      # second batch: improves c, adds d; ensure merge + dedup + resort + cap by top_k
      si2 =
        SI.emit_sense_candidates(si1, 2, [
          {"c|verb|1", 0.95}, # higher than previous
          {"d|verb|1", 0.86}
        ], "beta",
          margin: 0.25, top_k: 3
        )

      cands = SI.get_sense_candidates(si2, 2)
      assert Enum.map(cands, & &1.id) == ["c|verb|1", "b|verb|1", "d|verb|1"]
      assert Enum.map(cands, & &1.score) == [0.95, 0.90, 0.86]
      # still limited by top_k
      assert length(cands) == 3
    end

    test "min_score drops items even if within margin" do
      # max=0.80; with margin 0.30 threshold is 0.50 — but min_score = 0.60 should drop 0.55
      si =
        SI.emit_sense_candidates(@si, 1, [
          {"x|adj|0", 0.80},
          {"y|adj|0", 0.55}
        ], "xi",
          margin: 0.30,
          min_score: 0.60,
          top_k: 5
        )

      cands = SI.get_sense_candidates(si, 1)
      assert Enum.map(cands, & &1.id) == ["x|adj|0"]
      assert Enum.map(cands, & &1.score) == [0.80]
    end

test "accepts %{id, score} maps and bare ids (score defaults to 0.0)" do
  # Use a larger margin so the 0.0 bare-id candidate isn't filtered out.
  si =
    SI.emit_sense_candidates(@si, 3, [
      %{id: "mwe one|mwe|0", score: 0.25},
      "lonely|noun|0"
    ], "lemma3",
      margin: 1.0,   # <-- key change
      top_k: 5
    )

  cands = SI.get_sense_candidates(si, 3)
  assert Enum.map(cands, & &1.id) == ["mwe one|mwe|0", "lonely|noun|0"]
  assert Enum.map(cands, & &1.score) == [0.25, 0.0]
  assert Enum.all?(cands, &(&1.lemma == "lemma3"))
end

    test "merges per-index but keeps indices isolated" do
      si =
        @si
        |> SI.emit_sense_candidates(0, [{"a|n|0", 0.7}], "a0")
        |> SI.emit_sense_candidates(1, [{"b|n|0", 0.8}], "b0")

      all = SI.get_sense_candidates(si, :all)
      assert Map.keys(all) |> Enum.sort() == [0, 1]
      assert Enum.map(SI.get_sense_candidates(si, 0), & &1.id) == ["a|n|0"]
      assert Enum.map(SI.get_sense_candidates(si, 1), & &1.id) == ["b|n|0"]
    end
  end
end

