defmodule Brain.LIFG.MWENonPhrasePosTest do
  use ExUnit.Case, async: true

  alias Brain.LIFG.MWE

  test "a real multiword verb is not replaced by a generic phrase fallback" do
    sense = %{
      id: "turn off|verb|2",
      norm: "turn off",
      lemma: "turn off",
      pos: "verb",
      score: 0.3
    }

    si = %{
      tokens: [%{index: 0, phrase: "turn off", n: 2, mw: true, span: {0, 8}}],
      sense_candidates: %{0 => [sense]},
      active_cells: []
    }

    out = MWE.ensure_mwe_candidates(si, mwe_fallback: true, db_backfill?: false)

    assert out.sense_candidates[0] == [sense]
    refute Enum.any?(out.sense_candidates[0], fn candidate ->
             candidate.id == "turn off|phrase|fallback"
           end)
  end

  test "fallback backfill preserves a real lexical verb POS" do
    si = %{
      tokens: [%{index: 0, phrase: "turn off", n: 2, mw: true, span: {0, 8}}],
      sense_candidates: %{
        0 => [%{id: "turn off|phrase|fallback", pos: "phrase", norm: "turn off"}]
      },
      active_cells: [
        %{
          id: "turn off|verb|2",
          norm: "turn off",
          pos: "verb",
          definition: "To deactivate",
          gram_function: ["transitive"],
          activation: 0.6
        }
      ]
    }

    out = MWE.backfill_real_mwe_from_active_cells(si, db_backfill?: false)

    assert Enum.any?(out.sense_candidates[0], fn c ->
             c.id == "turn off|verb|2" and c.pos == "verb" and
               c.gram_function == ["transitive"]
           end)
  end
end
