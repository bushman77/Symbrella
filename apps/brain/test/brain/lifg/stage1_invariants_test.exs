defmodule Brain.LIFG.Stage1InvariantsTest do
  use ExUnit.Case, async: true
  alias Brain.LIFG.Stage1
  alias Brain.LIFG.Cover

  test "char-grams are rejected with telemetry/audit" do
    # Token "ra" should be rejected as a char-gram (not a word boundary token).
    si = %{
      sentence: "rain",
      tokens: [
        %{index: 0, phrase: "ra", n: 1, mw: false, span: {0, 2}}
      ],
      sense_candidates: %{
        0 => [%{id: "ra|noun|0", pos: "noun", norm: "ra"}]
      }
    }

    {:ok, out} = Stage1.run(si, [])
    assert out.choices == []
    assert out.audit.chargram_violation >= 1
    assert out.audit.rejected_by_boundary == [0]
  end

  test "sentence signature: tri+bi winners; cover keeps only the trigram" do
    # Minimal synthetic SI with three MWE tokens covering overlapping spans.
    si = %{
      sentence: "It is raining",
      tokens: [
        %{index: 0, phrase: "It is raining", n: 3, mw: true, span: {0, 14}},
        %{index: 1, phrase: "it is", n: 2, mw: true, span: {0, 5}},
        %{index: 2, phrase: "is raining", n: 2, mw: true, span: {3, 14}}
      ],
      sense_candidates: %{
        0 => [%{id: "It is raining|phrase|0", pos: "phrase", norm: "it is raining"}],
        1 => [%{id: "It is|phrase|0", pos: "phrase", norm: "it is"}],
        2 => [%{id: "is raining|phrase|0", pos: "phrase", norm: "is raining"}]
      }
    }

    {:ok, %{choices: choices} = out} = Stage1.run(si, [])
    ids = Enum.map(choices, & &1.chosen_id) |> Enum.sort()
    assert ids == ["It is raining|phrase|0", "It is|phrase|0", "is raining|phrase|0"]

    # Resolve non-overlapping cover: should keep only the trigram
    %{cover: cover} = Cover.resolve_cover(si, choices, allow_overlaps?: false)
    assert length(cover) == 1
    [only] = cover
    assert only.id == "It is raining|phrase|0"
    assert only.span == {0, 14}
  end
end
