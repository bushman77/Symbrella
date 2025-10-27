defmodule Brain.LIFG.PostFinalizationTest do
  use ExUnit.Case, async: true
  alias Brain.LIFG.Stage1
  alias Brain.LIFG.Post

  # Pin tokenizer behavior for tests (words-only; no char-grams)
  setup_all do
    Application.put_env(:core, :tokenizer_mode, :words)
    Application.put_env(:core, :tokenizer_emit_chargrams, false)
    :ok
  end

  test "overlapping tri+bi MWEs collapse to the trigram in final cover" do
    si = %{
      sentence: "It is raining",
      tokens: [
        %{index: 0, phrase: "It is raining", n: 3, mw: true, span: {0, 14}},
        %{index: 1, phrase: "it is",         n: 2, mw: true, span: {0, 5}},
        %{index: 2, phrase: "is raining",    n: 2, mw: true, span: {3, 14}}
      ],
      sense_candidates: %{
        0 => [%{id: "It is raining|phrase|0", pos: "phrase", norm: "it is raining", activation: 0.9}],
        1 => [%{id: "It is|phrase|0",         pos: "phrase", norm: "it is",          activation: 0.9}],
        2 => [%{id: "is raining|phrase|0",    pos: "phrase", norm: "is raining",     activation: 0.9}]
      }
    }

    {:ok, %{choices: choices}} = Stage1.run(si, [])
    out = Post.finalize(si, choices, allow_overlaps?: false)

    assert out.flips == 0
    assert length(out.cover) == 1
    [only] = out.cover
    assert only.id == "It is raining|phrase|0"
    assert only.span == {0, 14}
  end

  test "reanalysis flips a rejected winner before final cover" do
    si = %{
      sentence: "cat sat",
      tokens: [
        %{index: 0, phrase: "cat", n: 1, mw: false, span: {0, 3}},
        %{index: 1, phrase: "sat", n: 1, mw: false, span: {4, 7}}
      ],
      sense_candidates: %{
        0 => [
          %{id: "cat|noun|0", pos: "noun", norm: "cat", activation: 0.90},
          %{id: "cat|verb|0", pos: "verb", norm: "cat", activation: 0.80}
        ],
        1 => [
          %{id: "sat|verb|0", pos: "verb", norm: "sat", activation: 0.85}
        ]
      }
    }

    {:ok, %{choices: choices}} = Stage1.run(si, [])
    chosen0 = Enum.find(choices, &(&1.token_index == 0)).chosen_id

    # Fail function rejects whatever Stage1 picked for token 0, forcing a flip.
    fail_fun = fn
      %{token_index: 0, chosen_id: id} -> id == chosen0
      _ -> false
    end

    out = Post.finalize(si, choices, reanalysis?: true, fail_fun: fail_fun, allow_overlaps?: false)

    assert out.flips == 1
    ch0b = Enum.find(out.choices, &(&1.token_index == 0))
    assert ch0b.chosen_id != chosen0
    assert Enum.member?(ch0b.alt_ids, chosen0)

    # Non-overlapping cover should include both single-word spans
    assert Enum.sort(Enum.map(out.cover, & &1.span)) == Enum.sort([{0, 3}, {4, 7}])
  end
end

