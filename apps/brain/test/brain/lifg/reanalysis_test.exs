defmodule Brain.LIFG.ReanalysisTest do
  use ExUnit.Case, async: true
  alias Brain.LIFG.Stage1
  alias Brain.LIFG.Reanalysis

  # Test-level tokenizer defaults (words-only; no char-grams)
  setup_all do
    Application.put_env(:core, :tokenizer_mode, :words)
    Application.put_env(:core, :tokenizer_emit_chargrams, false)
    :ok
  end

  @si %{
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

  test "reanalysis flips to next-best alt when failure is signaled" do
    {:ok, %{choices: choices}} = Stage1.run(@si, [])
    ch0 = Enum.find(choices, &(&1.token_index == 0))
    assert ch0.alt_ids != []

    fail_fun = fn
      %{token_index: 0, chosen_id: id} -> id == ch0.chosen_id
      _ -> false
    end

    %{choices: flipped, flips: n} = Reanalysis.fallback(choices, fail_fun)
    ch0b = Enum.find(flipped, &(&1.token_index == 0))

    assert n == 1
    assert ch0b.chosen_id != ch0.chosen_id
    assert Enum.member?(ch0b.alt_ids, ch0.chosen_id)
  end

  test "promotes runner-up when top is vetoed" do
    si = %{
      sentence: "Hello",
      tokens: [%{index: 0, phrase: "Hello", span: {0, 5}, n: 1, mw: false}],
      sense_candidates: %{
        0 => [
          %{id: "hello|interjection|top", score: 0.9, veto?: true},
          %{id: "hello|interjection|runner", score: 0.6}
        ]
      }
    }

    {:ok, %{choices: [choice]}} = Stage1.run(si, reanalysis: true)
    assert choice.chosen_id == "hello|interjection|runner"
  end

  test "keeps top when it is valid" do
    si = %{
      sentence: "Hello",
      tokens: [%{index: 0, phrase: "Hello", span: {0, 5}, n: 1, mw: false}],
      sense_candidates: %{
        0 => [
          %{id: "hello|interjection|top", score: 0.9},
          %{id: "hello|interjection|runner", score: 0.6}
        ]
      }
    }

    {:ok, %{choices: [choice]}} = Stage1.run(si, reanalysis: true)
    assert choice.chosen_id == "hello|interjection|top"
  end
end
