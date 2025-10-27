defmodule Brain.LIFG.SenseSlateTest do
  use ExUnit.Case, async: true
  alias Brain.LIFG.Stage1

  # Test-level tokenizer defaults (words-only; no char-grams)
  setup_all do
    Application.put_env(:core, :tokenizer_mode, :words)
    Application.put_env(:core, :tokenizer_emit_chargrams, false)
    :ok
  end

  test "Stage1 reads si.sense_candidates keyed by token index" do
    si = %{
      sentence: "hello there",
      tokens: [
        %{index: 0, phrase: "hello", n: 1, mw: false, span: {0, 5}},
        %{index: 1, phrase: "there", n: 1, mw: false, span: {6, 11}}
      ],
      sense_candidates: %{
        0 => [%{id: "hello|noun|0", pos: "noun", norm: "hello", activation: 0.6}],
        1 => [%{id: "there|noun|0", pos: "noun", norm: "there", activation: 0.7}]
      }
    }

    {:ok, %{choices: choices, audit: audit}} = Stage1.run(si, [])
    assert length(choices) == 2
    assert audit.weak_decisions == 0
    assert Enum.any?(choices, &(&1.chosen_id == "hello|noun|0"))
    assert Enum.any?(choices, &(&1.chosen_id == "there|noun|0"))
  end

  test "sense_candidates takes precedence over candidates_by_token when both present" do
    si = %{
      sentence: "x y",
      tokens: [
        %{index: 0, phrase: "x", n: 1, mw: false, span: {0, 1}},
        %{index: 1, phrase: "y", n: 1, mw: false, span: {2, 3}}
      ],
      sense_candidates: %{
        0 => [%{id: "x|noun|0", pos: "noun", norm: "x", activation: 0.9}],
        1 => [%{id: "y|noun|0", pos: "noun", norm: "y", activation: 0.8}]
      },
      candidates_by_token: %{
        0 => [%{id: "x|verb|0", pos: "verb", norm: "x", activation: 0.1}],
        1 => [%{id: "y|verb|0", pos: "verb", norm: "y", activation: 0.1}]
      }
    }

    {:ok, %{choices: choices}} = Stage1.run(si, [])
    ids = Enum.map(choices, & &1.chosen_id) |> Enum.sort()
    assert ids == ["x|noun|0", "y|noun|0"]
  end
end

