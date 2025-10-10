defmodule Brain.LIFG.ReanalysisTest do
  use ExUnit.Case, async: true
  alias Brain.LIFG.Stage1

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
