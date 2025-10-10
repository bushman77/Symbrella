defmodule Brain.LIFGScoresModeTest do
  use ExUnit.Case, async: true

  setup do
    original = Application.get_env(:brain, :lifg_stage1_scores_mode)
    on_exit(fn -> Application.put_env(:brain, :lifg_stage1_scores_mode, original) end)
    :ok
  end

  test "Stage1 uses env scores mode when not overridden" do
    Application.put_env(:brain, :lifg_stage1_scores_mode, :top2)

    si = %{
      tokens: [%{index: 0, phrase: "x"}],
      sense_candidates: %{
        0 => [
          %{id: "a", features: %{lex_fit: 1.0}},
          %{id: "b", features: %{lex_fit: 0.9}},
          %{id: "c", features: %{lex_fit: 0.1}}
        ]
      }
    }

    {:ok, %{choices: [ch]}} = Brain.LIFG.Stage1.run(si)
    # should only include top2 scores in the map
    assert map_size(ch.scores) in [1, 2]
    refute Map.has_key?(ch.scores, "c")
  end

  test "Stage1 opts override env scores mode" do
    Application.put_env(:brain, :lifg_stage1_scores_mode, :none)

    si = %{
      tokens: [%{index: 0, phrase: "x"}],
      sense_candidates: %{
        0 => [
          %{id: "a", features: %{lex_fit: 1.0}},
          %{id: "b", features: %{lex_fit: 0.9}}
        ]
      }
    }

    {:ok, %{choices: [ch]}} = Brain.LIFG.Stage1.run(si, scores: :all)
    assert map_size(ch.scores) == 2
  end
end
