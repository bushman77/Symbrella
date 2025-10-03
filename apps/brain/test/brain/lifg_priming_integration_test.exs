defmodule Brain.LIFG.PrimingIntegrationTest do
  use ExUnit.Case, async: true

  test "when priming is disabled, ties resolve deterministically by candidate order" do
    si = %{
      tokens: [%{index: 0, phrase: "bank"}],
      sense_candidates: %{
        0 => [
          %{id: "bank|financial", features: %{lex_fit: 0.5, rel_prior: 0.5, activation: 0.0, intent_bias: 0.0}},
          %{id: "bank|river",     features: %{lex_fit: 0.5, rel_prior: 0.5, activation: 0.0, intent_bias: 0.0}}
        ]
      }
    }

    # Use disambiguate_stage1 to exercise the new path (priming weight = 0.0)
    si2 = Brain.LIFG.disambiguate_stage1(si, weights: %{lex: 0.25, sim: 0.0, rel: 0.15, prag: 0.10, act: 0.10}, scores: :top2)
    [ev | _] = si2.trace
    [%{chosen_id: winner}] = ev.choices

    # With equal scores and no priming, the first candidate wins deterministically.
    assert winner == "bank|financial"
  end
end

