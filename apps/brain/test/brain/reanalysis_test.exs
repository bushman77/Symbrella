defmodule Brain.LIFG.ReanalysisTest do
  use ExUnit.Case, async: true

  test "runner-up selected when fit?/2 vetoes top" do
    si = %{
      tokens: [%{index: 0, phrase: "hello"}],
      sense_candidates: %{
        0 => [
          %{id: "top",    features: %{lex_fit: 0.9, rel_prior: 0.6, activation: 0.1, intent_bias: 0.0}},
          %{id: "runner", features: %{lex_fit: 0.8, rel_prior: 0.6, activation: 0.1, intent_bias: 0.0}}
        ]
      }
    }

    veto_top = fn cand, _si -> cand.id != "top" end
    si2 = Brain.LIFG.disambiguate_stage1(si, fit?: veto_top)
    [ev | _] = si2.trace
    [%{chosen_id: "runner"}] = ev.choices
  end
end

