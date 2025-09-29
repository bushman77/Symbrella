defmodule Brain.LIFG.PrimingIntegrationTest do
  use ExUnit.Case, async: false
  alias Brain.LIFG
  alias Brain.LIFG.Priming

  setup do
    Priming.clear()
    :ok
  end

  test "priming lifts a tied candidate to the top" do
    # Single token, two candidates tied on all features; priming should break tie.
    tokens = [%{phrase: "bank", span: {0, 1}}]

    slate = %{
      0 => [
        %{id: "bank|money", prior: 0.5, features: %{}},
        %{id: "bank|river", prior: 0.5, features: %{}}
      ]
    }

    si = %{tokens: tokens, sense_candidates: slate, context_vec: [0.0]}

    # Give recent credit to "bank|river"
    :ok = Priming.bump("bank|river", now_ms: 1_000_000, delta: 1.0)

    # Turn on priming: weight > 0, small half-life to make the effect obvious
    si2 =
      LIFG.disambiguate_stage1(si,
        weights: %{lex: 0.0, sim: 0.0, rel: 0.0, prag: 0.0, act: 0.0, prime: 0.8},
        prime: [half_life_sec: 300.0, cap: 1.0, now_ms: 1_000_000, bump_winners: false]
      )

    winner = si2.sense_winners[0]
    assert winner.id == "bank|river"
  end
end
