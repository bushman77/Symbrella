defmodule Brain.LIFGStage2ContractTest do
  use ExUnit.Case, async: true

  alias Brain.LIFG.Stage2

  @si_minimal %{
    sentence: "test sentence",
    tokens: [%{index: 0, phrase: "test"}],
    trace: []
  }

  test "run/2 is safe and returns :skip on the scaffold" do
    si0 = @si_minimal

    assert {:skip, %{si: si1, reason: reason}} = Stage2.run(si0, [])
    assert si1 == si0
    assert reason == :not_enabled
  end

  test "run/2 accepts arbitrary options and still returns :skip" do
    si0 = @si_minimal

    opts = [
      max_interpretations: 16,
      max_alternatives_per_token: 3,
      reanalysis_margin_threshold: 0.2,
      global_weight_lex: 0.5,
      global_weight_schema: 0.4,
      global_weight_episode: 0.1,
      global_weight_intent: 0.0
    ]

    assert {:skip, %{si: si1, reason: :not_enabled}} = Stage2.run(si0, opts)
    assert si1 == si0
  end
end

