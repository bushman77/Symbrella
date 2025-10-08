# apps/brain/test/brain/lifg_gating_test.exs
defmodule Brain.LIFGGatingTest do
  use ExUnit.Case, async: false

  test "lifg choices cross gate into WM with min_score" do
    # Runtime expects si.sense_candidates as a map: token_index => [candidates...]
    si = %{
      tokens: [%{index: 0, phrase: "this"}],
      sense_candidates: %{
        0 => [
          # Make "strong" clearly top-1 under default weights so p_top1 > 0.6 after softmax
          %{id: "THIS/strong", features: %{lex_fit: 1.0, rel_prior: 1.0, activation: 1.0, intent_bias: 1.0}},
          %{id: "THIS/weak",   features: %{lex_fit: 0.0, rel_prior: 0.0, activation: 0.0, intent_bias: 0.0}}
        ]
      }
    }

    # Keep the same call site; pass normalize: :softmax so the gate’s min_score uses probabilities
    {:ok, _} =
      Brain.lifg_stage1(
        si,
        [0.0],                          # ctx arg (ignored by Stage1, kept for arity compatibility)
        gate_into_wm: true,
        lifg_min_score: 0.6,
        normalize: :softmax,
        scores: :all                    # ensure score_norm is present in features for gating
      )

    %{wm: wm} = Brain.snapshot_wm()
    assert Enum.any?(wm, &(&1.id == "THIS/strong" and &1.source == :lifg))
  end
end

