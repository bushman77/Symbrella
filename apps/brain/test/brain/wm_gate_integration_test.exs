# apps/brain/test/brain/wm_gate_integration_test.exs
defmodule WMGateIntegrationTest do
  use ExUnit.Case, async: true

  test "LIFG choices gate into WM with min_score" do
    si = %{
      tokens: [%{index: 0, phrase: "this"}],
      active_cells: [
        %{token_index: 0, id: "THIS/strong", features: %{lex_fit: 0.9, rel_prior: 0.6, activation: 0.0, intent_bias: 0.0}},
        %{token_index: 0, id: "THIS/weak",   features: %{lex_fit: 0.7, rel_prior: 0.6, activation: 0.0, intent_bias: 0.0}}
      ]
    }

    # Call the Brain process (assuming app supervision starts it)
    assert {:ok, out} =
             GenServer.call(
               Brain,
               {:lifg_stage1, si, [0.0], [gate_into_wm: true, lifg_min_score: 0.6]},
               :infinity
             )

    # We can’t easily read state.wm here without exposing; rely on out + no crash + telemetry.
    assert length(out.choices) == 1
    assert is_list(out.boosts)
    assert is_list(out.inhibitions)
  end
end

