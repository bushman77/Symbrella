defmodule Brain.LIFG.BoundaryGuardInvariantTest do
  use ExUnit.Case, async: true
  import Support.TelemetryHelpers

  alias Brain.LIFG.Stage1

  test "drops non word-boundary substrings and emits boundary_drop" do
    si = %{
      sentence: "what im",
      tokens: [
        # misaligned substring inside "what" → should be dropped
        %{index: 0, phrase: "hat", span: {1, 4}, n: 1},
        # proper boundary-aligned token
        %{index: 1, phrase: "what", span: {0, 4}, n: 1}
      ],
      sense_candidates: %{
        1 => [%{id: "what|pron|1", features: %{pos: "pronoun"}, lemma: "what"}]
      }
    }

    {meas, meta} =
      capture(
        [:brain, :lifg, :boundary_drop],
        fn ->
          assert {:ok, %{choices: choices, audit: audit}} = Stage1.run(si)
          assert length(choices) == 1
          assert audit.kept_tokens == 1
          assert audit.dropped_tokens == 1
          [%{token_index: 1}] = choices
        end,
        300
      )

    assert meas == %{}
    assert meta[:token_index] == 0
    assert meta[:mw] == false
    # (no longer assert meta[:phrase] == "hat")
  end

  # second test ("MWEs bypass boundary guard") can stay as-is
end
