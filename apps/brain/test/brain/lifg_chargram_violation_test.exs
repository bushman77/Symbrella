defmodule Brain.LIFG.ChargramViolationTest do
  use ExUnit.Case, async: true
  import Support.TelemetryHelpers

  test "drops explicit char-gram tokens and emits telemetry" do
    si = %{
      sentence: "hello there",
      tokens: [
        # explicit char-gram (flagged) → should be dropped with telemetry
        %{index: 0, phrase: "he", span: {0, 2}, source: :chargram, n: 1},
        # legit, boundary-aligned token
        %{index: 1, phrase: "hello", span: {0, 5}, n: 1}
      ],
      sense_candidates: %{
        1 => [%{id: "hello|interjection|1", features: %{pos: "interjection"}, lemma: "hello"}]
      }
    }

    {meas, meta} =
      capture(
        [:brain, :lifg, :chargram_violation],
        fn ->
          {:ok, %{choices: choices, audit: audit}} = Brain.LIFG.Stage1.run(si)
          assert length(choices) == 1
          assert audit.kept_tokens == 1
          assert audit.dropped_tokens == 1
          [%{token_index: 1, chosen_id: id}] = choices
          assert String.starts_with?(id, "hello|")
        end,
        300
      )

    assert meas == %{}
    assert meta[:token_index] == 0
    assert meta[:phrase] == "he"
  end
end
