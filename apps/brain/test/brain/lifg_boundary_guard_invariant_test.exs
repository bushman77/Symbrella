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

    si |> IO.inspect()

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

    meas|> IO.inspect(label: :meas)
    meta|>IO.inspect(label: :meta)

    assert meas == %{}
    assert meta[:token_index] == 0
    assert meta[:phrase] == "hat"
    assert meta[:mw] == false
  end

  test "MWEs bypass boundary guard (no boundary_drop emitted)" do
    si = %{
      sentence: "hello there",
      tokens: [
        # multiword token should bypass strict boundary guard
        %{index: 0, phrase: "hello there", span: {0, 11}, n: 2, mw: true}
      ],
      sense_candidates: %{
        0 => [%{id: "hello there|phrase|1", features: %{pos: "phrase"}, lemma: "hello there"}]
      }
    }

    :ok =
      refute_event(
        [:brain, :lifg, :boundary_drop],
        fn ->
          assert {:ok, %{choices: choices, audit: audit}} = Stage1.run(si)
          assert length(choices) == 1
          assert audit.kept_tokens == 1
          assert audit.dropped_tokens == 0
        end,
        200
      )
  end
end
