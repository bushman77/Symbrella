defmodule Brain.LIFG.TripwireTest do
  use ExUnit.Case, async: true

  import Support.TelemetryHelpers
  alias Brain.LIFG.Stage1

  # Use a test-only event so we don't collide with other tests
  @event [:test, :lifg, :chargram_violation_tripwire]

  test "drops misaligned char-grams and emits telemetry" do
    si = %{
      sentence: "Kick the ball",
      tokens: [
        # should drop (char-gram sitting across word boundary)
        %{index: 0, phrase: "ck t", span: {1, 5}, n: 1, mw: false},
        # should keep
        %{index: 1, phrase: "Kick", span: {0, 4}, n: 1, mw: false}
      ],
      sense_candidates: %{
        0 => ["junk"],
        1 => [%{id: "kick|verb", score: 0.8}]
      }
    }

    {meas, meta} =
      capture(
        @event,
        fn ->
          # IMPORTANT: override the chargram_event so only this test sees it
          assert {:ok, %{choices: choices, audit: audit}} =
                   Stage1.run(si, chargram_event: @event)

          # only token 1 remains
          assert Enum.map(choices, & &1.token_index) == [1]
          assert audit.dropped_tokens == 1
        end,
        300
      )

    # Tripwire invariants
    assert meas == %{}
    assert meta[:token_index] == 0
    assert meta[:phrase] == "ck t"
    assert meta[:mw] == false
    assert meta[:reason] == :chargram
    assert meta[:count] == 1
    assert meta[:v] == 2
  end
end

