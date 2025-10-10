defmodule Brain.LIFG.MWEFallbackTelemetryTest do
  use ExUnit.Case, async: true

  test "emits :mwe_fallback_emitted once when MWE token lacks senses" do
    # Minimal SI with spans (your invariants)
    si = %{
      sentence: "Hello there",
      tokens: [
        %{index: 0, n: 2, phrase: "Hello there", mw: true, span: {0, 11}},
        %{index: 1, n: 1, lemma: "hello", span: {0, 5}},
        %{index: 2, n: 1, lemma: "there", span: {6, 11}}
      ],
      # No senses for the MWE (index 0) → fallback should be injected
      sense_candidates: %{
        1 => [%{id: "hello|interjection|6", lemma: "hello", score: 0.40}],
        2 => [%{id: "there|adverb|2", lemma: "there", score: 0.10}]
      }
    }

    # Attach a one-off telemetry handler with a unique id per run
    handler_id = "mwe-fb-test-#{System.unique_integer([:positive])}"

    attach_ok =
      :telemetry.attach_many(
        handler_id,
        [[:brain, :pmtg, :mwe_fallback_emitted]],
        fn _event, meas, meta, pid ->
          send(pid, {:fb, meas, meta})
        end,
        self()
      )

    assert :ok = attach_ok
    on_exit(fn -> :telemetry.detach(handler_id) end)

    # Run Stage-1 with fallback feature enabled
    assert {:ok, _} = Brain.LIFG.Stage1.run(si, mwe_fallback: true, scores: :all)

    # Assert that the event fired with expected shape
    assert_receive {:fb, %{count: 1}, %{token_index: 0, phrase: "Hello there", score: _}}, 100
  end
end
