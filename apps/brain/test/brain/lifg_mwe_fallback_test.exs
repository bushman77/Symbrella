# apps/brain/test/brain/lifg_mwe_fallback_test.exs
defmodule Brain.LIFG.MWEFallbackTelemetryTest do
  use ExUnit.Case, async: false

  test "emits :mwe_fallback_emitted once when MWE token lacks senses" do
    si = %{
      sentence: "Hello there",
      tokens: [
        %{index: 0, n: 2, phrase: "Hello there", mw: true, span: {0, 11}},
        %{index: 1, n: 1, lemma: "hello", span: {0, 5}},
        %{index: 2, n: 1, lemma: "there", span: {6, 11}}
      ],
      sense_candidates: %{
        1 => [%{id: "hello|interjection|6", lemma: "hello", score: 0.40}],
        2 => [%{id: "there|adverb|2", lemma: "there", score: 0.10}]
      }
    }

    handler_id = "mwe-fb-test-#{System.unique_integer([:positive])}"

    attach_ok =
      :telemetry.attach_many(
        handler_id,
        [[:brain, :pmtg, :mwe_fallback_emitted]],
        fn _event, meas, meta, pid ->
          # Filter to the specific event instance we care about to avoid cross-test noise.
          if meta[:token_index] == 0 and meta[:phrase] == "hello there" do
            send(pid, {:fb, meas, meta})
          end
        end,
        self()
      )

    assert :ok = attach_ok
    on_exit(fn -> :telemetry.detach(handler_id) end)

    assert {:ok, _} = Brain.LIFG.Stage1.run(si, mwe_fallback: true, scores: :all)

    assert_receive {:fb, %{count: 1}, %{token_index: 0, phrase: "hello there", score: _}}, 200
    refute_receive {:fb, _meas, _meta}, 50
  end
end
