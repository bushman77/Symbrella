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

  test "does not synthesize fallback for content plus pronoun phrase with punctuation" do
    si = %{
      sentence: "stimulate you?",
      tokens: [
        %{index: 0, n: 2, phrase: "stimulate you?", mw: true, span: {0, 14}},
        %{index: 1, n: 1, phrase: "stimulate", span: {0, 9}},
        %{index: 2, n: 1, phrase: "you?", span: {10, 14}}
      ],
      sense_candidates: %{}
    }

    assert {:ok, %{choices: choices, audit: audit}} =
             Brain.LIFG.Stage1.run(si, mwe_fallback: true, scores: :all)

    refute Enum.any?(choices, fn choice ->
             choice.chosen_id == "stimulate you|phrase|fallback" or
               choice.chosen_id == "stimulate you?|phrase|fallback"
           end)

    assert audit.mwe_fallbacks == 0
    assert audit.fallback_winners == 0
  end

  test "stage1 stop telemetry includes tokens choices and finalists" do
    si = %{
      sentence: "Hello there",
      intent: :ask,
      confidence: 0.7,
      tokens: [
        %{index: 0, n: 2, phrase: "Hello there", mw: true, span: {0, 11}},
        %{index: 1, n: 1, lemma: "hello", phrase: "hello", span: {0, 5}},
        %{index: 2, n: 1, lemma: "there", phrase: "there", span: {6, 11}}
      ],
      sense_candidates: %{
        1 => [%{id: "hello|interjection|6", lemma: "hello", score: 0.40}],
        2 => [%{id: "there|adverb|2", lemma: "there", score: 0.10}]
      }
    }

    event = [:test, :brain, :pipeline, :lifg_stage1, :stop]
    handler_id = "stage1-stop-payload-test-#{System.unique_integer([:positive])}"

    assert :ok =
             :telemetry.attach(
               handler_id,
               event,
               fn _event, meas, meta, pid -> send(pid, {:stage1_stop, meas, meta}) end,
               self()
             )

    on_exit(fn -> :telemetry.detach(handler_id) end)

    assert {:ok, _} =
             Brain.LIFG.Stage1.run(si,
               mwe_fallback: true,
               scores: :all,
               stage1_stop_event: event
             )

    assert_receive {:stage1_stop, _meas, meta}, 200
    assert [_ | _] = meta.tokens
    assert [_ | _] = meta.choices
    assert [_ | _] = meta.finalists
    assert meta.intent == :ask
    assert meta.confidence == 0.7
    assert meta.sentence == "Hello there"
  end

  test "fallback rerun emits a distinct stop event instead of duplicating normal stage1 stop" do
    si = %{
      sentence: "Really bad drugs",
      tokens: [
        %{index: 0, n: 3, phrase: "Really bad drugs", mw: true, span: {0, 16}},
        %{index: 1, n: 1, phrase: "really", lemma: "really", span: {0, 6}},
        %{index: 2, n: 1, phrase: "bad", lemma: "bad", span: {7, 10}},
        %{index: 3, n: 1, phrase: "drugs", lemma: "drugs", span: {11, 16}}
      ],
      sense_candidates: %{},
      active_cells: []
    }

    normal_event = [:test, :brain, :pipeline, :lifg_stage1, :stop]
    rerun_event = [:test, :brain, :pipeline, :lifg_stage1, :rerun_stop]
    handler_id = "stage1-rerun-stop-test-#{System.unique_integer([:positive])}"

    assert :ok =
             :telemetry.attach_many(
               handler_id,
               [normal_event, rerun_event],
               fn event, _meas, meta, pid -> send(pid, {:stage1_stop_event, event, meta}) end,
               self()
             )

    on_exit(fn -> :telemetry.detach(handler_id) end)

    assert {:ok, _} =
             Brain.LIFG.run(si,
               mwe_fallback: true,
               scores: :all,
               stage1_stop_event: normal_event,
               rerun_stage1_stop_event: rerun_event
             )

    assert_receive {:stage1_stop_event, ^normal_event, _meta}, 200
    assert_receive {:stage1_stop_event, ^rerun_event, _meta}, 200
    refute_receive {:stage1_stop_event, ^normal_event, _meta}, 50
  end
end
