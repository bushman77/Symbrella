defmodule Core.Neural.ReviewQueueTest do
  use ExUnit.Case, async: true

  alias Core.Neural.ReviewQueue
  alias Core.TrainingArchive
  alias Brain.Neural.TrainingBuilder

  setup do
    dir =
      Path.join(System.tmp_dir!(), "symbrella_review_queue_#{System.unique_integer([:positive])}")

    File.mkdir_p!(dir)
    on_exit(fn -> File.rm_rf(dir) end)
    %{dir: dir}
  end

  defp record(dir, session, turn_id, lemma, score \\ 0.6) do
    episode = %{
      slate: %{sentence: "Earlier we discussed #{lemma}", tokens: [%{phrase: lemma}]},
      meta: %{scope: :test},
      norms: MapSet.new([lemma])
    }

    assert :ok =
             TrainingArchive.record_turn(
               %{
                 turn_id: turn_id,
                 session_id: session,
                 user_text: "Do you remember #{lemma}?",
                 assistant_text: "I recall a possible connection.",
                 si: %{confidence: 0.7, mood: %{da: 0.4}},
                 hippocampal_events: [
                   %{
                     kind: :recall_attempt,
                     operation: :attach,
                     recall_id: "r-#{turn_id}",
                     source: :memory,
                     cues: [lemma],
                     result_count: 1,
                     hit?: true
                   },
                   %{
                     kind: :episodic_recall,
                     operation: :attach,
                     recall_id: "r-#{turn_id}",
                     rank: 1,
                     score: score,
                     episode_at_ms: 1_001,
                     episode: episode,
                     used_in_response: nil
                   }
                 ]
               },
               config: [enabled?: true, dir: dir]
             )
  end

  test "reviews existing attachment events, never inventing labels", %{dir: dir} do
    record(dir, "same-session", "t1", "orange", 0.7)
    record(dir, "same-session", "t2", "lemon", 0.3)

    assert {:ok, [a, b]} = ReviewQueue.pending(dir)
    assert a.user_text == "Do you remember orange?"
    assert a.memory_summary =~ "Earlier we discussed orange"

    refute File.exists?(Path.join(dir, "feedback.jsonl"))

    assert :ok == ReviewQueue.review(dir, a, true)
    assert :ok == ReviewQueue.review(dir, b, false)
    assert {:ok, []} == ReviewQueue.pending(dir)
    assert {:error, :not_pending} == ReviewQueue.review(dir, a, true)

    feedback =
      File.read!(Path.join(dir, "feedback.jsonl"))
      |> String.split("\n", trim: true)
      |> Enum.map(&Jason.decode!/1)

    assert Enum.map(feedback, &get_in(&1, ["label", "relevant"])) == [true, false]
    assert Enum.all?(feedback, &(&1["source"] == "human_review"))

    assert {:error, :insufficient_independent_sessions} =
             TrainingBuilder.build(dir, :memory_relevance)

    assert {:ok, smoke} = TrainingBuilder.build_smoke(dir, :memory_relevance)
    assert smoke.metadata.split_mode == :in_sample_smoke
    assert smoke.metadata.reviewed_examples == 2
    assert Nx.shape(smoke.train.x) == {2, 12}
    assert Nx.to_list(smoke.train.y) == [[1.0], [0.0]]
  end

  test "reviewed candidates from two sessions support normal grouped holdout", %{dir: dir} do
    record(dir, "session-1", "t1", "apple")
    record(dir, "session-2", "t2", "pear")

    {:ok, [a, b]} = ReviewQueue.pending(dir)
    assert :ok == ReviewQueue.review(dir, a, true)
    assert :ok == ReviewQueue.review(dir, b, false)

    assert {:ok, data} = TrainingBuilder.build(dir, :memory_relevance)
    assert data.metadata.train_examples == 1
    assert data.metadata.eval_examples == 1

    assert MapSet.disjoint?(
             MapSet.new(data.metadata.train_sessions),
             MapSet.new(data.metadata.eval_sessions)
           )
  end

  test "read-only listing skips unreviewable events", %{dir: dir} do
    record(dir, "session-1", "t1", "correct")
    events_path = Path.join(dir, "memory_events.jsonl")

    File.write!(events_path, Jason.encode!(%{
      record_type: "memory_event",
      kind: "recall_attempt",
      session_id: "session-1",
      turn_id: "t1"
    }) <> "\n", [:append])

    assert {:ok, [_one]} = ReviewQueue.pending(dir)
  end
end
