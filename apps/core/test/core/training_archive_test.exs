defmodule Core.TrainingArchiveTest do
  use ExUnit.Case, async: false

  alias Core.TrainingArchive

  setup do
    root =
      Path.join(
        System.tmp_dir!(),
        "symbrella_training_archive_#{System.unique_integer([:positive])}"
      )

    on_exit(fn -> File.rm_rf(root) end)
    %{dir: Path.join(root, "training")}
  end

  defp turn(attrs \\ %{}) do
    Map.merge(
      %{
        turn_id: "turn-test-1",
        session_id: "session-test",
        user_text: "What is my name?",
        assistant_text: "Your name is Bradley.",
        si: %{
          intent: :ask,
          confidence: 0.82,
          semantic_choices: [%{id: "name|noun|1", score: 0.7}],
          sense_candidates: %{0 => [%{id: "name|noun|1"}]},
          sentence_signatures: [%{span: {0, 2}}],
          mood: %{da: 0.51},
          evidence: %{episodes: []}
        },
        response_meta: %{}
      },
      attrs
    )
  end

  defp records(path) do
    path
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(&Jason.decode!/1)
  end

  test "config enables automatic appends without passing per-call options", %{dir: dir} do
    previous = Application.get_env(:core, TrainingArchive, [])

    # The normal application takes these settings from config/config.exs.
    # Exercise the public default-config path without any launch variables.
    on_exit(fn -> Application.put_env(:core, TrainingArchive, previous) end)

    Application.put_env(
      :core,
      TrainingArchive,
      Keyword.merge(previous, enabled?: true, dir: dir)
    )

    assert :ok == TrainingArchive.record_turn(turn())
    assert :ok ==
             TrainingArchive.record_turn(
               turn(%{turn_id: "turn-test-2", user_text: "Hello", assistant_text: "Hi"})
             )

    assert Enum.map(records(Path.join(dir, "turns.jsonl")), & &1["turn_id"]) ==
             ["turn-test-1", "turn-test-2"]

    # Disabling via app config takes effect without modifying the recorder.
    Application.put_env(
      :core,
      TrainingArchive,
      Keyword.merge(previous, enabled?: false, dir: dir)
    )

    assert :disabled == TrainingArchive.record_turn(turn(%{turn_id: "turn-test-3"}))
    assert length(records(Path.join(dir, "turns.jsonl"))) == 2
  end

  test "disabled by default; does not create a directory", %{dir: dir} do
    assert :disabled == TrainingArchive.record_turn(turn(), config: [dir: dir])
    refute File.exists?(dir)
  end

  test "rejects incomplete conversations without writing", %{dir: dir} do
    assert {:error, :missing_turn_fields} =
             TrainingArchive.record_turn(
               turn(%{assistant_text: ""}),
               config: [enabled?: true, dir: dir]
             )

    refute File.exists?(Path.join(dir, "turns.jsonl"))
  end

  test "writes one structured turn and linked episodic and fact recall events", %{dir: dir} do
    long_definition = String.duplicate("remembered_definition_", 150)

    recalled = %{
      score: 0.73,
      at: 123_456,
      episode: %{
        meta: %{kind: :fact, key: :user_name},
        slate: %{winners: [%{lemma: "Bradley", definition: long_definition}]},
        norms: MapSet.new(["bradley", "name"])
      }
    }

    si =
      turn().si
      |> put_in([:evidence, :episodes], [recalled])

    meta = %{
      action: :identity,
      response_source: :memory,
      memory_source: :hippocampus_fact,
      memory_key: :user_name
    }

    assert :ok ==
             TrainingArchive.record_turn(
               turn(%{si: si, response_meta: meta}),
               config: [enabled?: true, dir: dir]
             )

    [saved] = records(Path.join(dir, "turns.jsonl"))
    assert saved["turn_id"] == "turn-test-1"
    assert saved["user_text"] == "What is my name?"
    assert saved["cognition"]["intent"] == "ask"
    assert saved["cognition"]["sense_candidates"]["0"] == [%{"id" => "name|noun|1"}]
    assert saved["cognition"]["sentence_signatures"] == [%{"span" => [0, 2]}]
    assert saved["training"] == %{"review_status" => "unreviewed", "label" => nil}

    events = records(Path.join(dir, "memory_events.jsonl"))
    assert Enum.map(events, & &1["kind"]) == ["episodic_recall", "fact_recall"]
    assert Enum.all?(events, &(&1["turn_id"] == "turn-test-1"))

    [episode, fact] = events
    assert episode["score"] == 0.73
    assert episode["used_in_response"] == nil
    assert String.length(episode["episode_fingerprint"]) == 64
    assert episode["episode"]["slate"]["winners"] |> hd() |> Map.fetch!("definition") ==
             long_definition

    assert Enum.sort(episode["episode"]["norms"]) == ["bradley", "name"]
    assert fact["memory_key"] == "user_name"
    assert fact["used_in_response"] == true

    assert Bitwise.band(File.stat!(dir).mode, 0o777) == 0o700
    assert Bitwise.band(File.stat!(Path.join(dir, "turns.jsonl")).mode, 0o777) == 0o600
    assert Bitwise.band(File.stat!(Path.join(dir, "memory_events.jsonl")).mode, 0o777) ==
             0o600
  end

  test "appends distinct completed turns instead of overwriting history", %{dir: dir} do
    config = [enabled?: true, dir: dir]
    assert :ok == TrainingArchive.record_turn(turn(), config: config)

    assert :ok ==
             TrainingArchive.record_turn(
               turn(%{turn_id: "turn-test-2", user_text: "Hello", assistant_text: "Hi"}),
               config: config
             )

    assert Enum.map(records(Path.join(dir, "turns.jsonl")), & &1["turn_id"]) ==
             ["turn-test-1", "turn-test-2"]

    refute File.exists?(Path.join(dir, "memory_events.jsonl"))
  end

  test "stores human-reviewed corrections separately, without inventing labels", %{dir: dir} do
    config = [enabled?: true, dir: dir]
    assert :ok == TrainingArchive.record_turn(turn(), config: config)

    feedback = %{
      session_id: "session-test",
      turn_id: "turn-test-1",
      reviewed_by: "developer",
      label: %{kind: :word_sense, token_index: 2, accepted_id: "us|proper_noun|1"},
      note: "US here means United States"
    }

    assert :ok == TrainingArchive.record_feedback(feedback, config: config)

    [stored] = records(Path.join(dir, "feedback.jsonl"))
    assert stored["turn_id"] == "turn-test-1"
    assert stored["source"] == "human_review"
    assert stored["label"]["accepted_id"] == "us|proper_noun|1"

    [saved_turn] = records(Path.join(dir, "turns.jsonl"))
    assert saved_turn["training"]["review_status"] == "unreviewed"

    assert {:error, :invalid_human_feedback} ==
             TrainingArchive.record_feedback(
               Map.delete(feedback, :reviewed_by),
               config: config
             )
  end
  test "joins direct hippocampal trace misses, encodings and retrieved candidates to the turn", %{
    dir: dir
  } do
    episode = %{
      slate: %{winners: [%{id: "alpha|noun|1", lemma: "alpha"}]},
      meta: %{source: :chat},
      norms: MapSet.new(["alpha"])
    }

    events = [
      %{kind: :episodic_encode, episode: episode, episode_at_ms: 1000},
      %{
        kind: :recall_attempt, recall_id: "query-1", operation: :recall,
        source: :memory, cues: ["missing"], limit: 3,
        result_count: 0, hit?: false
      },
      %{
        kind: :recall_attempt, recall_id: "query-2", operation: :attach,
        source: :memory, cues: ["alpha"], limit: 3,
        result_count: 1, hit?: true
      },
      %{
        kind: :episodic_recall, recall_id: "query-2",
        operation: :attach, rank: 1, score: 0.81,
        episode_at_ms: 1000, episode: episode, used_in_response: nil
      },
      %{kind: :fact_lookup, memory_key: :user_name, found?: false, result: nil}
    ]

    # The final SemanticInput also contains the same attached episode.
    # It should not cause a second episodic_recall row.
    si = put_in(turn().si, [:evidence, :episodes],
      [%{score: 0.81, at: 1000, episode: episode}])

    assert :ok ==
             TrainingArchive.record_turn(
               turn(%{si: si, hippocampal_events: events}),
               config: [enabled?: true, dir: dir]
             )

    stored = records(Path.join(dir, "memory_events.jsonl"))

    assert Enum.map(stored, & &1["kind"]) == [
             "episodic_encode",
             "recall_attempt",
             "recall_attempt",
             "episodic_recall",
             "fact_lookup"
           ]

    assert Enum.all?(stored, &(&1["session_id"] == "session-test"))
    assert Enum.all?(stored, &(&1["turn_id"] == "turn-test-1"))

    assert Enum.at(stored, 1)["hit"] == false
    assert Enum.at(stored, 1)["result_count"] == 0
    assert Enum.at(stored, 2)["recall_id"] == Enum.at(stored, 3)["recall_id"]

    [write | _] = stored
    retrieved = Enum.at(stored, 3)
    assert write["episode_fingerprint"] == retrieved["episode_fingerprint"]
    assert retrieved["rank"] == 1
    assert retrieved["score"] == 0.81
    assert retrieved["used_in_response"] == nil
    assert List.wrap(retrieved["episode"]["norms"]) == ["alpha"]
  end

  test "direct trace evidence can supply reviewed memory relevance tensors", %{dir: dir} do
    config = [enabled?: true, dir: dir]

    Enum.each(1..2, fn n ->
      word = "memory-#{n}"

      episode = %{
        slate: %{winners: [%{lemma: word}]},
        meta: %{source: :test},
        norms: MapSet.new([word])
      }

      attrs =
        turn(%{
          turn_id: "turn-#{n}",
          session_id: "session-#{n}",
          user_text: "Recall #{word}",
          assistant_text: "Recall completed",
          hippocampal_events: [
            %{
              kind: :episodic_recall,
              operation: :recall,
              recall_id: "query-#{n}",
              rank: 1,
              score: 0.6,
              episode_at_ms: 1000 + n,
              episode: episode,
              used_in_response: nil
            }
          ]
        })

      assert :ok == TrainingArchive.record_turn(attrs, config: config)
    end)

    rows = records(Path.join(dir, "memory_events.jsonl"))
    assert length(rows) == 2

    Enum.each(rows, fn event ->
      assert :ok ==
               TrainingArchive.record_feedback(
                 %{
                   session_id: event["session_id"],
                   turn_id: event["turn_id"],
                   reviewed_by: "developer",
                   label: %{
                     kind: "memory_relevance",
                     episode_fingerprint: event["episode_fingerprint"],
                     relevant: true
                   }
                 },
                 config: config
               )
    end)

    assert {:ok, dataset} = Brain.Neural.TrainingBuilder.build(dir, :memory_relevance)
    assert dataset.metadata.train_examples == 1
    assert dataset.metadata.eval_examples == 1
    assert Nx.shape(dataset.train.x) == {1, 12}
    assert Nx.shape(dataset.eval.y) == {1, 1}
  end


  test "archives a memory fast-path with independent runtime snapshot and no recall attempted", %{dir: dir} do
    attrs =
      turn(%{
        si: %{intent: :unknown, confidence: 1.0},
        pipeline_status: %{route: "memory_fast_path", semantic_pipeline: "bypassed"},
        runtime_observation: %{
          source: "Brain.MoodCore.snapshot",
          captured_at: "2026-10-13T12:00:00Z",
          status: "captured",
          levels: %{da: 0.66, "5ht": 0.52, glu: 0.7, ne: 0.6},
          mood: %{exploration: 0.7, inhibition: 0.55, vigilance: 0.6, plasticity: 0.45}
        },
        hippocampal_events: [],
        hippocampal_trace: %{
          worker: %{status: "complete", recall_attempts: 0, recall_misses: 0},
          final: %{status: "complete", recall_attempts: 0, recall_misses: 0}
        }
      })

    assert :ok == TrainingArchive.record_turn(attrs, config: [enabled?: true, dir: dir])
    [saved] = records(Path.join(dir, "turns.jsonl"))
    assert saved["pipeline_status"] == %{
             "route" => "memory_fast_path", "semantic_pipeline" => "bypassed"
           }

    assert saved["runtime_observation"]["levels"]["da"] == 0.66
    assert saved["runtime_observation"]["levels"]["glu"] == 0.7
    assert saved["hippocampal_trace"]["status"] == "complete"
    assert saved["hippocampal_trace"]["recall_status"] == "not_attempted"
    assert saved["hippocampal_trace"]["recall_attempts"] == 0
    assert saved["hippocampal_trace"]["scope"] == "worker_and_final_liveview_process"

    # Recording the measurement does not pretend the semantic pipeline ran.
    refute Map.has_key?(saved["cognition"], "self_model")
    refute File.exists?(Path.join(dir, "memory_events.jsonl"))
  end

  test "unavailable worker trace never becomes a negative retrieval label", %{dir: dir} do
    attrs = turn(%{
      hippocampal_trace: %{
        worker: %{status: "not_started"},
        final: %{status: "complete", recall_attempts: 0}
      }
    })

    assert :ok == TrainingArchive.record_turn(attrs, config: [enabled?: true, dir: dir])
    [saved] = records(Path.join(dir, "turns.jsonl"))
    assert saved["hippocampal_trace"]["status"] == "incomplete"
    assert saved["hippocampal_trace"]["recall_status"] == "unknown"
    assert saved["hippocampal_trace"]["recall_attempts"] == nil
  end


end
