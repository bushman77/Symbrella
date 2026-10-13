defmodule Core.TrainingArchiveTest do
  use ExUnit.Case, async: true

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
end
