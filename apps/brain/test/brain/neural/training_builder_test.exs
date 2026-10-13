defmodule Brain.Neural.TrainingBuilderTest do
  use ExUnit.Case, async: true

  alias Brain.Neural.TrainingBuilder, as: Builder

  setup do
    dir =
      Path.join(
        System.tmp_dir!(),
        "symbrella_neural_builder_#{System.unique_integer([:positive])}"
      )

    File.mkdir_p!(dir)
    on_exit(fn -> File.rm_rf(dir) end)
    %{dir: dir}
  end

  defp write(dir, name, rows) do
    body = Enum.map_join(rows, "\n", &Jason.encode!/1)
    File.write!(Path.join(dir, name), if(body == "", do: "", else: body <> "\n"))
  end

  defp turn(session, turn_id) do
    %{
      schema_version: 1,
      record_type: "completed_turn",
      session_id: session,
      turn_id: turn_id,
      user_text: "Can you recall that earlier fact?",
      assistant_text: "I found something related.",
      cognition: %{
        confidence: 0.6,
        mood: %{"da" => 0.45, "5ht" => 0.52, "glu" => 0.7, "ne" => 0.3},
        lifg_choices: [%{id: "fact|noun|1"}]
      },
      training: %{review_status: "unreviewed", label: nil}
    }
  end

  defp episode(session, turn_id, fingerprint, rank \\ 1) do
    %{
      schema_version: 1,
      record_type: "memory_event",
      kind: "episodic_recall",
      session_id: session,
      turn_id: turn_id,
      episode_fingerprint: fingerprint,
      rank: rank,
      score: 0.75,
      episode: %{slate: %{winners: ["example"]}}
    }
  end

  defp feedback(session, turn_id, label) do
    %{
      schema_version: 1,
      record_type: "human_feedback",
      source: "human_review",
      reviewed_by: "developer",
      session_id: session,
      turn_id: turn_id,
      label: label
    }
  end

  defp memory_label(fingerprint, relevant) do
    %{kind: "memory_relevance", episode_fingerprint: fingerprint, relevant: relevant}
  end

  test "joins reviewed memory labels to actual episodes and produces disjoint Nx batches", %{
    dir: dir
  } do
    sessions = ["s-a", "s-b", "s-c", "s-d"]
    fingers = for digit <- ["a", "b", "c", "d"], do: String.duplicate(digit, 64)

    turns = Enum.map(sessions, &turn(&1, "turn-1"))

    events =
      Enum.zip(sessions, fingers)
      |> Enum.map(fn {session, fingerprint} ->
        episode(session, "turn-1", fingerprint)
      end)

    reviewed =
      sessions
      |> Enum.zip(fingers)
      |> Enum.with_index()
      |> Enum.map(fn {{session, fingerprint}, index} ->
        feedback(session, "turn-1", memory_label(fingerprint, rem(index, 2) == 0))
      end)

    # This is a different task. It must not become a memory relevance label.
    other = feedback("s-a", "turn-1", %{kind: "word_sense", accepted_id: "us|proper|1"})

    write(dir, "turns.jsonl", turns)
    write(dir, "memory_events.jsonl", events)
    write(dir, "feedback.jsonl", reviewed ++ [other])

    assert {:ok, result} = Builder.build(dir, :memory_relevance)
    assert result.metadata.reviewed_examples == 4
    assert result.metadata.feature_schema_version == Builder.feature_schema_version()
    assert result.metadata.feature_names == Builder.feature_names()
    assert result.metadata.label_names == [:relevant]
    assert result.metadata.train_examples + result.metadata.eval_examples == 4
    assert result.metadata.eval_examples >= 1
    assert result.metadata.train_examples >= 1

    assert Nx.shape(result.train.x) ==
             {result.metadata.train_examples, length(Builder.feature_names())}

    assert Nx.shape(result.train.y) == {result.metadata.train_examples, 1}
    assert Nx.shape(result.eval.y) == {result.metadata.eval_examples, 1}

    assert MapSet.disjoint?(
             MapSet.new(result.metadata.train_sessions),
             MapSet.new(result.metadata.eval_sessions)
           )

    targets = Nx.to_list(result.train.y) ++ Nx.to_list(result.eval.y)
    assert Enum.sort(Enum.map(targets, &hd/1)) == [0.0, 0.0, 1.0, 1.0]

    refs = result.train.sample_refs ++ result.eval.sample_refs
    assert Enum.sort(Enum.map(refs, & &1.episode_fingerprint)) == Enum.sort(fingers)
  end

  test "supports reviewed uncertainty targets without any hippocampal events", %{dir: dir} do
    write(dir, "turns.jsonl", [turn("session-1", "a"), turn("session-2", "b")])

    write(dir, "feedback.jsonl", [
      feedback("session-1", "a", %{kind: "uncertainty", value: 0.15}),
      feedback("session-2", "b", %{kind: "uncertainty", value: 0.85})
    ])

    assert {:ok, result} = Builder.build(dir, :uncertainty)
    assert result.metadata.label_names == [:uncertainty]
    assert result.metadata.train_examples == 1
    assert result.metadata.eval_examples == 1

    assert Nx.to_list(result.train.y) |> hd() |> hd() |> is_float()
    assert Nx.shape(result.train.x) == {1, length(Builder.feature_names())}
  end

  test "does not train on unreviewed turns or empty archives", %{dir: dir} do
    write(dir, "turns.jsonl", [turn("s-1", "a"), turn("s-2", "b")])
    write(dir, "feedback.jsonl", [])

    assert {:error, :no_reviewed_examples} = Builder.build(dir, :uncertainty)
    assert {:error, :no_reviewed_examples} = Builder.build(dir, :memory_relevance)
  end

  test "rejects unknown episodes and orphaned feedback", %{dir: dir} do
    fp = String.duplicate("a", 64)
    write(dir, "turns.jsonl", [turn("session-1", "a"), turn("session-2", "b")])
    write(dir, "memory_events.jsonl", [episode("session-1", "a", fp)])

    write(dir, "feedback.jsonl", [
      feedback("session-1", "a", memory_label(String.duplicate("b", 64), true))
    ])

    assert {:error, {:unknown_recalled_episode, {"session-1", "a"}, _}} =
             Builder.build(dir, :memory_relevance)

    write(dir, "feedback.jsonl", [
      feedback("missing-session", "missing-turn", %{kind: "uncertainty", value: 0.5})
    ])

    assert {:error, {:orphan_feedback, {"missing-session", "missing-turn"}}} =
             Builder.build(dir, :uncertainty)
  end

  test "rejects duplicate or unverified labels", %{dir: dir} do
    write(dir, "turns.jsonl", [turn("s-1", "a"), turn("s-2", "b")])
    a = feedback("s-1", "a", %{kind: "uncertainty", value: 0.3})
    b = feedback("s-2", "b", %{kind: "uncertainty", value: 0.9})
    write(dir, "feedback.jsonl", [a, a, b])

    assert {:error, {:duplicate_review, {:uncertainty, {"s-1", "a"}}}} =
             Builder.build(dir, :uncertainty)

    write(dir, "feedback.jsonl", [Map.put(a, :source, "runtime"), b])
    assert {:error, :unverified_feedback} = Builder.build(dir, :uncertainty)
  end

  test "rejects ambiguous group splitting and across-split episode reuse", %{dir: dir} do
    write(dir, "turns.jsonl", [turn("s-1", "a"), turn("s-1", "b")])
    write(dir, "feedback.jsonl", [
      feedback("s-1", "a", %{kind: "uncertainty", value: 0.2}),
      feedback("s-1", "b", %{kind: "uncertainty", value: 0.8})
    ])

    assert {:error, :insufficient_independent_sessions} =
             Builder.build(dir, :uncertainty)

    fp = String.duplicate("e", 64)
    write(dir, "turns.jsonl", [turn("s-1", "a"), turn("s-2", "b")])
    write(dir, "memory_events.jsonl", [
      episode("s-1", "a", fp),
      episode("s-2", "b", fp)
    ])

    write(dir, "feedback.jsonl", [
      feedback("s-1", "a", memory_label(fp, true)),
      feedback("s-2", "b", memory_label(fp, false))
    ])

    assert {:error, :cross_split_episode_leakage} =
             Builder.build(dir, :memory_relevance)
  end

  test "rejects invalid labels, unsupported schemas and malformed JSONL", %{dir: dir} do
    write(dir, "turns.jsonl", [turn("s-1", "a"), turn("s-2", "b")])

    write(dir, "feedback.jsonl", [
      feedback("s-1", "a", %{kind: "uncertainty", value: 9.0}),
      feedback("s-2", "b", %{kind: "uncertainty", value: 0.2})
    ])

    assert {:error, :invalid_uncertainty_label} = Builder.build(dir, :uncertainty)

    write(dir, "feedback.jsonl", [
      feedback("s-1", "a", %{kind: "uncertainty", value: 0.1}),
      feedback("s-2", "b", %{kind: "uncertainty", value: 0.2})
    ])
    write(dir, "turns.jsonl", [Map.put(turn("s-1", "a"), :schema_version, 999), turn("s-2", "b")])
    assert {:error, :invalid_turn_record} = Builder.build(dir, :uncertainty)

    File.write!(Path.join(dir, "feedback.jsonl"), "this is not JSON\n")
    assert {:error, {:invalid_jsonl_record, "feedback.jsonl", 1}} =
             Builder.build(dir, :uncertainty)
  end
end
