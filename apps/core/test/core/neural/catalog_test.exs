defmodule Core.Neural.CatalogTest do
  use ExUnit.Case, async: true

  alias Core.Neural.Catalog
  alias Core.TrainingArchive

  setup do
    dir = Path.join(System.tmp_dir!(), "symbrella_catalog_#{System.unique_integer([:positive])}")
    File.mkdir_p!(dir)
    on_exit(fn -> File.rm_rf(dir) end)
    %{dir: dir}
  end

  test "automatically indexes turns, hippocampal events and actual human feedback", %{dir: dir} do
    episode = %{
      slate: %{tokens: [%{phrase: "canucks"}]},
      meta: %{kind: :conversation},
      norms: MapSet.new(["canucks"])
    }

    assert :ok ==
             TrainingArchive.record_turn(
               %{
                 session_id: "s-1",
                 turn_id: "t-1",
                 user_text: "Vancouver Canucks?",
                 assistant_text: "They play hockey.",
                 si: %{
                   intent: :ask,
                   confidence: 0.7,
                   mood: %{da: 0.6, glu: 0.4},
                   tokens: [%{phrase: "canucks"}],
                   semantic_choices: [%{id: "canucks|noun|1"}]
                 },
                 hippocampal_events: [
                   %{kind: :recall_attempt, operation: :attach, recall_id: "r1", cues: ["canucks"], result_count: 1, hit?: true},
                   %{kind: :episodic_recall, operation: :attach, recall_id: "r1", rank: 1, score: 0.8, episode_at_ms: 123, episode: episode}
                 ]
               },
               config: [enabled?: true, dir: dir]
             )

    [turn] = read(Path.join(dir, "turns.jsonl"))
    assert turn["cognition"]["intent"] == "ask"
    [attempt, recalled] = read(Path.join(dir, "memory_events.jsonl"))

    assert :ok ==
             TrainingArchive.record_feedback(
               %{
                 session_id: "s-1",
                 turn_id: "t-1",
                 reviewed_by: "person",
                 label: %{kind: "memory_relevance", episode_fingerprint: recalled["episode_fingerprint"], relevant: false}
               },
               config: [enabled?: true, dir: dir]
             )

    assert {:ok, catalog} = Catalog.build(dir)
    assert catalog.summary.total_records == 4
    assert catalog.summary.categories["cognitive_turn"] == 1
    assert catalog.summary.categories["memory_retrieval"] == 2
    assert catalog.summary.categories["human_feedback"] == 1

    [turn_row, attempt_row, episode_row, human_row] = catalog.rows
    assert turn_row.category == "cognitive_turn"
    assert turn_row.subtype == "ask"
    assert "affect" in turn_row.signal_groups
    assert "semantics" in turn_row.signal_groups
    assert turn_row.observed_fields["intent"] == "ask"
    assert turn_row.label_provenance == "runtime_observation"
    assert attempt_row.subtype == "recall_attempt"
    assert attempt_row.observed_fields["hit"] == true
    assert episode_row.observed_fields["episode_fingerprint"] == recalled["episode_fingerprint"]
    assert human_row.subtype == "memory_relevance"
    assert human_row.label_provenance == "human_review"
    assert human_row.observed_fields["correction"] == false

    assert {:ok, %{output: out, summary: summary}} = Catalog.write(dir)
    assert summary.total_records == 4
    assert Path.basename(out) == "catalog.jsonl"
    assert Bitwise.band(File.stat!(out).mode, 0o777) == 0o600

    serialized = File.read!(out)
    refute serialized =~ "Vancouver Canucks?"
    refute serialized =~ "They play hockey."
    assert length(String.split(serialized, "\n", trim: true)) == 4

    # Rebuilding creates the same indexed records without duplicating anything.
    assert {:ok, _} = Catalog.write(dir)
    assert read(out) |> length() == 4
    assert attempt["kind"] == "recall_attempt"
  end

  test "rejects malformed JSON and never overwrites raw archives", %{dir: dir} do
    turns = Path.join(dir, "turns.jsonl")
    File.write!(turns, "not JSON\n")

    assert {:error, {:malformed_jsonl, "turns.jsonl", 1}} = Catalog.build(dir)
    assert {:error, :output_overwrites_raw_archive} == Catalog.write(dir, turns)
    assert File.read!(turns) == "not JSON\n"
  end

  defp read(path) do
    path |> File.read!() |> String.split("\n", trim: true) |> Enum.map(&Jason.decode!/1)
  end
end
