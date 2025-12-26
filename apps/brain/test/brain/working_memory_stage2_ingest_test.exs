# apps/brain/test/brain/working_memory_stage2_ingest_test.exs
defmodule Brain.WorkingMemoryStage2IngestTest do
  use ExUnit.Case, async: true

  alias Brain.WorkingMemory

  @moduledoc """
  Unit coverage for `Brain.WorkingMemory.ingest_stage2/4`.

  Focus:
  - Decision normalization (commit/allow/boost/map encodings)
  - Non-commit decisions are ignored
  - Default source is `:lifg` for Stage2 ingestion
  - Duplicate merge behavior when `merge_duplicates?: true`
  """

  @cfg %{
    capacity: 7,
    decay_ms: 30_000,
    gate_threshold: 0.4,
    merge_duplicates?: true
  }

  test "ingest_stage2 admits only committed items across supported encodings and sets source=:lifg" do
    now_ms = 1_000

    decisions = [
      {:commit, %{id: "a|noun|0", token_index: 0, lemma: "a", score: 0.9}},
      {:allow, %{id: "b|noun|0", token_index: 1, lemma: "b", score: 0.8}},
      {:boost, %{id: "c|noun|0", token_index: 2, lemma: "c", score: 0.7}},
      %{decision: :allow, id: "d|noun|0", token_index: 3, lemma: "d", score: 0.6},
      %{action: :commit, id: "e|noun|0", token_index: 4, lemma: "e", score: 0.5},

      # ignored:
      :skip,
      {:skip, %{id: "zzz"}},
      %{decision: :deny, id: "nope"},
      %{action: :ignore, id: "nope2"}
    ]

    wm = WorkingMemory.ingest_stage2([], decisions, now_ms, @cfg)

    ids = wm |> Enum.map(& &1.id) |> MapSet.new()
    assert "a|noun|0" in ids
    assert "b|noun|0" in ids
    assert "c|noun|0" in ids
    assert "d|noun|0" in ids
    assert "e|noun|0" in ids
    refute "zzz" in ids

    # All admitted entries should default to source :lifg (per ingest_stage2 contract).
    assert Enum.all?(wm, fn it -> it.source == :lifg end)

    # Sanity: normalize clamps and timestamps exist
    assert Enum.all?(wm, fn it ->
             is_number(it.score) and it.score >= 0.0 and it.score <= 1.0 and
               is_integer(it.inserted_at) and is_integer(it.last_bump)
           end)
  end

  test "ingest_stage2 merges duplicates when merge_duplicates? is true" do
    now_ms = 2_000

    existing =
      WorkingMemory.normalize(%{id: "dup|noun|0", score: 0.2}, now_ms - 100, source: :lifg)

    # New commit with higher score/activation should merge into existing
    decisions = [
      {:commit, %{id: "dup|noun|0", token_index: 0, lemma: "dup", score: 0.9, activation: 0.9}}
    ]

    wm2 = WorkingMemory.ingest_stage2([existing], decisions, now_ms, @cfg)

    assert [%{id: "dup|noun|0"} = merged | _] = wm2
    assert merged.score >= 0.9
    assert merged.activation >= 0.9
    assert merged.source == :lifg
  end
end
