defmodule Brain.Hippocampus.TurnTraceTest do
  use ExUnit.Case, async: false

  alias Brain.Hippocampus
  alias Brain.Hippocampus.TurnTrace

  setup do
    Process.whereis(Hippocampus) || start_supervised!(Hippocampus)
    Hippocampus.reset()
    on_exit(fn -> TurnTrace.finish() end)
    :ok
  end

  defp slate(word), do: %{winners: [%{id: "#{word}|noun|0", lemma: word}]}

  test "captures full operation order, a recall miss and a ranked hit without changing returns" do
    assert TurnTrace.start() == :ok
    assert slate("alpha") == Hippocampus.encode(slate("alpha"), %{origin: :test})

    assert [] == Hippocampus.recall(["missing"], ignore_head: :never)
    assert [%{score: score}] = Hippocampus.recall(["alpha"], ignore_head: :never)
    assert score > 0

    assert [
             %{kind: :episodic_encode, episode: %{meta: %{origin: :test}}},
             %{kind: :recall_attempt, hit?: false, result_count: 0},
             %{kind: :recall_attempt, hit?: true, result_count: 1, recall_id: attempt_id},
             %{kind: :episodic_recall, recall_id: episode_id, rank: 1, episode: %{slate: _}}
           ] = TurnTrace.finish()

    assert attempt_id == episode_id

    refute TurnTrace.active?()
    assert TurnTrace.finish() == []
    # The public API remains a transparent pass-through when tracing is off.
    assert [%{score: _}] = Hippocampus.recall(["alpha"], ignore_head: :never)
  end

  test "observes fact hits and misses, and attached episode candidates" do
    TurnTrace.start()

    # reset/0 clears the memory window, not persisted facts. Use a unique key
    # so existing user_name facts cannot turn this expected miss into a hit.
    key = "turn_trace_test_#{Base.encode16(:crypto.strong_rand_bytes(8), case: :lower)}"

    assert nil == Hippocampus.fact(key)

    Hippocampus.encode(slate("Alex"), %{
      kind: :fact,
      key: key,
      value: "Alex",
      tags: ["fact", key]
    })

    assert Hippocampus.fact(key) == "Alex"

    si = %{winners: [%{lemma: "Alex"}], evidence: %{}}
    attached = Hippocampus.attach_episodes(si, limit: 1, ignore_head: false)
    assert [%{episode: _}] = attached.evidence.episodes

    events = TurnTrace.finish()
    assert Enum.any?(events, &(&1.kind == :fact_lookup and &1.memory_key == key and
                                  &1.found? == false))
    assert Enum.any?(events, &(&1.kind == :fact_lookup and &1.memory_key == key and
                                  &1.found? == true and &1.result == "Alex"))
    assert Enum.any?(events, &(&1.kind == :recall_attempt and &1.operation == :attach))
    assert Enum.any?(events, &(&1.kind == :episodic_recall and &1.operation == :attach))
  end

  test "a trace is process-local and is not inherited by another chat worker" do
    TurnTrace.start()
    assert TurnTrace.active?()

    assert Task.async(fn -> {TurnTrace.active?(), TurnTrace.finish()} end)
           |> Task.await() == {false, []}

    assert TurnTrace.finish() == []
  end

  test "marks overflow instead of silently treating a partial trace as complete" do
    TurnTrace.start()

    Enum.each(1..520, fn _ ->
      assert :ok == TurnTrace.recall(:recall, ["missing"], [limit: 3], [])
    end)

    events = TurnTrace.finish()
    assert length(events) == 513
    assert List.last(events) == %{kind: :trace_truncated, limit: 512}
  end
end
