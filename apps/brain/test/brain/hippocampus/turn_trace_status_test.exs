defmodule Brain.Hippocampus.TurnTraceStatusTest do
  use ExUnit.Case, async: true

  alias Brain.Hippocampus.TurnTrace

  test "a complete, empty trace means no API recall was attempted" do
    assert :ok == TurnTrace.start()
    assert %{events: [], status: status} = TurnTrace.finish_with_status()
    assert status.status == "complete"
    assert status.recall_status == "not_attempted"
    assert status.recall_attempts == 0
    assert status.recall_misses == 0
  end

  test "missing trace must not be inferred as no recall" do
    assert %{status: status, events: []} = TurnTrace.finish_with_status()
    assert status.status == "not_started"
    assert status.recall_status == "unknown"
    assert status.recall_attempts == nil
  end

  test "an explicit zero-result recall is recorded as an attempted miss" do
    TurnTrace.start()
    assert :ok == TurnTrace.recall(:recall, ["canucks"], [limit: 3], [])
    %{status: status, events: events} = TurnTrace.finish_with_status()

    assert status.status == "complete"
    assert status.recall_status == "attempted"
    assert status.recall_attempts == 1
    assert status.recall_misses == 1
    assert [%{kind: :recall_attempt, result_count: 0, hit?: false}] = events
  end

  test "legacy finish returns only events and deactivates capture" do
    TurnTrace.start()
    assert [] == TurnTrace.finish()
    refute TurnTrace.active?()
  end
end
