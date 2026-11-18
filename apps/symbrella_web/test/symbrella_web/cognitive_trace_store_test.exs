defmodule SymbrellaWeb.CognitiveTraceStoreTest do
  use ExUnit.Case, async: true

  alias SymbrellaWeb.CognitiveTraceStore

  defp state, do: %{traces: %{}, order: [], handler_id: nil}

  defp record(state, id, stage, status, at_ms, evidence \\ %{}) do
    CognitiveTraceStore.ingest(state, %{duration_us: 1200},
      %{trace_id: id, stage: stage, status: status, at_ms: at_ms, evidence: evidence})
  end

  test "ingests only named, allowlisted stage receipts and preserves correlated order" do
    {a, first} = record(state(), "turn-abc123", :turn, :started, 10)
    assert first.trace_id == "turn-abc123"
    assert first.status == :running

    {b, second} = record(a, "turn-abc123", :lifg_selection, :ok, 11, %{choices: 2})
    assert Enum.map(second.events, & &1.stage) == [:turn, :lifg_selection]
    assert second.events |> List.last() |> Map.fetch!(:region) == :lifg
    assert second.events |> List.last() |> Map.fetch!(:evidence) == %{choices: 2}

    {c, completed} = record(b, "turn-abc123", :turn, :ok, 13)
    assert completed.status == :completed
    assert length(c.order) == 1

    {unchanged, nil} = record(c, "turn-abc123", :invented_call, :ok, 14)
    assert unchanged == c

    {unchanged2, nil} = record(c, "invalid-turn-id", :phase_1_input, :ok, 15)
    assert unchanged2 == c
  end

  test "degraded summaries are retained without user text or arbitrary metadata" do
    extra = %{degraded: true, understood: 0, uncertain: 3,
              unresolved: 1, user_text: "secret", token: "private"}

    {_, trace} = record(state(), "turn-cafe01", :comprehension_summary, :ok, 100, extra)
    [event] = trace.events

    assert event.evidence == %{degraded: true, understood: 0, uncertain: 3, unresolved: 1}
    refute Map.has_key?(event, :user_text)
    refute Map.has_key?(event.evidence, :token)
  end

  test "observed history keeps 20 turns, 40 events each, and expires old turns" do
    first =
      Enum.reduce(1..25, state(), fn i, acc ->
        {updated, _} = record(acc, "turn-#{Integer.to_string(i, 16)}", :turn, :started, i)
        updated
      end)

    assert length(first.order) == 20
    assert "turn-19" == hd(first.order)

    second =
      Enum.reduce(1..50, first, fn i, acc ->
        {updated, _} = record(acc, "turn-19", :phase_1_input, :ok, 100 + i)
        updated
      end)

    assert length(second.traces["turn-19"].events) == 40

    {expired, _} = record(second, "turn-dead", :turn, :started, 900_000)
    assert expired.order == ["turn-dead"]
  end
end
