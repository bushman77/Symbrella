defmodule Db.AgencyEventsTest do
  use ExUnit.Case, async: false

  alias Db.AgencyEvent
  alias Db.AgencyEvents

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Db)
    Ecto.Adapters.SQL.Sandbox.mode(Db, {:shared, self()})
    :ok
  end

  test "create_event/1 stores an inspectable agency event" do
    attrs = %{
      session_id: "session-a",
      action: :answer,
      input: %{text: "what are you doing?"},
      decision: %{mode: :answer, tone: :warm},
      reasons: %{policy: "response_plan"},
      self_model: %{confidence: 0.72},
      self_state: %{focus: :execute},
      outcome: %{assistant_text: "I am answering."}
    }

    assert {:ok, %AgencyEvent{} = row} = AgencyEvents.create_event(attrs)
    assert row.session_id == "session-a"
    assert row.action == "answer"
    assert row.agency_v == 1
    assert payload_get(row.input, :text) == "what are you doing?"
    assert payload_get(row.self_model, :confidence) == 0.72
  end

  test "recent/1 returns newest events for one session" do
    assert {:ok, _old} =
             AgencyEvents.create_event(%{session_id: "a", action: :clarify, outcome: %{n: 1}})

    assert {:ok, newest} =
             AgencyEvents.create_event(%{session_id: "a", action: :answer, outcome: %{n: 2}})

    assert {:ok, _other} =
             AgencyEvents.create_event(%{session_id: "b", action: :answer, outcome: %{n: 3}})

    assert [found | _] = AgencyEvents.recent(session_id: "a", limit: 5)
    assert found.id == newest.id
    assert payload_get(found.outcome, :n) == 2
  end

  test "create_event/1 rejects invalid payloads" do
    assert {:error, changeset} = AgencyEvents.create_event(nil)
    refute changeset.valid?

    assert {:error, changeset} = AgencyEvents.create_event(%{session_id: "missing-action"})
    refute changeset.valid?
  end

  defp payload_get(payload, key) do
    Map.get(payload, key, Map.get(payload, to_string(key)))
  end
end
