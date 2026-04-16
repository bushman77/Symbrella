defmodule Db.SelfSnapshotsTest do
  use ExUnit.Case, async: false

  alias Db.SelfSnapshot
  alias Db.SelfSnapshots

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Db)
    Ecto.Adapters.SQL.Sandbox.mode(Db, {:shared, self()})
    :ok
  end

  test "create_snapshot/2 stores versioned snapshot payload" do
    snapshot = %{
      v: 1,
      confidence: 0.7,
      uncertainty: 0.3,
      stability: 0.8,
      snapshot_at_ms: 123
    }

    assert {:ok, %SelfSnapshot{} = row} =
             SelfSnapshots.create_snapshot(snapshot, scope: "runtime", source: "test")

    assert row.scope == "runtime"
    assert row.source == "test"
    assert row.snapshot_v == 1
    assert row.self_model_v == 1
    assert snapshot_get(row.snapshot, :confidence) == 0.7
    assert snapshot_get(row.snapshot, :snapshot_at_ms) == 123
  end

  test "latest_snapshot/1 returns newest row for scope" do
    assert {:error, :not_found} = SelfSnapshots.latest_snapshot(scope: "runtime")

    assert {:ok, _old} =
             SelfSnapshots.create_snapshot(%{v: 1, confidence: 0.4}, scope: "runtime")

    assert {:ok, newest} =
             SelfSnapshots.create_snapshot(%{v: 1, confidence: 0.9}, scope: "runtime")

    assert {:ok, found} = SelfSnapshots.latest_snapshot(scope: "runtime")
    assert found.id == newest.id
    assert snapshot_get(found.snapshot, :confidence) == 0.9
  end

  test "latest_snapshot/1 scopes rows independently" do
    assert {:ok, _runtime} =
             SelfSnapshots.create_snapshot(%{v: 1, confidence: 0.4}, scope: "runtime")

    assert {:ok, session} =
             SelfSnapshots.create_snapshot(%{v: 1, confidence: 0.8}, scope: "session")

    assert {:ok, found} = SelfSnapshots.latest_snapshot(scope: "session")
    assert found.id == session.id
    assert snapshot_get(found.snapshot, :confidence) == 0.8
  end

  test "create_snapshot/2 rejects invalid payloads" do
    assert {:error, changeset} = SelfSnapshots.create_snapshot(nil)
    refute changeset.valid?
  end

  defp snapshot_get(snapshot, key) do
    Map.get(snapshot, key, Map.get(snapshot, to_string(key)))
  end
end
