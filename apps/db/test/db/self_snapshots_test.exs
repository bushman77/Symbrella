defmodule Db.SelfSnapshotsTest do
  use ExUnit.Case, async: false

  alias Db.SelfSnapshot
  alias Db.SelfSnapshots

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Db)
    Ecto.Adapters.SQL.Sandbox.mode(Db, {:shared, self()})

    # Live restores can leave runtime-scoped snapshots in the database.
    # Give each test a private logical scope without deleting any rows.
    id = System.unique_integer([:positive, :monotonic])
    {:ok, scope: "snapshot-test-#{id}", session_scope: "snapshot-session-test-#{id}"}
  end

  test "create_snapshot/2 stores versioned snapshot payload", %{scope: scope} do
    snapshot = %{
      v: 1,
      confidence: 0.7,
      uncertainty: 0.3,
      stability: 0.8,
      snapshot_at_ms: 123
    }

    assert {:ok, %SelfSnapshot{} = row} =
             SelfSnapshots.create_snapshot(snapshot, scope: scope, source: "test")

    assert row.scope == scope
    assert row.source == "test"
    assert row.snapshot_v == 1
    assert row.self_model_v == 1
    assert snapshot_get(row.snapshot, :confidence) == 0.7
    assert snapshot_get(row.snapshot, :snapshot_at_ms) == 123
  end

  test "latest_snapshot/1 returns newest row for scope", %{scope: scope} do
    assert {:error, :not_found} = SelfSnapshots.latest_snapshot(scope: scope)

    assert {:ok, _old} =
             SelfSnapshots.create_snapshot(%{v: 1, confidence: 0.4}, scope: scope)

    assert {:ok, newest} =
             SelfSnapshots.create_snapshot(%{v: 1, confidence: 0.9}, scope: scope)

    assert {:ok, found} = SelfSnapshots.latest_snapshot(scope: scope)
    assert found.id == newest.id
    assert snapshot_get(found.snapshot, :confidence) == 0.9
  end

  test "latest_snapshot/1 scopes rows independently", %{scope: scope, session_scope: session_scope} do
    assert {:ok, _runtime} =
             SelfSnapshots.create_snapshot(%{v: 1, confidence: 0.4}, scope: scope)

    assert {:ok, session} =
             SelfSnapshots.create_snapshot(%{v: 1, confidence: 0.8}, scope: session_scope)

    assert {:ok, found} = SelfSnapshots.latest_snapshot(scope: session_scope)
    assert {:ok, runtime} = SelfSnapshots.latest_snapshot(scope: scope)
    assert snapshot_get(runtime.snapshot, :confidence) == 0.4
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
