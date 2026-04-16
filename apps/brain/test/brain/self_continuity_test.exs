defmodule Brain.SelfContinuityTest do
  use ExUnit.Case, async: false

  alias Brain.SelfContinuity
  alias Brain.SelfModel
  alias Db.SelfSnapshot

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Db)
    Ecto.Adapters.SQL.Sandbox.mode(Db, {:shared, self()})
    :ok
  end

  test "missing snapshot degrades explicitly" do
    assert {:degraded, %SelfModel{} = model, :missing_snapshot} =
             SelfContinuity.restore(nil)

    assert model.continuity.degraded? == true
    assert model.continuity.reboot_restored? == false
    assert model.continuity.restore_reason == :missing_snapshot
    assert model.continuity.source_snapshot_v == nil
    assert is_integer(model.continuity.restored_at_ms)
  end

  test "valid snapshot restores bounded self model" do
    snapshot =
      %SelfModel{confidence: 2.0, uncertainty: -1.0}
      |> SelfContinuity.to_snapshot()

    assert {:ok, %SelfModel{} = model} = SelfContinuity.restore(snapshot)

    assert model.confidence == 1.0
    assert model.uncertainty == 0.0
    assert model.continuity.reboot_restored? == true
    assert model.continuity.degraded? == false
    assert model.continuity.restore_reason == :restored
    assert model.continuity.source_snapshot_v == 1
  end

  test "unsupported snapshot version degrades explicitly" do
    snapshot =
      %SelfModel{}
      |> SelfContinuity.to_snapshot()
      |> Map.put(:v, 999)

    assert {:degraded, %SelfModel{} = model, :unsupported_snapshot_version} =
             SelfContinuity.restore(snapshot)

    assert model.continuity.degraded? == true
    assert model.continuity.reboot_restored? == false
    assert model.continuity.restore_reason == :unsupported_snapshot_version
    assert model.continuity.source_snapshot_v == 999
  end

  test "missing snapshot version degrades explicitly" do
    snapshot =
      %SelfModel{}
      |> SelfContinuity.to_snapshot()
      |> Map.delete(:v)

    assert {:degraded, %SelfModel{} = model, :missing_snapshot_version} =
             SelfContinuity.restore(snapshot)

    assert model.continuity.degraded? == true
    assert model.continuity.reboot_restored? == false
    assert model.continuity.restore_reason == :missing_snapshot_version
  end

  test "stale snapshot degrades explicitly" do
    snapshot =
      %SelfModel{}
      |> SelfContinuity.to_snapshot()
      |> Map.put(:snapshot_at_ms, System.system_time(:millisecond) - 10_000)

    assert {:degraded, %SelfModel{} = model, :stale_snapshot} =
             SelfContinuity.restore(snapshot, max_age_ms: 1)

    assert model.continuity.degraded? == true
    assert model.continuity.reboot_restored? == false
    assert model.continuity.restore_reason == :stale_snapshot
  end

  test "invalid snapshot degrades explicitly" do
    snapshot =
      %SelfModel{}
      |> SelfContinuity.to_snapshot()
      |> Map.put(:confidence, "not-a-number")

    assert {:degraded, %SelfModel{} = model, :invalid_snapshot} =
             SelfContinuity.restore(snapshot)

    assert model.continuity.degraded? == true
    assert model.continuity.reboot_restored? == false
    assert model.continuity.restore_reason == :invalid_snapshot
  end

  test "restore emits continuity telemetry with version metadata" do
    id = "self-continuity-test-#{System.unique_integer([:positive])}"
    parent = self()

    :ok =
      :telemetry.attach(
        id,
        [:brain, :self_model, :continuity_restored],
        fn _event, measurements, metadata, pid ->
          send(pid, {:continuity_restored, measurements, metadata})
        end,
        parent
      )

    on_exit(fn -> :telemetry.detach(id) end)

    snapshot = SelfContinuity.to_snapshot(%SelfModel{})

    assert {:ok, %SelfModel{}} = SelfContinuity.restore(snapshot)

    assert_receive {:continuity_restored, measurements, metadata}, 500

    assert measurements.count == 1
    assert measurements.restored == 1
    assert measurements.degraded == 0

    assert metadata.v == 1
    assert metadata.status == :ok
    assert metadata.reason == :restored
    assert metadata.self_model_v == 1
    assert metadata.snapshot_v == 1
    assert metadata.reboot_restored? == true
  end

  test "degraded restore emits continuity telemetry with reason" do
    id = "self-continuity-degraded-test-#{System.unique_integer([:positive])}"
    parent = self()

    :ok =
      :telemetry.attach(
        id,
        [:brain, :self_model, :continuity_restored],
        fn _event, measurements, metadata, pid ->
          send(pid, {:continuity_restored, measurements, metadata})
        end,
        parent
      )

    on_exit(fn -> :telemetry.detach(id) end)

    assert {:degraded, %SelfModel{}, :missing_snapshot} = SelfContinuity.restore(nil)

    assert_receive {:continuity_restored, measurements, metadata}, 500

    assert measurements.count == 1
    assert measurements.restored == 0
    assert measurements.degraded == 1

    assert metadata.v == 1
    assert metadata.status == :degraded
    assert metadata.reason == :missing_snapshot
    assert metadata.self_model_v == 1
    assert metadata.snapshot_v == nil
    assert metadata.reboot_restored? == false
  end

  test "persist/2 writes a Db self snapshot" do
    model = %SelfModel{confidence: 0.72, uncertainty: 0.28, stability: 0.81}

    assert {:ok, %SelfSnapshot{} = row} =
             SelfContinuity.persist(model, scope: "brain-test", source: "test")

    assert row.scope == "brain-test"
    assert row.source == "test"
    assert row.snapshot_v == 1
    assert snapshot_get(row.snapshot, :confidence) == 0.72
    assert snapshot_get(row.snapshot, :uncertainty) == 0.28
    assert snapshot_get(row.snapshot, :stability) == 0.81
  end

  test "restore_latest/1 restores latest Db snapshot through Brain policy" do
    assert {:ok, _old} =
             SelfContinuity.persist(%SelfModel{confidence: 0.2}, scope: "restore-latest-test")

    assert {:ok, _new} =
             SelfContinuity.persist(%SelfModel{confidence: 0.9}, scope: "restore-latest-test")

    assert {:ok, %SelfModel{} = model} =
             SelfContinuity.restore_latest(scope: "restore-latest-test")

    assert model.confidence == 0.9
    assert model.continuity.reboot_restored? == true
    assert model.continuity.degraded? == false
    assert model.continuity.restore_reason == :restored
  end

  test "restore_latest/1 degrades when Db has no snapshot for scope" do
    assert {:degraded, %SelfModel{} = model, :missing_snapshot} =
             SelfContinuity.restore_latest(scope: "missing-scope-test")

    assert model.continuity.reboot_restored? == false
    assert model.continuity.degraded? == true
    assert model.continuity.restore_reason == :missing_snapshot
  end

  defp snapshot_get(snapshot, key) do
    Map.get(snapshot, key, Map.get(snapshot, to_string(key)))
  end
end
