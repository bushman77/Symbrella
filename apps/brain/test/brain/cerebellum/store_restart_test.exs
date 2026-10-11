defmodule Brain.Cerebellum.StoreRestartTest do
  use ExUnit.Case, async: false

  alias Brain.Cerebellum.Store
  alias Db.CerebellumModel

  @table :brain_cerebellum_models

  test "supervised restart recovers persisted weights" do
    scope = "cerebellum_restart_test"
    ctx = :crypto.strong_rand_bytes(32)

    old_pid = Process.whereis(Store)
    assert is_pid(old_pid)

    # Verify the Store is an actual supervisor child.
    assert Enum.any?(
             Supervisor.which_children(Symbrella.Supervisor),
             fn
               {Store, ^old_pid, :worker, _} -> true
               _ -> false
             end
           )

    # Create and persist a learning update.
    initial = Store.get_weights(scope, ctx)

    assert :ok =
             Store.learn(
               scope,
               ctx,
               [0.0, 0.0, 0.02, 0.0, 0.0],
               0.25
             )

    assert :ok = Store.flush()

    saved =
      Db.get_by!(CerebellumModel,
        scope: scope,
        context_key: ctx
      )

    assert saved.count_seen == 1
    refute saved.weights == initial

    # Simulate a Store process crash in the test VM.
    monitor = Process.monitor(old_pid)
    Process.exit(old_pid, :kill)

    assert_receive {:DOWN, ^monitor, :process, ^old_pid, :killed},
                   5_000

    # OTP should automatically restart the Store.
    new_pid = wait_for_restart(old_pid)

    refute new_pid == old_pid

    # The restarted Store must own a fresh ETS table.
    assert :ets.info(@table, :owner) == new_pid

    # Our context should not yet be cached.
    assert :ets.lookup(@table, {scope, ctx}) == []

    # Recover the previously persisted weights.
    recovered = Store.get_weights(scope, ctx)

    assert recovered == saved.weights

    # Verify that recovery repopulated ETS.
    assert [
             {{^scope, ^ctx}, %{weights: ^recovered}}
           ] = :ets.lookup(@table, {scope, ctx})
  end

  defp wait_for_restart(old_pid, attempts \\ 100)

  defp wait_for_restart(_old_pid, 0) do
    flunk("Cerebellum Store did not restart")
  end

  defp wait_for_restart(old_pid, attempts) do
    case Process.whereis(Store) do
      pid when is_pid(pid) and pid != old_pid ->
        pid

      _ ->
        Process.sleep(20)
        wait_for_restart(old_pid, attempts - 1)
    end
  end
end
