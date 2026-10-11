defmodule Brain.Cerebellum.StorePersistenceTest do
  use ExUnit.Case, async: false

  alias Brain.Cerebellum.Store
  alias Db.CerebellumModel

  test "learned weights are persisted to PostgreSQL" do
    # Unique test context; never touch live model keys.
    scope = "cerebellum_test"
    ctx = :crypto.strong_rand_bytes(32)

    on_exit(fn ->
      :ets.delete(:brain_cerebellum_models, {scope, ctx})
    end)

    assert Db.get_by(CerebellumModel,
             scope: scope,
             context_key: ctx
           ) == nil

    # Load initial weights.
    initial = Store.get_weights(scope, ctx)
    assert length(initial) == 5

    # Apply a small learning update.
    gradient = [0.0, 0.0, 0.02, 0.0, 0.0]

    assert :ok =
             Store.learn(scope, ctx, gradient, 0.25)

    # Persist pending updates.
    assert :ok = Store.flush()

    saved =
      Db.get_by!(CerebellumModel,
        scope: scope,
        context_key: ctx
      )

    assert saved.count_seen == 1

    assert_in_delta(
      Enum.at(saved.weights, 2),
      Enum.at(initial, 2) + 0.02,
      0.000001
    )

    assert Store.stats().pending == 0

    # Simulate losing the cached weights.
    # The supervised Store process remains running.
    :ets.delete(:brain_cerebellum_models, {scope, ctx})

    # The next lookup must recover the persisted weights.
    reloaded = Store.get_weights(scope, ctx)

    assert reloaded == saved.weights
    assert Store.stats().pending == 0
  end
end
