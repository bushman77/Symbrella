defmodule Brain.Cerebellum.StoreConcurrencyTest do
  use ExUnit.Case, async: false

  alias Brain.Cerebellum.Store
  alias Db.CerebellumModel

  test "concurrent learning updates are not lost" do
    scope = "cerebellum_concurrency_test"
    ctx = :crypto.strong_rand_bytes(32)

    on_exit(fn ->
      :ets.delete(:brain_cerebellum_models, {scope, ctx})
    end)

    initial = Store.get_weights(scope, ctx)

    results =
      1..16
      |> Task.async_stream(
        fn _ ->
          Store.learn(
            scope,
            ctx,
            [0.0, 0.0, 0.01, 0.0, 0.0],
            0.25
          )
        end,
        max_concurrency: 4,
        timeout: 10_000
      )
      |> Enum.to_list()

    assert length(results) == 16
    assert Enum.all?(results, &(&1 == {:ok, :ok}))

    assert :ok = Store.flush()

    saved =
      Db.get_by!(CerebellumModel,
        scope: scope,
        context_key: ctx
      )

    assert saved.count_seen == 16

    assert_in_delta(
      Enum.at(saved.weights, 2),
      Enum.at(initial, 2) + 0.16,
      0.000001
    )

    assert Store.get_weights(scope, ctx) == saved.weights
  end
end
