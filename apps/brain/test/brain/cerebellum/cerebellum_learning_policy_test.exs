defmodule Brain.Cerebellum.LearningPolicyTest do
  use ExUnit.Case, async: false

  alias Brain.Cerebellum
  alias Brain.Cerebellum.Store

  @table :brain_cerebellum_models

  setup do
    scope = "cerebellum_policy_test"
    ctx = :crypto.strong_rand_bytes(32)

    on_exit(fn ->
      :ets.delete(@table, {scope, ctx})
    end)

    {:ok, scope: scope, ctx: ctx}
  end

  test "prediction remains active without learning", %{scope: scope, ctx: ctx} do
    candidates = candidates()
    scores = base_scores()

    initial_weights = Store.get_weights(scope, ctx)
    initial_count = cached_record(scope, ctx).count_seen

    calibrated =
      Cerebellum.calibrate_scores(
        nil,
        scores,
        candidates,
        %{
          scope: scope,
          context_key: ctx,
          margin_tau: 0.5
        }
      )

    assert calibrated["correct"] > calibrated["wrong"]

    assert Store.get_weights(scope, ctx) == initial_weights
    assert cached_record(scope, ctx).count_seen == initial_count
  end

  test "unverified Stage1 decisions cannot update weights", %{
    scope: scope,
    ctx: ctx
  } do
    initial = Store.get_weights(scope, ctx)

    assert :ok =
             Cerebellum.learn_lifg(
               nil,
               "correct",
               candidates(),
               base_scores(),
               scope: scope,
               context_key: ctx,
               margin_tau: 1.0
             )

    assert Store.get_weights(scope, ctx) == initial
    assert cached_record(scope, ctx).count_seen == 0
  end

  test "Stage1 self-predictions are not trusted supervision", %{
    scope: scope,
    ctx: ctx
  } do
    initial = Store.get_weights(scope, ctx)

    assert :ok =
             Cerebellum.learn_lifg(
               nil,
               "correct",
               candidates(),
               base_scores(),
               scope: scope,
               context_key: ctx,
               margin_tau: 1.0,
               supervision: %{
                 target_id: "correct",
                 source: :stage1
               }
             )

    assert Store.get_weights(scope, ctx) == initial
    assert cached_record(scope, ctx).count_seen == 0
  end

  test "verified labels train toward the correct target", %{
    scope: scope,
    ctx: ctx
  } do
    initial = Store.get_weights(scope, ctx)

    # Stage1 chose the wrong candidate.
    # Independent ground truth identifies the correct one.
    assert :ok =
             Cerebellum.learn_lifg(
               nil,
               "wrong",
               candidates(),
               base_scores(),
               scope: scope,
               context_key: ctx,
               margin_tau: 1.0,
               supervision: %{
                 target_id: "correct",
                 source: :gold_label
               }
             )

    learned = Store.get_weights(scope, ctx)

    # Feature index 1 is lexical fit.
    # Learning toward "correct" should increase its weight.
    assert Enum.at(learned, 1) > Enum.at(initial, 1)

    assert cached_record(scope, ctx).count_seen == 1
    assert :ok = Store.flush()
  end

  defp candidates do
    [
      %{
        id: "correct",
        lex_fit: 1.0,
        rel_prior: 0.0,
        activation: 0.0,
        intent_bias: 0.0
      },
      %{
        id: "wrong",
        lex_fit: 0.0,
        rel_prior: 0.0,
        activation: 0.0,
        intent_bias: 0.0
      }
    ]
  end

  defp base_scores do
    %{
      "correct" => 0.5,
      "wrong" => 0.5
    }
  end

  defp cached_record(scope, ctx) do
    assert [
             {{^scope, ^ctx}, record}
           ] = :ets.lookup(@table, {scope, ctx})

    record
  end
end
