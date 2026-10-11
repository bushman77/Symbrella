defmodule Brain.Cerebellum.ContextFitLearningTest do
  use ExUnit.Case, async: false

  alias Brain.Cerebellum
  alias Brain.Cerebellum.Store

  test "verified learning can distinguish senses using context_fit" do
    scope =
      "cerebellum_experiment:context_" <>
        Integer.to_string(System.unique_integer([:positive]))

    ctx = Cerebellum.context_key({:context_fit_test, scope})

    on_exit(fn ->
      :ets.delete(:brain_cerebellum_models, {scope, ctx})
    end)

    # Both candidates are identical except for context_fit.
    candidates = [
      %{
        id: "bank|noun|river",
        context_fit: 1.0,
        lex_fit: 0.5,
        rel_prior: 0.5,
        activation: 0.5,
        intent_bias: 0.5
      },
      %{
        id: "bank|noun|finance",
        context_fit: 0.0,
        lex_fit: 0.5,
        rel_prior: 0.5,
        activation: 0.5,
        intent_bias: 0.5
      }
    ]

    # Baseline incorrectly favors the financial sense.
    scores = %{
      "bank|noun|river" => 0.46,
      "bank|noun|finance" => 0.54
    }

    opts = %{
      scope: scope,
      context_key: ctx,
      acc_conflict: 1.0,
      acc_conflict_tau: 0.0
    }

    before =
      Cerebellum.calibrate_scores(
        nil,
        scores,
        candidates,
        opts
      )

    assert before["bank|noun|finance"] >
             before["bank|noun|river"]

    initial_weights = Store.get_weights(scope, ctx)

    # Independently verified target identifies the river sense.
    for _ <- 1..2 do
      assert :ok =
               Cerebellum.learn_lifg(
                 nil,
                 "bank|noun|finance",
                 candidates,
                 scores,
                 scope: scope,
                 context_key: ctx,
                 margin_tau: 1.0,
                 supervision: %{
                   source: :gold_label,
                   target_id: "bank|noun|river"
                 }
               )
    end

    learned_weights = Store.get_weights(scope, ctx)

    # The context_fit coefficient should increase.
    assert Enum.at(learned_weights, 0) >
             Enum.at(initial_weights, 0)

    after_training =
      Cerebellum.calibrate_scores(
        nil,
        scores,
        candidates,
        opts
      )

    assert after_training["bank|noun|river"] >
             after_training["bank|noun|finance"]

    assert :ok = Store.flush()
  end
end
