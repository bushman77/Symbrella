defmodule Brain.Cerebellum.SupervisedTrainingTest do
  use ExUnit.Case, async: false

  alias Brain.Cerebellum
  alias Brain.Cerebellum.Store
  alias Brain.Cerebellum.SupervisedTraining, as: Training

  setup do
    scope =
      "cerebellum_experiment:test_" <>
        Integer.to_string(System.unique_integer([:positive]))

    ctx = Cerebellum.context_key({:lifg_stage1, intent: :ask, mwe: false})

    on_exit(fn ->
      :ets.delete(:brain_cerebellum_models, {scope, ctx})
    end)

    {:ok, scope: scope, ctx: ctx}
  end

  test "rejects unverified training and production scopes", %{
    scope: scope,
    ctx: ctx
  } do
    sample = example(:train, "river", ctx)

    assert {:error, :unverified_example} =
             Training.train(%{sample | source: :stage1}, scope)

    assert {:error, :wrong_split} =
             Training.train(%{sample | split: :eval}, scope)

    assert {:error, :non_experimental_scope} =
             Training.train(sample, "lifg_stage1")
  end

  test "rejects labels that are not candidate IDs", %{
    scope: scope,
    ctx: ctx
  } do
    sample = example(:train, "river", ctx)
    invalid = %{sample | label_id: "missing"}

    assert {:error, :label_not_in_candidates} =
             Training.train(invalid, scope)
  end

  test "training improves a synthetic held-out comparison", %{
    scope: scope,
    ctx: ctx
  } do
    training_sample = example(:train, "river", ctx)
    evaluation_sample = example(:eval, "shore", ctx)

    assert {:ok, before} =
             Training.evaluate(evaluation_sample, scope)

    refute before.baseline_correct?
    refute before.adjusted_correct?

    assert :ok = Training.train(training_sample, scope)

    assert {:ok, after_training} =
             Training.evaluate(evaluation_sample, scope)

    refute after_training.baseline_correct?
    assert after_training.adjusted_correct?

    # Evaluation must not increment learning counters.
    [{{^scope, ^ctx}, record}] =
      :ets.lookup(:brain_cerebellum_models, {scope, ctx})

    assert record.count_seen == 1

    assert :ok = Store.flush()
  end

  defp example(split, preferred_id, ctx) do
    %{
      source: :human_review,
      reviewed_by: "test-fixture",
      split: split,
      label_id: preferred_id,
      context_key: ctx,
      candidates: [
        %{
          id: preferred_id,
          context_fit: 1.0,
          lex_fit: 1.0,
          rel_prior: 0.0,
          activation: 0.0,
          intent_bias: 0.0
        },
        %{
          id: "financial",
          context_fit: 0.0,
          lex_fit: 0.0,
          rel_prior: 0.0,
          activation: 0.0,
          intent_bias: 0.0
        }
      ],
      base_scores: %{
        preferred_id => 0.46,
        "financial" => 0.54
      }
    }
  end
end
