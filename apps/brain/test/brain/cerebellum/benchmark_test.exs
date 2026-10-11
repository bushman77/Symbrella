defmodule Brain.Cerebellum.BenchmarkTest do
  use ExUnit.Case, async: false

  alias Brain.Cerebellum
  alias Brain.Cerebellum.Benchmark
  alias Brain.Cerebellum.Store
  alias Brain.Cerebellum.SupervisedTraining, as: Training

  @table :brain_cerebellum_models

  setup do
    scope =
      "cerebellum_experiment:benchmark_" <>
        Integer.to_string(System.unique_integer([:positive]))

    ctx =
      Cerebellum.context_key({:lifg_stage1, intent: :ask, mwe: false})

    on_exit(fn ->
      :ets.delete(@table, {scope, ctx})
    end)

    {:ok, scope: scope, ctx: ctx}
  end

  test "reports accuracy, corrections, and regressions", %{
    scope: scope,
    ctx: ctx
  } do
    # All examples are synthetic.
    # One is deliberately contradictory to exercise regression counting.
    evaluations = [
      example(:eval, "river-ambiguous", "river", 0.46, 0.54, ctx),
      example(:eval, "financial-case", "financial", 0.46, 0.54, ctx),
      example(:eval, "river-obvious", "river", 0.90, 0.10, ctx)
    ]

    # Before training.
    assert {:ok, before} =
             Benchmark.run(evaluations, scope)

    assert before.total == 3
    assert before.baseline_correct == 2
    assert before.adjusted_correct == 2
    assert before.corrections == 0
    assert before.regressions == 0

    # Train on a separate training record.
    training_example =
      example(:train, "training-river", "river", 0.46, 0.54, ctx)

    assert :ok = Training.train(training_example, scope)

    # Evaluate again without allowing further learning.
    assert {:ok, after_training} =
             Benchmark.run(evaluations, scope)

    assert after_training.total == 3

    assert after_training.baseline_accuracy_pct == 66.67
    assert after_training.adjusted_accuracy_pct == 66.67
    assert after_training.accuracy_change_pp == 0.0

    assert after_training.corrections == 1
    assert after_training.regressions == 1
    assert after_training.unchanged_correct == 1
    assert after_training.unchanged_wrong == 0
    assert after_training.net_corrections == 0

    # Confirm individual results.
    assert Enum.map(after_training.results, fn row ->
             {row.id, row.correction?, row.regression?}
           end) == [
             {"river-ambiguous", true, false},
             {"financial-case", false, true},
             {"river-obvious", false, false}
           ]

    # Benchmarking must not produce extra learning updates.
    assert [{{^scope, ^ctx}, record}] =
             :ets.lookup(@table, {scope, ctx})

    assert record.count_seen == 1

    assert :ok = Store.flush()
  end

  test "rejects training examples and unverified labels", %{
    scope: scope,
    ctx: ctx
  } do
    training_example =
      example(:train, "train", "river", 0.46, 0.54, ctx)

    assert {:error, {:invalid_example, 1, :wrong_split}} =
             Benchmark.run([training_example], scope)

    evaluation_example =
      example(:eval, "eval", "river", 0.46, 0.54, ctx)

    unverified =
      %{evaluation_example | source: :stage1}

    assert {:error, {:invalid_example, 2, :unverified_example}} =
             Benchmark.run([evaluation_example, unverified], scope)

    assert {:error, {:invalid_example, 1, :non_experimental_scope}} =
             Benchmark.run([evaluation_example], "lifg_stage1")
  end

  test "rejects empty or invalid datasets" do
    assert {:error, :empty_dataset} =
             Benchmark.run([], "cerebellum_experiment:test")

    assert {:error, :invalid_dataset} =
             Benchmark.run(:invalid, "cerebellum_experiment:test")
  end

  defp example(split, id, label_id, river_score, finance_score, ctx) do
    %{
      id: id,
      source: :human_review,
      reviewed_by: "benchmark-fixture",
      split: split,
      label_id: label_id,
      context_key: ctx,
      candidates: [
        %{
          id: "river",
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
        "river" => river_score,
        "financial" => finance_score
      }
    }
  end
end
