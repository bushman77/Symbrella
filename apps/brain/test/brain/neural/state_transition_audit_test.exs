defmodule Brain.Neural.StateTransitionAuditTest do
  use ExUnit.Case, async: true

  alias Brain.Neural.{StateTransitionAudit, StateTransitionBuilder}

  defp complete_result do
    %{
      split_mode: :session_holdout,
      evaluated?: true,
      train_sessions: ["train-1", "train-2"],
      eval_sessions: ["holdout-1", "holdout-2"],
      train_examples: 35,
      eval_examples: 24,
      valid_eval_cells: 240,
      eval_mse: 0.08,
      persistence_eval_mse: 0.1,
      eval_per_feature:
        Enum.map(StateTransitionBuilder.feature_names(), fn name ->
          %{feature: name, valid_pairs: 24}
        end)
    }
  end

  test "dataset audit reports only observed-cell counts and aggregate coverage" do
    names = StateTransitionBuilder.feature_names()
    width = length(names)

    train = %{mask: Nx.tensor([List.duplicate(1.0, width)], type: :f32)}
    evaluation = %{mask: Nx.tensor([List.duplicate(0.0, width)], type: :f32)}
    result = StateTransitionAudit.dataset(%{
      train: train,
      eval: evaluation,
      metadata: %{
        train_examples: 1, eval_examples: 1,
        train_sessions: ["train"], eval_sessions: ["holdout"],
        split_mode: :session_holdout,
        complete_modulator_turns: 1, missing_modulator_turns: 1
      }
    })

    assert result.train_transitions == 1
    assert result.eval_transitions == 1
    assert length(result.feature_coverage) == width
    assert Enum.all?(result.feature_coverage, &(&1.train_pairs == 1 and &1.eval_pairs == 0))
    assert :mood_da in result.core_features_below_minimum
    refute Map.has_key?(result, :user_text)
  end

  test "a tiny or in-sample improvement never authorizes learning integration" do
    small = %{complete_result() | eval_examples: 1}
    report = StateTransitionAudit.assess(small)
    assert report.status == :not_ready
    assert :too_few_evaluation_transitions in report.reasons
    assert report.deployment_authorized? == false

    smoke = %{complete_result() | split_mode: :in_sample_smoke, evaluated?: false}
    assert :not_independent_holdout in StateTransitionAudit.assess(smoke).reasons
  end

  test "a strong held-out improvement qualifies only for extended shadow review" do
    report = StateTransitionAudit.assess(complete_result())
    assert report.status == :candidate_for_extended_shadow
    assert report.reasons == []
    assert_in_delta report.relative_improvement, 0.2, 0.00001
    assert report.deployment_authorized? == false
  end

  test "lack of feature coverage or baseline improvement blocks promotion" do
    data = complete_result()
    no_gain = %{data | eval_mse: 0.12}
    assert :insufficient_baseline_improvement in StateTransitionAudit.assess(no_gain).reasons

    no_core =
      %{data | eval_per_feature:
        Enum.map(data.eval_per_feature, fn row ->
          if row.feature == :mood_da, do: %{row | valid_pairs: 0}, else: row
        end)}

    report = StateTransitionAudit.assess(no_core)
    assert :core_feature_coverage_missing in report.reasons
    assert :mood_da in report.core_features_below_minimum
  end
end
