defmodule Brain.Neural.StateTransitionAudit do
  @moduledoc """
  Privacy-preserving quality audit and conservative model-evidence gate.

  Only aggregate counts and numeric metrics are reported. No conversation
  text, individual memory, session ID, or model prediction is exported.
  A passing gate means "worth extended shadow evaluation", never permission
  to use learned predictions in live cognition.
  """

  alias Brain.Neural.StateTransitionBuilder

  @min_transitions 20
  @min_sessions 2
  @min_valid_eval_cells 100
  @min_relative_improvement 0.05
  @core_features [:confidence, :mood_da, :mood_5ht, :mood_glu, :mood_ne]
  @min_core_eval_pairs 5

  @doc "Summarize observed feature coverage, without retaining raw turns."
  def dataset(%{train: train, eval: evaluation, metadata: metadata}) do
    names = StateTransitionBuilder.feature_names()
    train_counts = counts(train.mask)
    eval_counts = counts(evaluation.mask)

    coverage =
      Enum.zip([names, train_counts, eval_counts])
      |> Enum.map(fn {name, train_count, eval_count} ->
        %{feature: name, train_pairs: train_count, eval_pairs: eval_count}
      end)

    %{
      train_transitions: metadata.train_examples,
      eval_transitions: metadata.eval_examples,
      train_session_count: length(metadata.train_sessions || []),
      eval_session_count: length(metadata.eval_sessions || []),
      split_mode: metadata.split_mode,
      complete_modulator_turns: metadata.complete_modulator_turns,
      missing_modulator_turns: metadata.missing_modulator_turns,
      feature_coverage: coverage,
      core_features_below_minimum:
        Enum.filter(@core_features, fn feature ->
          case Enum.find(coverage, &(&1.feature == feature)) do
            %{eval_pairs: n} when n >= @min_core_eval_pairs -> false
            _ -> true
          end
        end)
    }
  end

  @doc """
  Evidence gate for an offline residual artifact. Never authorizes deployment.

  Minimums are provisional, intentionally conservative engineering thresholds,
  not statistical guarantees. Independently repeated held-out runs and manual
  review are still required before any future runtime proposal.
  """
  def assess(%{} = result) do
    train_sessions = Map.get(result, :train_sessions) || []
    eval_sessions = Map.get(result, :eval_sessions) || []
    baseline = Map.get(result, :persistence_eval_mse)
    model = Map.get(result, :eval_mse)
    valid = Map.get(result, :valid_eval_cells, 0)
    per_feature = Map.get(result, :eval_per_feature) || []

    gain =
      if is_number(baseline) and baseline > 0 and is_number(model) do
        (baseline - model) / baseline
      else
        nil
      end

    core_coverage =
      Enum.filter(@core_features, fn feature ->
        case Enum.find(per_feature, &(&1.feature == feature)) do
          %{valid_pairs: n} when is_integer(n) and n >= @min_core_eval_pairs -> false
          _ -> true
        end
      end)

    reasons =
      []
      |> flag(result[:split_mode] != :session_holdout or result[:evaluated?] != true,
        :not_independent_holdout)
      |> flag(length(train_sessions) < @min_sessions, :too_few_training_sessions)
      |> flag(length(eval_sessions) < @min_sessions, :too_few_evaluation_sessions)
      |> flag(Map.get(result, :train_examples, 0) < @min_transitions, :too_few_training_transitions)
      |> flag(Map.get(result, :eval_examples, 0) < @min_transitions, :too_few_evaluation_transitions)
      |> flag(not is_integer(valid) or valid < @min_valid_eval_cells, :insufficient_observed_evaluation)
      |> flag(core_coverage != [], :core_feature_coverage_missing)
      |> flag(is_nil(gain) or gain < @min_relative_improvement, :insufficient_baseline_improvement)

    %{
      status: if(reasons == [], do: :candidate_for_extended_shadow, else: :not_ready),
      reasons: reasons,
      relative_improvement: gain,
      core_features_below_minimum: core_coverage,
      minimum_relative_improvement: @min_relative_improvement,
      minimum_train_transitions: @min_transitions,
      minimum_eval_transitions: @min_transitions,
      minimum_sessions_per_split: @min_sessions,
      minimum_valid_eval_cells: @min_valid_eval_cells,
      deployment_authorized?: false
    }
  end

  defp flag(reasons, true, reason), do: reasons ++ [reason]
  defp flag(reasons, false, _reason), do: reasons

  defp counts(mask) do
    mask
    |> Nx.sum(axes: [0])
    |> Nx.to_list()
    |> Enum.map(&round/1)
  end
end
