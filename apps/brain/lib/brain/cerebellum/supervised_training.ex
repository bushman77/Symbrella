defmodule Brain.Cerebellum.SupervisedTraining do
  @moduledoc """
  Experimental supervised training for LIFG sense ranking.

  Only explicitly human-reviewed training examples may
  produce learning updates. Evaluation examples are read-only.

  Experimental scopes are isolated from production LIFG.
  """

  alias Brain.Cerebellum

  @scope_prefix "cerebellum_experiment:"

  @features [:context_fit, :lex_fit, :rel_prior, :activation, :intent_bias]

  def train(example, scope) do
    with :ok <- validate_scope(scope),
         :ok <- validate_example(example, :train) do
      Cerebellum.learn_lifg(
        nil,
        nil,
        example.candidates,
        example.base_scores,
        scope: scope,
        context_key: example.context_key,
        margin_tau: 1.0,
        supervision: %{
          source: :gold_label,
          target_id: example.label_id
        }
      )
    end
  end

  def evaluate(example, scope) do
    with :ok <- validate_scope(scope),
         :ok <- validate_example(example, :eval) do
      baseline = example.base_scores

      adjusted =
        Cerebellum.calibrate_scores(
          nil,
          baseline,
          example.candidates,
          %{
            scope: scope,
            context_key: example.context_key,
            acc_conflict: 1.0,
            acc_conflict_tau: 0.0
          }
        )

      baseline_id = winner(baseline)
      adjusted_id = winner(adjusted)

      {:ok,
       %{
         label_id: example.label_id,
         baseline_id: baseline_id,
         adjusted_id: adjusted_id,
         baseline_correct?: baseline_id == example.label_id,
         adjusted_correct?: adjusted_id == example.label_id
       }}
    end
  end

  defp winner(scores) do
    scores
    |> Enum.max_by(fn {_id, score} -> score end)
    |> elem(0)
  end

  defp validate_scope(scope) when is_binary(scope) do
    if String.starts_with?(scope, @scope_prefix) and
         byte_size(scope) > byte_size(@scope_prefix) do
      :ok
    else
      {:error, :non_experimental_scope}
    end
  end

  defp validate_scope(_), do: {:error, :non_experimental_scope}

  defp validate_example(
         %{
           split: split,
           source: :human_review,
           reviewed_by: reviewer,
           label_id: label,
           context_key: ctx,
           candidates: candidates,
           base_scores: scores
         },
         required_split
       )
       when is_binary(reviewer) and is_binary(label) and
              is_binary(ctx) and is_list(candidates) and
              is_map(scores) do
    valid_candidates? =
      Enum.all?(candidates, fn candidate ->
        is_map(candidate) and
          is_binary(Map.get(candidate, :id)) and
          Enum.all?(@features, fn feature ->
            is_number(Map.get(candidate, feature))
          end)
      end)

    ids =
      if valid_candidates? do
        Enum.map(candidates, & &1.id)
      else
        []
      end

    cond do
      split != required_split ->
        {:error, :wrong_split}

      String.trim(reviewer) == "" ->
        {:error, :missing_reviewer}

      String.trim(label) == "" or byte_size(ctx) != 32 ->
        {:error, :invalid_label_or_context}

      not valid_candidates? or length(ids) < 2 ->
        {:error, :invalid_candidates}

      Enum.any?(ids, &(&1 == "")) or length(Enum.uniq(ids)) != length(ids) ->
        {:error, :duplicate_or_empty_candidate_id}

      label not in ids ->
        {:error, :label_not_in_candidates}

      MapSet.new(ids) != MapSet.new(Map.keys(scores)) ->
        {:error, :score_candidates_mismatch}

      not Enum.all?(Map.values(scores), &is_number/1) ->
        {:error, :invalid_scores}

      true ->
        :ok
    end
  end

  defp validate_example(_, _), do: {:error, :unverified_example}
end
