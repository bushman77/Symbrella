defmodule Brain.Cerebellum.Benchmark do
  @moduledoc """
  Read-only accuracy benchmark for supervised Cerebellum evaluation.

  Compares saved Stage1 base scores against Cerebellum-adjusted
  scores on labeled evaluation examples.

  Never calls the learning API.
  """

  alias Brain.Cerebellum.SupervisedTraining

  @doc """
  Evaluate a nonempty list of reviewed examples.

  Returns accuracy percentages, correction/regression counts,
  and individual prediction results.

  Examples must use split: :eval.
  Scope must be an experimental Cerebellum scope.
  """
  @spec run(list(), String.t()) ::
          {:ok, map()} | {:error, term()}

  def run([], _scope), do: {:error, :empty_dataset}

  def run(examples, scope) when is_list(examples) do
    result =
      examples
      |> Enum.with_index(1)
      |> Enum.reduce_while({:ok, []}, fn {example, index}, {:ok, rows} ->
        case SupervisedTraining.evaluate(example, scope) do
          {:ok, outcome} ->
            row = %{
              index: index,
              id: Map.get(example, :id, "example-#{index}"),
              label_id: outcome.label_id,
              baseline_id: outcome.baseline_id,
              adjusted_id: outcome.adjusted_id,
              baseline_correct?: outcome.baseline_correct?,
              adjusted_correct?: outcome.adjusted_correct?,
              correction?:
                not outcome.baseline_correct? and
                  outcome.adjusted_correct?,
              regression?:
                outcome.baseline_correct? and
                  not outcome.adjusted_correct?
            }

            {:cont, {:ok, [row | rows]}}

          {:error, reason} ->
            {:halt, {:error, {:invalid_example, index, reason}}}
        end
      end)

    case result do
      {:ok, rows} ->
        {:ok, summarize(Enum.reverse(rows))}

      {:error, _reason} = error ->
        error
    end
  end

  def run(_examples, _scope), do: {:error, :invalid_dataset}

  defp summarize(rows) do
    total = length(rows)

    baseline_correct =
      Enum.count(rows, & &1.baseline_correct?)

    adjusted_correct =
      Enum.count(rows, & &1.adjusted_correct?)

    corrections =
      Enum.count(rows, & &1.correction?)

    regressions =
      Enum.count(rows, & &1.regression?)

    unchanged_correct =
      Enum.count(rows, fn row ->
        row.baseline_correct? and row.adjusted_correct?
      end)

    unchanged_wrong =
      Enum.count(rows, fn row ->
        not row.baseline_correct? and
          not row.adjusted_correct?
      end)

    %{
      total: total,
      baseline_correct: baseline_correct,
      adjusted_correct: adjusted_correct,
      baseline_accuracy_pct: percent(baseline_correct, total),
      adjusted_accuracy_pct: percent(adjusted_correct, total),
      accuracy_change_pp: percent(adjusted_correct - baseline_correct, total),
      corrections: corrections,
      regressions: regressions,
      unchanged_correct: unchanged_correct,
      unchanged_wrong: unchanged_wrong,
      net_corrections: corrections - regressions,
      results: rows
    }
  end

  defp percent(count, total) do
    Float.round(count * 100.0 / total, 2)
  end
end
