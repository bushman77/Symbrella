defmodule Core.Brain.Prefrontal do
  @moduledoc """
  Core-side semantic-control bridge for prefrontal planner modules.

  This stage turns the current `SemanticInput` into a compact context for
  `Brain.Prefrontal`, then attaches the resulting bounded control signals back
  onto the SI. The planners stay pure; this module is the pipeline contract.
  """

  @spec attach(map(), keyword()) :: map()
  def attach(%{} = si, opts) when is_list(opts) do
    if Keyword.get(opts, :prefrontal_control?, true) == false do
      si
    else
      ctx = context_from_si(si, opts)
      signals = prefrontal_signals(ctx)

      prefrontal = %{
        version: 1,
        ctx: ctx,
        signals: signals
      }

      si
      |> Map.put(:prefrontal, prefrontal)
      |> Map.put(:control_signals, signals)
      |> Core.Pipeline.Trace.append(
        :prefrontal_control,
        decision: :attached,
        reason: :semantic_control_signals,
        scores: trace_scores(signals),
        meta: %{
          policy: Map.get(signals, :policy),
          top_k: Map.get(signals, :top_k),
          max_retries: Map.get(signals, :max_retries),
          branch_budget: Map.get(signals, :branch_budget),
          switch_after_ms: Map.get(signals, :switch_after_ms)
        }
      )
    end
  end

  def attach(si, _opts), do: si

  @spec context_from_si(map(), keyword()) :: map()
  def context_from_si(%{} = si, opts \\ []) when is_list(opts) do
    candidates = si |> Map.get(:sense_candidates, %{}) |> candidate_buckets()
    lifg_choices = si |> Map.get(:lifg_choices, []) |> list_or_empty()
    perception = si |> Map.get(:perception, %{}) |> map_or_empty()
    comprehension = si |> Map.get(:comprehension, %{}) |> map_or_empty()

    %{
      acc_conflict: clamp01(Map.get(si, :acc_conflict, comprehension_conflict(comprehension))),
      novelty: clamp01(novelty(si, perception)),
      wm_load: clamp01(wm_load(opts)),
      wm_diversity: clamp01(diversity(lifg_choices)),
      predicted_confidence: clamp01(Map.get(si, :confidence, 0.5)),
      actual_outcome: if(Map.get(comprehension, :degraded?) == true, do: 0.0, else: 1.0),
      recent_success_rate: if(Map.get(comprehension, :degraded?) == true, do: 0.35, else: 0.65),
      avg_cost_ms: cost_ms(si, candidates),
      ofc_value: clamp01(Map.get(si, :ofc_value, 0.5)),
      top_k_base: Keyword.get(opts, :prefrontal_top_k_base, 3),
      max_retries_base: Keyword.get(opts, :prefrontal_max_retries_base, 1)
    }
  end

  defp prefrontal_signals(ctx) do
    if Code.ensure_loaded?(Brain.Prefrontal) and
         function_exported?(Brain.Prefrontal, :signals_map, 1) do
      Brain.Prefrontal.signals_map(ctx)
    else
      %{}
    end
  rescue
    _ -> %{}
  catch
    _, _ -> %{}
  end

  defp trace_scores(signals) do
    %{
      utility_prior: Map.get(signals, :utility_prior),
      explore_rate: Map.get(signals, :explore_rate),
      salience_boost: Map.get(signals, :salience_boost),
      confidence_scale: normalized_scale(Map.get(signals, :confidence_scale)),
      acc_conflict_gain: normalized_scale(Map.get(signals, :acc_conflict_gain))
    }
  end

  defp candidate_buckets(candidates) when is_map(candidates) do
    Map.new(candidates, fn {idx, bucket} -> {idx, list_or_empty(bucket)} end)
  end

  defp candidate_buckets(_), do: %{}

  defp novelty(si, perception) do
    cond do
      is_number(Map.get(si, :novelty)) ->
        Map.get(si, :novelty)

      is_number(nested_get(si, [:episode, :novelty])) ->
        nested_get(si, [:episode, :novelty])

      true ->
        average_numeric_map(Map.get(perception, :salience, %{}))
    end
  end

  defp comprehension_conflict(%{} = comprehension) do
    if Map.get(comprehension, :degraded?) == true, do: 0.7, else: 0.0
  end

  defp wm_load(opts) do
    cond do
      Keyword.has_key?(opts, :wm_load) ->
        Keyword.fetch!(opts, :wm_load)

      Code.ensure_loaded?(Brain) and function_exported?(Brain, :snapshot_wm, 0) ->
        case Brain.snapshot_wm() do
          %{wm: wm} when is_list(wm) -> length(wm) / 7.0
          _ -> 0.0
        end

      true ->
        0.0
    end
  rescue
    _ -> 0.0
  catch
    _, _ -> 0.0
  end

  defp diversity([]), do: 0.0

  defp diversity(items) when is_list(items) do
    ids =
      items
      |> Enum.map(&candidate_id/1)
      |> Enum.reject(&is_nil/1)
      |> Enum.uniq()

    length(ids) / max(length(items), 1)
  end

  defp cost_ms(si, candidates) do
    token_count = si |> Map.get(:tokens, []) |> list_or_empty() |> length()
    candidate_count = candidates |> Map.values() |> Enum.map(&length/1) |> Enum.sum()
    120 + token_count * 18 + candidate_count * 12
  end

  defp average_numeric_map(map) when is_map(map) and map_size(map) > 0 do
    values = map |> Map.values() |> Enum.filter(&is_number/1)

    case values do
      [] -> 0.0
      _ -> Enum.sum(values) / length(values)
    end
  end

  defp average_numeric_map(_), do: 0.0

  defp candidate_id(%{} = item), do: Map.get(item, :id) || Map.get(item, "id")
  defp candidate_id(_), do: nil

  defp nested_get(%{} = map, [key]), do: Map.get(map, key) || Map.get(map, to_string(key))

  defp nested_get(%{} = map, [key | rest]) do
    case nested_get(map, [key]) do
      %{} = next -> nested_get(next, rest)
      _ -> nil
    end
  end

  defp nested_get(_, _), do: nil

  defp normalized_scale(value) when is_number(value), do: clamp01((value - 0.5) / 1.0)
  defp normalized_scale(_), do: 0.0

  defp map_or_empty(%{} = map), do: map
  defp map_or_empty(_), do: %{}

  defp list_or_empty(list) when is_list(list), do: list
  defp list_or_empty(_), do: []

  defp clamp01(value) when is_number(value), do: (value * 1.0) |> max(0.0) |> min(1.0)
  defp clamp01(_), do: 0.0
end
