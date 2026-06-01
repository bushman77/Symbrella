defmodule Core.Response.AgencyMemory do
  @moduledoc """
  Summarizes recent agency ledger events into bounded decision pressure.

  This is the first learning-from-agency layer: recent decisions do not rewrite
  policy directly. They become inspectable effects that the existing self-state
  policy can already understand.
  """

  @default_limit 12
  @max_limit 50

  @spec recall(term(), keyword()) :: map()
  def recall(session_id, opts \\ []) do
    if enabled?() do
      limit = opts |> Keyword.get(:limit, @default_limit) |> normalize_limit()

      Db.AgencyEvents.recent(session_id: session_id, limit: limit)
      |> summarize()
    else
      %{}
    end
  end

  @spec summarize([map()]) :: map()
  def summarize(events) when is_list(events) do
    events = Enum.take(events, @max_limit)

    stats =
      Enum.reduce(events, empty_stats(), fn event, acc ->
        update_stats(acc, event)
      end)

    effects = effects_from_stats(stats)
    reasons = reasons_from_stats(stats)

    %{
      v: 1,
      event_count: stats.event_count,
      effects: effects,
      reasons: reasons,
      stats: Map.drop(stats, [:event_count])
    }
  end

  def summarize(_), do: %{v: 1, event_count: 0, effects: [], reasons: [], stats: %{}}

  defp enabled? do
    Application.get_env(:core, :agency_ledger_enabled?, true) == true
  end

  defp empty_stats do
    %{
      event_count: 0,
      clarify_count: 0,
      fallback_count: 0,
      low_confidence_count: 0,
      repair_count: 0,
      reduce_scope_count: 0,
      stabilize_count: 0,
      negative_confidence_delta_count: 0,
      negative_trust_delta_count: 0
    }
  end

  defp update_stats(acc, event) do
    decision = map_get(event, :decision, %{})
    reasons = map_get(event, :reasons, %{})
    self_state = map_get(event, :self_state, %{})
    outcome = map_get(event, :outcome, %{})
    input = map_get(event, :input, %{})
    reflection = map_get(event, :reflection, %{})
    reflection_signals = List.wrap(map_get(reflection, :signals, []))
    adjustment = map_get(reflection, :next_time_adjustment)
    confidence_delta = number(map_get(reflection, :confidence_delta))
    trust_delta = number(map_get(reflection, :trust_delta))

    effects =
      List.wrap(map_get(self_state, :effects, [])) ++
        List.wrap(map_get(outcome, :self_state_effects, [])) ++
        reflection_effects(reflection_signals, adjustment)

    action = map_get(event, :action) || map_get(decision, :action)
    response_source = map_get(decision, :response_source)

    acc
    |> Map.update!(:event_count, &(&1 + 1))
    |> bump(clarify_event?(action, effects), :clarify_count)
    |> bump(fallback_event?(response_source, reasons), :fallback_count)
    |> bump(low_confidence_event?(input), :low_confidence_count)
    |> bump(repair_event?(effects), :repair_count)
    |> bump(:reduce_scope in effects, :reduce_scope_count)
    |> bump(:stabilize_before_acting in effects, :stabilize_count)
    |> bump(confidence_delta < 0.0, :negative_confidence_delta_count)
    |> bump(trust_delta < 0.0, :negative_trust_delta_count)
  end

  defp bump(stats, true, key), do: Map.update!(stats, key, &(&1 + 1))
  defp bump(stats, false, _key), do: stats

  defp clarify_event?(action, effects) do
    action in [:ask_first, :offer_options, "ask_first", "offer_options"] or
      :ask_clarifying_question in effects or "ask_clarifying_question" in effects or
      :hedge_under_uncertainty in effects or "hedge_under_uncertainty" in effects
  end

  defp fallback_event?(response_source, reasons) do
    response_source in [:template_fallback, "template_fallback"] or
      present?(map_get(reasons, :fallback_reason))
  end

  defp low_confidence_event?(input) do
    confidence = number(map_get(input, :confidence))
    confidence > 0.0 and confidence <= 0.35
  end

  defp repair_event?(effects) do
    :prefer_repair in effects or "prefer_repair" in effects or
      :stabilize_before_acting in effects or "stabilize_before_acting" in effects
  end

  defp effects_from_stats(stats) do
    []
    |> maybe_effect(stats.repair_count >= 2, :prefer_repair)
    |> maybe_effect(stats.negative_confidence_delta_count >= 2, :hedge_under_uncertainty)
    |> maybe_effect(
      stats.stabilize_count >= 2 or stats.negative_trust_delta_count >= 2,
      :stabilize_before_acting
    )
    |> maybe_effect(
      stats.reduce_scope_count >= 2 or stats.fallback_count >= 2 or
        stats.negative_confidence_delta_count >= 2,
      :reduce_scope
    )
    |> maybe_effect(
      stats.clarify_count >= 2 or stats.low_confidence_count >= 2,
      :ask_clarifying_question
    )
    |> Enum.reverse()
  end

  defp reasons_from_stats(stats) do
    []
    |> maybe_effect(stats.repair_count >= 2, :recent_repairs)
    |> maybe_effect(stats.stabilize_count >= 2, :recent_stabilization)
    |> maybe_effect(stats.fallback_count >= 2, :recent_template_fallbacks)
    |> maybe_effect(stats.reduce_scope_count >= 2, :recent_scope_reductions)
    |> maybe_effect(stats.clarify_count >= 2, :recent_clarifications)
    |> maybe_effect(stats.low_confidence_count >= 2, :recent_low_confidence)
    |> maybe_effect(stats.negative_confidence_delta_count >= 2, :recent_confidence_drops)
    |> maybe_effect(stats.negative_trust_delta_count >= 2, :recent_trust_drops)
    |> Enum.reverse()
  end

  defp reflection_effects(signals, adjustment) do
    []
    |> maybe_effect(signal?(signals, :clarify), :ask_clarifying_question)
    |> maybe_effect(signal?(signals, :uncertainty), :hedge_under_uncertainty)
    |> maybe_effect(signal?(signals, :stabilize), :stabilize_before_acting)
    |> maybe_effect(signal?(signals, :repair), :prefer_repair)
    |> maybe_effect(
      signal?(signals, :reduce_scope) or adjustment in [:reduce_scope, "reduce_scope"],
      :reduce_scope
    )
  end

  defp signal?(signals, signal), do: signal in signals or Atom.to_string(signal) in signals

  defp maybe_effect(list, true, effect), do: [effect | list]
  defp maybe_effect(list, false, _effect), do: list

  defp normalize_limit(limit) when is_integer(limit) and limit > 0, do: min(limit, @max_limit)
  defp normalize_limit(_), do: @default_limit

  defp map_get(map, key, default \\ nil)

  defp map_get(%_struct{} = struct, key, default),
    do: struct |> Map.from_struct() |> map_get(key, default)

  defp map_get(%{} = map, key, default) when is_atom(key) do
    Map.get(map, key, Map.get(map, Atom.to_string(key), default))
  end

  defp map_get(_, _, default), do: default

  defp number(value) when is_integer(value), do: value * 1.0
  defp number(value) when is_float(value), do: value
  defp number(_), do: 0.0

  defp present?(value), do: value not in [nil, "", [], %{}]
end
