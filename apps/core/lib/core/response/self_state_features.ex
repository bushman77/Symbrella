defmodule Core.Response.SelfStateFeatures do
  @moduledoc """
  Pure feature shaping for self-state inputs used by response policy.
  """

  @spec self_state(map() | nil, map() | nil, map() | nil, map() | nil) :: map()
  def self_state(self_model, self_monitor, self_memory_recall, agency_memory) do
    self_model
    |> self_state_effects()
    |> apply_self_monitor(self_monitor)
    |> apply_self_memory_recall(self_memory_recall)
    |> apply_agency_memory(agency_memory)
  end

  @spec append_curiosity_probe(String.t(), map(), map(), map(), map() | nil) ::
          {String.t(), map() | nil}
  def append_curiosity_probe(text, features, decision, guard, skill) when is_binary(text) do
    cond do
      map_get(guard, :guardrail?, false) or skill != nil ->
        {text, nil}

      String.contains?(text, "?") ->
        {text, nil}

      curiosity_probe?(features, decision) ->
        probe = curiosity_probe_text(features)
        {text <> " " <> probe, %{text: probe, reason: :uncertainty_reduction}}

      true ->
        {text, nil}
    end
  end

  def append_curiosity_probe(text, _features, _decision, _guard, _skill), do: {text, nil}

  defp self_state_effects(nil), do: %{}

  defp self_state_effects(self_model) when is_map(self_model) do
    confidence = number(map_get(self_model, :confidence, 0.5))
    uncertainty = number(map_get(self_model, :uncertainty, 0.5))
    stability = number(map_get(self_model, :stability, 0.5))
    cognitive_load = number(map_get(self_model, :cognitive_load, 0.0))
    focus = normalize_focus(map_get(self_model, :focus, :balanced))

    effects =
      []
      |> maybe_effect(uncertainty >= 0.7, :hedge_under_uncertainty)
      |> maybe_effect(stability <= 0.35, :prefer_repair)
      |> maybe_effect(cognitive_load >= 0.85, :reduce_scope)
      |> maybe_effect(focus == :clarify, :ask_clarifying_question)
      |> maybe_effect(focus == :stabilize, :stabilize_before_acting)
      |> Enum.reverse()

    %{
      v: map_get(self_model, :v, 1),
      confidence: clamp01(confidence),
      uncertainty: clamp01(uncertainty),
      stability: clamp01(stability),
      cognitive_load: clamp01(cognitive_load),
      focus: focus,
      effects: effects
    }
  end

  defp self_state_effects(_), do: %{}

  defp apply_self_monitor(self_state, self_monitor)
       when is_map(self_state) and is_map(self_monitor) do
    monitor_effects =
      self_monitor
      |> map_get(:recovery_suggestions, [])
      |> List.wrap()

    monitor_warnings =
      self_monitor
      |> map_get(:warnings, [])
      |> List.wrap()
      |> Enum.map(&map_get(&1, :kind))
      |> Enum.reject(&is_nil/1)

    effects =
      self_state
      |> map_get(:effects, [])
      |> List.wrap()
      |> Kernel.++(monitor_effects)
      |> Enum.uniq()

    self_state
    |> Map.put(:effects, effects)
    |> Map.put(:self_monitor, %{
      status: map_get(self_monitor, :status),
      warning_kinds: monitor_warnings,
      recovery_suggestions: monitor_effects
    })
  end

  defp apply_self_monitor(self_state, _self_monitor), do: self_state

  defp apply_self_memory_recall(self_state, self_memory_recall)
       when is_map(self_state) and is_map(self_memory_recall) do
    remembered_effects =
      self_memory_recall
      |> map_get(:recovery_suggestions, [])
      |> List.wrap()

    if remembered_effects == [] do
      self_state
    else
      effects =
        self_state
        |> map_get(:effects, [])
        |> List.wrap()
        |> Kernel.++(remembered_effects)
        |> Enum.uniq()

      self_state
      |> Map.put(:effects, effects)
      |> Map.put(:self_memory_recall, %{
        memory_count: length(List.wrap(map_get(self_memory_recall, :memories, []))),
        warning_kinds: List.wrap(map_get(self_memory_recall, :warning_kinds, [])),
        recovery_suggestions: remembered_effects
      })
    end
  end

  defp apply_self_memory_recall(self_state, _self_memory_recall), do: self_state

  defp apply_agency_memory(self_state, agency_memory)
       when is_map(self_state) and is_map(agency_memory) do
    agency_effects = agency_memory |> map_get(:effects, []) |> List.wrap()

    if agency_effects == [] do
      self_state
    else
      effects =
        self_state
        |> map_get(:effects, [])
        |> List.wrap()
        |> Kernel.++(agency_effects)
        |> Enum.uniq()

      self_state
      |> Map.put(:effects, effects)
      |> Map.put(:agency_memory, Map.take(agency_memory, [:v, :event_count, :reasons, :stats]))
    end
  end

  defp apply_agency_memory(self_state, _agency_memory), do: self_state

  defp curiosity_probe?(features, decision) do
    self_state = Map.get(features, :self_state, %{})

    uncertainty = map_get(self_state, :uncertainty, 0.0)
    focus = map_get(self_state, :focus, :balanced)
    effects = map_get(self_state, :effects, [])

    effects != [] and
      (Map.get(decision, :action) in [:offer_options, :ask_first] or
         uncertainty >= 0.7 or focus == :clarify or :ask_clarifying_question in List.wrap(effects))
  end

  defp curiosity_probe_text(features) do
    case map_get(Map.get(features, :self_state, %{}), :focus, :balanced) do
      :clarify -> "What detail would reduce the uncertainty most?"
      :stabilize -> "What should I verify first before moving further?"
      _ -> "What is the most important detail to resolve next?"
    end
  end

  defp maybe_effect(effects, true, effect), do: [effect | effects]
  defp maybe_effect(effects, false, _effect), do: effects

  defp map_get(map, key, default \\ nil)

  defp map_get(map, key, default) when is_map(map),
    do: Map.get(map, key, Map.get(map, to_string(key), default))

  defp map_get(_map, _key, default), do: default

  defp number(value) when is_integer(value), do: value * 1.0
  defp number(value) when is_float(value), do: value
  defp number(_), do: 0.0

  defp normalize_focus(focus) when focus in [:balanced, :clarify, :execute, :stabilize],
    do: focus

  defp normalize_focus(focus) when is_binary(focus) do
    case focus do
      "clarify" -> :clarify
      "execute" -> :execute
      "stabilize" -> :stabilize
      _ -> :balanced
    end
  end

  defp normalize_focus(_), do: :balanced

  defp clamp01(x) when is_number(x), do: min(1.0, max(0.0, x))
  defp clamp01(_), do: 0.0
end
