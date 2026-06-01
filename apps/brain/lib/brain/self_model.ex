defmodule Brain.SelfModel do
  @moduledoc """
  Canonical runtime self-state derived from inspectable Brain evidence.
  """

  defstruct confidence: 0.5,
            uncertainty: 0.5,
            stability: 0.5,
            focus: :balanced,
            vigilance: 0.5,
            plasticity: 0.5,
            inhibition: 0.5,
            cognitive_load: 0.0,
            mood: %{},
            active_goals: [],
            recent_errors: [],
            recent_actions: [],
            last_appraisal: nil,
            last_lifg: nil,
            self_other_attribution: %{},
            continuity: %{reboot_restored?: false, recent_episode_ids: []},
            updated_at_ms: nil,
            v: 1

  def from_runtime(runtime) when is_list(runtime) do
    runtime
    |> Map.new()
    |> from_runtime()
  end

  def from_runtime(%{} = runtime) do
    self_portrait = Map.get(runtime, :self_portrait, %{})
    meta = Map.get(runtime, :meta, %{})
    mood_snapshot = Map.get(runtime, :mood, %{})
    wm_snapshot = Map.get(runtime, :wm, %{})
    goals = Map.get(runtime, :goals, [])

    traits = Map.get(self_portrait, :traits, %{})
    patterns = Map.get(self_portrait, :patterns, %{})
    mood = Map.get(mood_snapshot, :mood, %{})

    confidence = bounded(meta[:conf] || traits[:confidence_baseline] || 0.5)

    cognitive_load = cognitive_load(wm_snapshot)
    uncertainty = clamp01(1.0 - confidence)

    %__MODULE__{
      confidence: confidence,
      uncertainty: uncertainty,
      stability: bounded(traits[:stability] || 0.5),
      focus: focus_from(confidence, uncertainty, cognitive_load),
      vigilance: bounded(mood[:vigilance] || 0.5),
      plasticity: bounded(mood[:plasticity] || 0.5),
      inhibition: bounded(mood[:inhibition] || 0.5),
      cognitive_load: cognitive_load,
      mood: mood_snapshot,
      active_goals: active_goals(goals),
      recent_errors: recent_errors(patterns),
      recent_actions: Map.get(self_portrait, :last_events, []),
      updated_at_ms: System.system_time(:millisecond)
    }
  end

  def export(%__MODULE__{} = model) do
    %{
      v: model.v,
      confidence: clamp01(model.confidence),
      uncertainty: clamp01(model.uncertainty),
      stability: clamp01(model.stability),
      focus: normalize_focus(model.focus),
      vigilance: clamp01(model.vigilance),
      plasticity: clamp01(model.plasticity),
      inhibition: clamp01(model.inhibition),
      cognitive_load: clamp01(model.cognitive_load),
      mood: map_or_empty(model.mood),
      active_goals: List.wrap(model.active_goals),
      recent_errors: List.wrap(model.recent_errors),
      recent_actions: List.wrap(model.recent_actions),
      last_appraisal: model.last_appraisal,
      last_lifg: model.last_lifg,
      self_other_attribution: map_or_empty(model.self_other_attribution),
      continuity: map_or_empty(model.continuity),
      updated_at_ms: model.updated_at_ms
    }
  end

  def import(%{} = snapshot) do
    with {:ok, 1} <- fetch_version(snapshot) do
      {:ok,
       %__MODULE__{
         v: 1,
         confidence: bounded(get(snapshot, :confidence, 0.5)),
         uncertainty: bounded(get(snapshot, :uncertainty, 0.5)),
         stability: bounded(get(snapshot, :stability, 0.5)),
         focus: normalize_focus(get(snapshot, :focus, :balanced)),
         vigilance: bounded(get(snapshot, :vigilance, 0.5)),
         plasticity: bounded(get(snapshot, :plasticity, 0.5)),
         inhibition: bounded(get(snapshot, :inhibition, 0.5)),
         cognitive_load: bounded(get(snapshot, :cognitive_load, 0.0)),
         mood: map_or_empty(get(snapshot, :mood, %{})),
         active_goals: List.wrap(get(snapshot, :active_goals, [])),
         recent_errors: List.wrap(get(snapshot, :recent_errors, [])),
         recent_actions: List.wrap(get(snapshot, :recent_actions, [])),
         last_appraisal: get(snapshot, :last_appraisal, nil),
         last_lifg: get(snapshot, :last_lifg, nil),
         self_other_attribution: map_or_empty(get(snapshot, :self_other_attribution, %{})),
         continuity: map_or_empty(get(snapshot, :continuity, %{})),
         updated_at_ms: get(snapshot, :updated_at_ms, nil)
       }}
    else
      {:ok, version} -> {:error, {:unsupported_version, version}}
      {:error, reason} -> {:error, reason}
    end
  end

  def import(_), do: {:error, :invalid_snapshot}

  def export_continuity(%__MODULE__{} = model) do
    model
    |> export()
    |> Map.take([
      :v,
      :confidence,
      :uncertainty,
      :stability,
      :focus,
      :cognitive_load,
      :active_goals,
      :continuity
    ])
  end

  def import_continuity(%{} = snapshot), do: __MODULE__.import(snapshot)
  def import_continuity(_), do: {:error, :invalid_snapshot}

  defp cognitive_load(%{wm: wm, cfg: %{capacity: capacity}})
       when is_list(wm) and is_number(capacity) and capacity > 0 do
    clamp01(length(wm) / capacity)
  end

  defp cognitive_load(_), do: 0.0

  defp focus_from(_confidence, _uncertainty, cognitive_load) when cognitive_load >= 0.85,
    do: :stabilize

  defp focus_from(_confidence, uncertainty, _cognitive_load) when uncertainty >= 0.7,
    do: :clarify

  defp focus_from(confidence, uncertainty, _cognitive_load)
       when confidence >= 0.7 and uncertainty <= 0.35,
       do: :execute

  defp focus_from(_confidence, _uncertainty, _cognitive_load), do: :balanced

  defp recent_errors(patterns) when is_map(patterns) do
    [:boundary_drops, :chargram_violations, :lifg_payload_gaps, :lifg_pos_anomalies]
    |> Enum.flat_map(fn key ->
      count = Map.get(patterns, key, 0)

      if is_number(count) and count > 0 do
        [%{kind: key, count: count}]
      else
        []
      end
    end)
  end

  defp active_goals(goals) when is_list(goals) do
    Enum.map(goals, fn
      %{id: id, label: label, priority: priority, tension: tension} = goal ->
        %{
          id: id,
          label: label,
          priority: bounded(priority),
          tension: bounded(tension),
          source: Map.get(goal, :source),
          reason: Map.get(goal, :reason)
        }

      other ->
        other
    end)
  end

  defp active_goals(_), do: []

  defp number(value) when is_integer(value), do: value * 1.0
  defp number(value) when is_float(value), do: value
  defp number(_), do: 0.0

  defp bounded(value), do: value |> number() |> clamp01()

  defp clamp01(value) when is_number(value), do: value |> max(0.0) |> min(1.0)
  defp clamp01(_), do: 0.0

  defp fetch_version(snapshot) do
    case get(snapshot, :v, nil) do
      version when is_integer(version) -> {:ok, version}
      version when is_binary(version) -> parse_version(version)
      nil -> {:error, :missing_version}
      other -> {:error, {:invalid_version, other}}
    end
  end

  defp parse_version(version) do
    case Integer.parse(version) do
      {int, ""} -> {:ok, int}
      _ -> {:error, {:invalid_version, version}}
    end
  end

  defp normalize_focus(focus) when focus in [:balanced, :clarify, :execute, :stabilize],
    do: focus

  defp normalize_focus(focus) when is_binary(focus) do
    case focus do
      "balanced" -> :balanced
      "clarify" -> :clarify
      "execute" -> :execute
      "stabilize" -> :stabilize
      _ -> :balanced
    end
  end

  defp normalize_focus(_), do: :balanced

  defp map_or_empty(value) when is_map(value), do: value
  defp map_or_empty(_), do: %{}

  defp get(map, key, default) when is_map(map) do
    Map.get(map, key, Map.get(map, to_string(key), default))
  end
end
