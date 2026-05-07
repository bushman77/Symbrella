defmodule Brain.SelfModel do
  @moduledoc """
  Canonical runtime self-state derived from inspectable Brain evidence.
  """

  defstruct confidence: 0.5,
            uncertainty: 0.5,
            stability: 0.5,
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

    traits = Map.get(self_portrait, :traits, %{})
    patterns = Map.get(self_portrait, :patterns, %{})
    mood = Map.get(mood_snapshot, :mood, %{})

    confidence = bounded(meta[:conf] || traits[:confidence_baseline] || 0.5)

    %__MODULE__{
      confidence: confidence,
      uncertainty: clamp01(1.0 - confidence),
      stability: bounded(traits[:stability] || 0.5),
      vigilance: bounded(mood[:vigilance] || 0.5),
      plasticity: bounded(mood[:plasticity] || 0.5),
      inhibition: bounded(mood[:inhibition] || 0.5),
      cognitive_load: cognitive_load(wm_snapshot),
      mood: mood_snapshot,
      recent_errors: recent_errors(patterns),
      recent_actions: Map.get(self_portrait, :last_events, []),
      updated_at_ms: System.system_time(:millisecond)
    }
  end

  defp cognitive_load(%{wm: wm, cfg: %{capacity: capacity}})
       when is_list(wm) and is_number(capacity) and capacity > 0 do
    clamp01(length(wm) / capacity)
  end

  defp cognitive_load(_), do: 0.0

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

  defp number(value) when is_integer(value), do: value * 1.0
  defp number(value) when is_float(value), do: value
  defp number(_), do: 0.0

  defp bounded(value), do: value |> number() |> clamp01()

  defp clamp01(value) when is_number(value), do: value |> max(0.0) |> min(1.0)
  defp clamp01(_), do: 0.0
end
