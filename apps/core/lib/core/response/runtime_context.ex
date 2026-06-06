defmodule Core.Response.RuntimeContext do
  @moduledoc """
  Boundary collector for Brain runtime state used in response prompts.

  This module isolates optional Brain process reads from LLM synthesis. It returns
  plain maps that prompt/context modules can consume without knowing where the
  runtime state came from.
  """

  alias Core.Response.LlmPrompt
  alias Core.Response.SelfStateSummary

  @compile {:no_warn_undefined, Brain}
  @compile {:no_warn_undefined, Brain.Introspection}
  @compile {:no_warn_undefined, Brain.Introspect}
  @compile {:no_warn_undefined, Brain.MoodCore}
  @compile {:no_warn_undefined, Brain.SelfPortrait}

  @type snapshot :: %{
          wm_items: list(),
          self_model: map() | nil,
          runtime_state: map()
        }

  @spec snapshot() :: snapshot()
  def snapshot do
    wm_items = safe_wm_items()
    self_model = safe_self_model()

    %{
      wm_items: wm_items,
      self_model: self_model,
      runtime_state: safe_runtime_state(wm_items)
    }
  end

  @spec mood_like() :: map()
  def mood_like do
    case safe_mood_snapshot() do
      %{mood: %{} = mood} = snapshot ->
        %{mood: mood, tone_hint: Map.get(snapshot, :tone_hint)}

      _ ->
        %{}
    end
  end

  @spec self_model_log(map() | nil) :: map() | nil
  def self_model_log(nil), do: nil

  def self_model_log(model) do
    %{
      v: model_value(model, :v),
      confidence: model_value(model, :confidence),
      uncertainty: model_value(model, :uncertainty),
      stability: model_value(model, :stability),
      cognitive_load: model_value(model, :cognitive_load)
    }
  end

  @spec lifg_runtime_from_snapshot(map()) :: map()
  def lifg_runtime_from_snapshot(%{} = snapshot) do
    state = map_get(snapshot, :state, %{})
    last = map_get(state, :last, %{})
    audit = map_get(last, :audit, %{})
    guards = map_get(last, :guards, %{})
    meta = map_get(last, :meta, %{})
    choices = List.wrap(map_get(last, :choices, []))

    missing = number(map_get(guards, :missing_candidates) || map_get(audit, :missing_candidates))
    weak = number(map_get(audit, :weak_decisions))
    fallback = number(map_get(audit, :fallback_winners) || map_get(audit, :mwe_fallbacks))
    chargram = number(map_get(guards, :chargram_violation) || map_get(audit, :chargram_violation))
    boundary = boundary_count(guards, audit)
    acc_conflict = map_get(meta, :acc_conflict)

    degraded? =
      missing > 0 or weak > 0 or fallback > 0 or chargram > 0 or boundary > 0 or
        (is_number(acc_conflict) and acc_conflict >= 0.5)

    %{
      focused?: true,
      running?: map_get(snapshot, :running?) == true,
      intent: map_get(last, :intent),
      confidence: map_get(last, :confidence),
      choices_count: length(choices),
      missing_candidates: missing,
      weak_decisions: weak,
      fallback_winners: fallback,
      chargram_violations: chargram,
      boundary_drops: boundary,
      acc_conflict: acc_conflict,
      degraded?: degraded?
    }
  end

  def lifg_runtime_from_snapshot(_), do: %{}

  defp safe_wm_items do
    if Code.ensure_loaded?(Brain) and function_exported?(Brain, :snapshot_wm, 0) do
      safe_call(
        fn ->
          case Brain.snapshot_wm() do
            %{wm: wm} when is_list(wm) -> wm
            _ -> []
          end
        end,
        []
      )
    else
      []
    end
  end

  defp safe_self_model do
    if Code.ensure_loaded?(Brain.Introspection) and
         function_exported?(Brain.Introspection, :snapshot, 0) do
      safe_call(fn -> Brain.Introspection.snapshot() end, nil)
    else
      nil
    end
  end

  defp safe_runtime_state(wm_items) do
    mood = safe_mood_snapshot()
    lifg = safe_lifg_runtime()
    wm = wm_runtime(wm_items)
    self_portrait = safe_self_portrait_snapshot()

    runtime = %{
      source: :brain,
      phase: :prompt_context,
      status: :ready,
      mood: mood_values(mood),
      neuromodulators: neuromodulator_values(mood),
      tone_hint: map_get(mood, :tone_hint),
      pressure_label: map_get(mood, :pressure_label),
      mood_trace: mood |> map_get(:mood_trace, []) |> List.wrap() |> Enum.take(3),
      wm: wm,
      lifg: lifg,
      self_portrait: self_portrait
    }

    Map.put(
      runtime,
      :self_state_summary,
      SelfStateSummary.prompt_line(%{
        mood: mood,
        self_portrait: self_portrait,
        wm: %{wm: wm_items, cfg: %{capacity: map_get(wm, :capacity)}},
        lifg: %{state: %{last: lifg}, running?: map_get(lifg, :running?)}
      })
    )
  end

  defp safe_self_portrait_snapshot do
    if Code.ensure_loaded?(Brain.SelfPortrait) and
         function_exported?(Brain.SelfPortrait, :snapshot, 0) do
      safe_call(fn -> Brain.SelfPortrait.snapshot() end, %{})
    else
      %{}
    end
  end

  defp safe_mood_snapshot do
    if Code.ensure_loaded?(Brain.MoodCore) and function_exported?(Brain.MoodCore, :snapshot, 0) do
      safe_call(fn -> Brain.MoodCore.snapshot() end, %{})
    else
      %{}
    end
  end

  defp safe_lifg_runtime do
    if Code.ensure_loaded?(Brain.Introspect) and
         function_exported?(Brain.Introspect, :snapshot, 1) do
      safe_call(fn -> Brain.Introspect.snapshot(:lifg) end, %{})
      |> lifg_runtime_from_snapshot()
    else
      %{}
    end
  end

  defp wm_runtime(wm_items) when is_list(wm_items) do
    capacity =
      if Code.ensure_loaded?(Brain) and function_exported?(Brain, :snapshot_wm, 0) do
        safe_call(
          fn ->
            case Brain.snapshot_wm() do
              %{cfg: %{capacity: cap}} when is_number(cap) -> cap
              _ -> nil
            end
          end,
          nil
        )
      end

    size = length(wm_items)
    load = if is_number(capacity) and capacity > 0, do: size / capacity, else: nil

    %{
      size: size,
      capacity: capacity,
      load: load,
      concepts: LlmPrompt.summarize_wm(wm_items)
    }
  end

  defp mood_values(mood) do
    case map_get(mood, :mood) do
      values when is_map(values) ->
        %{
          exploration: map_get(values, :exploration),
          inhibition: map_get(values, :inhibition),
          vigilance: map_get(values, :vigilance),
          plasticity: map_get(values, :plasticity)
        }

      _ ->
        %{}
    end
  end

  defp neuromodulator_values(mood) do
    case map_get(mood, :levels) do
      levels when is_map(levels) ->
        %{
          dopamine: map_get(levels, :da),
          serotonin: map_get(levels, :"5ht"),
          glutamate: map_get(levels, :glu),
          norepinephrine: map_get(levels, :ne)
        }

      _ ->
        %{}
    end
  end

  defp boundary_count(guards, audit) do
    rejected = map_get(guards, :rejected_by_boundary) || map_get(audit, :rejected_by_boundary)

    cond do
      is_list(rejected) -> length(rejected)
      is_binary(rejected) -> String.length(rejected)
      is_number(map_get(audit, :boundary_drops)) -> map_get(audit, :boundary_drops)
      true -> 0
    end
  end

  defp number(value) when is_integer(value), do: value
  defp number(value) when is_float(value), do: round(value)
  defp number(_), do: 0

  defp model_value(model, key) when is_map(model), do: Map.get(model, key)
  defp model_value(_, _), do: nil

  defp safe_call(fun, default) when is_function(fun, 0) do
    fun.()
  rescue
    _ -> default
  catch
    :exit, _ -> default
  end

  defp map_get(map, key, default \\ nil)

  defp map_get(map, key, default) when is_map(map) and is_atom(key) do
    Map.get(map, key, Map.get(map, Atom.to_string(key), default))
  end

  defp map_get(_, _, default), do: default
end
