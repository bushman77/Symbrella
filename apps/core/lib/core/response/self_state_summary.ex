defmodule Core.Response.SelfStateSummary do
  @moduledoc """
  Prompt-facing and chat-facing summaries of Symbrella's inspectable self-state.

  This module formats runtime evidence. It does not claim human feeling or
  consciousness.
  """

  @compile {:no_warn_undefined, Brain}
  @compile {:no_warn_undefined, Brain.Introspect}
  @compile {:no_warn_undefined, Brain.MoodCore}
  @compile {:no_warn_undefined, Brain.SelfPortrait}

  @spec live_snapshot() :: map()
  def live_snapshot do
    %{
      mood: safe_mood_snapshot(),
      self_portrait: safe_self_portrait_snapshot(),
      wm: safe_wm_snapshot(),
      lifg: safe_region_snapshot(:lifg)
    }
  end

  @spec self_portrait_answer() :: String.t()
  def self_portrait_answer do
    live = live_snapshot()
    summary = summarize(live)

    """
    You can get my self portrait from the local Symbrella runtime:

    1. Open the Brain dashboard at `/brain` and look for the SelfPortrait panel.
    2. From IEx inside the umbrella, call `Brain.SelfPortrait.snapshot()`.
    3. For lower-level inspection, use `Brain.Introspect.snapshot(:self_portrait)`.

    #{summary}

    That portrait is a software self-state snapshot: traits, recent events, confidence, and monitor signals. It is not a claim of human consciousness or feelings.
    """
    |> String.trim()
  end

  @spec self_check_answer() :: String.t()
  def self_check_answer do
    summary = live_snapshot() |> summarize()

    """
    I am doing a runtime self-check against the local Symbrella state.

    #{summary}

    I do not see this as human panic or consciousness. It is an inspectable software state: mood indices, raw modulators, working memory, and recent region snapshots. If the app is crashing or behaving incorrectly, share the error text or logs and I will help trace the failing subsystem.
    """
    |> String.trim()
  end

  @spec mood_indices_answer() :: String.t()
  def mood_indices_answer do
    mood_snapshot = safe_mood_snapshot()
    mood = map_get(mood_snapshot, :mood, %{})
    baseline_mood = baseline_mood_indices(mood_snapshot)
    pressure_label = map_get(mood_snapshot, :pressure_label, mood_pressure_label(mood))
    trace = mood_snapshot |> map_get(:mood_trace, []) |> List.wrap() |> Enum.take(3)

    if map_size(mood) == 0 do
      "I do not have a live MoodCore snapshot available for this turn."
    else
      """
      Current mood indices:
      - Exploration: #{format_index_with_delta(mood, baseline_mood, :exploration)}
      - Inhibition: #{format_index_with_delta(mood, baseline_mood, :inhibition)}
      - Vigilance: #{format_index_with_delta(mood, baseline_mood, :vigilance)}
      - Plasticity: #{format_index_with_delta(mood, baseline_mood, :plasticity)}

      Pressure state: #{pressure_label}

      #{mood_interpretation(mood, pressure_label)}
      #{mood_trace_summary(trace)}
      """
      |> String.trim()
    end
  end

  @spec summarize(map()) :: String.t()
  def summarize(%{} = state) do
    lines =
      []
      |> maybe_add(mood_line(map_get(state, :mood)))
      |> maybe_add(self_portrait_line(map_get(state, :self_portrait)))
      |> maybe_add(wm_line(map_get(state, :wm)))
      |> maybe_add(lifg_line(map_get(state, :lifg)))

    case lines do
      [] -> "Current live summary: no Brain runtime snapshot was available for this turn."
      _ -> "Current live summary:\n" <> Enum.map_join(lines, "\n", &("- " <> &1))
    end
  end

  @spec prompt_line(map()) :: String.t()
  def prompt_line(%{} = state) do
    state
    |> summarize()
    |> String.replace("\n", " ")
  end

  def prompt_line(_), do: ""

  defp mood_line(%{} = mood_snapshot) do
    mood = map_get(mood_snapshot, :mood, %{})
    tone = map_get(mood_snapshot, :tone_hint)
    pressure_label = map_get(mood_snapshot, :pressure_label)
    levels = map_get(mood_snapshot, :levels, %{})

    parts =
      []
      |> maybe_add_number("exploration", map_get(mood, :exploration))
      |> maybe_add_number("inhibition", map_get(mood, :inhibition))
      |> maybe_add_number("vigilance", map_get(mood, :vigilance))
      |> maybe_add_number("plasticity", map_get(mood, :plasticity))

    raw =
      []
      |> maybe_add_number("da", map_get(levels, :da))
      |> maybe_add_number("5ht", map_get(levels, :"5ht"))
      |> maybe_add_number("glu", map_get(levels, :glu))
      |> maybe_add_number("ne", map_get(levels, :ne))

    cond do
      parts != [] and raw != [] ->
        "Mood indices are #{Enum.join(parts, ", ")}; raw modulators are #{Enum.join(raw, ", ")}; tone_hint=#{inspect(tone || :neutral)}; pressure_label=#{pressure_label || mood_pressure_label(mood)}."

      parts != [] ->
        "Mood indices are #{Enum.join(parts, ", ")}; tone_hint=#{inspect(tone || :neutral)}; pressure_label=#{pressure_label || mood_pressure_label(mood)}."

      true ->
        nil
    end
  end

  defp mood_line(_), do: nil

  defp self_portrait_line(%{} = portrait) do
    traits = map_get(portrait, :traits, %{})
    patterns = map_get(portrait, :patterns, %{})
    last_events = map_get(portrait, :last_events, [])

    trait_parts =
      []
      |> maybe_add_number("curiosity_bias", map_get(traits, :curiosity_bias))
      |> maybe_add_number("confidence_baseline", map_get(traits, :confidence_baseline))
      |> maybe_add_number("stability", map_get(traits, :stability))
      |> maybe_add_number("novelty_seeking", map_get(traits, :novelty_seeking))
      |> maybe_add_number("risk_aversion", map_get(traits, :risk_aversion))

    pattern_parts =
      patterns
      |> nonzero_pairs()
      |> Enum.take(5)
      |> Enum.map(fn {key, value} -> "#{key}=#{value}" end)

    event_count = if is_list(last_events), do: length(last_events), else: 0

    cond do
      trait_parts != [] and pattern_parts != [] ->
        "SelfPortrait traits are #{Enum.join(trait_parts, ", ")}; active patterns are #{Enum.join(pattern_parts, ", ")}; recent_events=#{event_count}."

      trait_parts != [] ->
        "SelfPortrait traits are #{Enum.join(trait_parts, ", ")}; recent_events=#{event_count}."

      pattern_parts != [] ->
        "SelfPortrait active patterns are #{Enum.join(pattern_parts, ", ")}; recent_events=#{event_count}."

      portrait != %{} ->
        "SelfPortrait is present but has no active pattern summary yet."

      true ->
        nil
    end
  end

  defp self_portrait_line(_), do: nil

  defp wm_line(%{wm: wm, cfg: cfg}) when is_list(wm) and is_map(cfg) do
    capacity = map_get(cfg, :capacity)
    concepts = summarize_wm(wm)

    load =
      if is_number(capacity) and capacity > 0 do
        Float.round(length(wm) / capacity, 2)
      end

    details =
      []
      |> maybe_add_present("size", length(wm))
      |> maybe_add_present("capacity", capacity)
      |> maybe_add_present("load", load)
      |> maybe_add(concepts != [], "focus=#{Enum.join(concepts, ", ")}")

    if details == [], do: nil, else: "Working memory: #{Enum.join(details, ", ")}."
  end

  defp wm_line(%{} = wm_snapshot) do
    wm = map_get(wm_snapshot, :wm)
    cfg = map_get(wm_snapshot, :cfg, %{})
    if is_list(wm), do: wm_line(%{wm: wm, cfg: cfg}), else: nil
  end

  defp wm_line(_), do: nil

  defp lifg_line(%{} = snapshot) do
    state = map_get(snapshot, :state, %{})
    last = map_get(state, :last, %{})
    audit = map_get(last, :audit, %{})
    choices = List.wrap(map_get(last, :choices, []))

    details =
      []
      |> maybe_add_present("running", map_get(snapshot, :running?))
      |> maybe_add_present("intent", map_get(last, :intent))
      |> maybe_add_number("confidence", map_get(last, :confidence))
      |> maybe_add_present("choices", length(choices))
      |> maybe_add_present("weak_decisions", map_get(audit, :weak_decisions))
      |> maybe_add_present("fallback_winners", map_get(audit, :fallback_winners))

    if details == [], do: nil, else: "LIFG snapshot: #{Enum.join(details, ", ")}."
  end

  defp lifg_line(_), do: nil

  defp baseline_mood_indices(%{} = mood_snapshot) do
    baseline = map_get(mood_snapshot, :baseline, %{})

    da = map_get(baseline, :da)
    s5 = map_get(baseline, :"5ht")
    glu = map_get(baseline, :glu)
    ne = map_get(baseline, :ne)

    if Enum.all?([da, s5, glu, ne], &is_number/1) do
      %{
        exploration: 0.6 * da + 0.4 * ne,
        inhibition: s5,
        vigilance: ne,
        plasticity: 0.5 * da + 0.5 * glu
      }
    else
      %{}
    end
  end

  defp baseline_mood_indices(_), do: %{}

  defp mood_interpretation(%{} = mood, pressure_label) do
    exploration = number(map_get(mood, :exploration), 0.0)
    inhibition = number(map_get(mood, :inhibition), 0.0)
    vigilance = number(map_get(mood, :vigilance), 0.0)
    plasticity = number(map_get(mood, :plasticity), 0.0)

    cond do
      vigilance >= 0.65 and inhibition <= 0.55 ->
        "Interpretation: high vigilance with reduced inhibition. This is a de-escalation/self-check state, not a calm baseline."

      vigilance >= 0.50 and inhibition <= 0.55 ->
        "Interpretation: elevated vigilance with reduced inhibition. This is cautious emergency attention, not a calm baseline."

      vigilance >= 0.50 ->
        "Interpretation: vigilance is elevated, so the response posture should be more careful and safety-focused than playful."

      inhibition >= 0.60 and vigilance < 0.45 ->
        "Interpretation: restrained and steady, close to the calm baseline."

      exploration >= 0.60 or plasticity >= 0.60 ->
        "Interpretation: engaged and adaptive, with more exploration/plasticity than baseline."

      true ->
        "Interpretation: near baseline, with no strong mood-pressure signal."
    end
    |> Kernel.<>(" Pressure label: #{pressure_label}.")
  end

  defp mood_pressure_label(%{} = mood) do
    vigilance = number(map_get(mood, :vigilance), 0.0)
    inhibition = number(map_get(mood, :inhibition), 0.0)
    exploration = number(map_get(mood, :exploration), 0.0)
    plasticity = number(map_get(mood, :plasticity), 0.0)

    cond do
      vigilance >= 0.65 and inhibition <= 0.55 -> :deescalation_pressure
      vigilance >= 0.50 and inhibition <= 0.55 -> :cautious_emergency_attention
      vigilance >= 0.50 -> :heightened_attention
      inhibition >= 0.60 and vigilance < 0.45 -> :steady_restraint
      exploration >= 0.60 or plasticity >= 0.60 -> :engaged_adaptation
      true -> :baseline
    end
  end

  defp mood_pressure_label(_), do: :baseline

  defp mood_trace_summary([]), do: ""

  defp mood_trace_summary(trace) do
    lines =
      trace
      |> Enum.map(&format_trace_entry/1)
      |> Enum.reject(&(&1 == ""))

    case lines do
      [] -> ""
      _ -> "Recent mood trace:\n" <> Enum.map_join(lines, "\n", &("- " <> &1))
    end
  end

  defp format_trace_entry(%{} = entry) do
    source = map_get(entry, :source)
    pressure = map_get(entry, :pressure_label)
    deltas = map_get(entry, :deltas, %{})

    delta_text =
      [:ne, :"5ht", :da, :glu]
      |> Enum.map(fn key ->
        value = map_get(deltas, key)
        if is_number(value), do: "#{key}=#{format_delta(value)}", else: nil
      end)
      |> Enum.reject(&is_nil/1)
      |> Enum.join(", ")

    [inspect(source), pressure && "-> #{pressure}", delta_text]
    |> Enum.reject(&(&1 in [nil, ""]))
    |> Enum.join(" ")
  end

  defp format_trace_entry(_), do: ""

  defp safe_mood_snapshot do
    if Code.ensure_loaded?(Brain.MoodCore) and function_exported?(Brain.MoodCore, :snapshot, 0) do
      safe_call(fn -> Brain.MoodCore.snapshot() end, %{})
    else
      %{}
    end
  end

  defp safe_self_portrait_snapshot do
    if Code.ensure_loaded?(Brain.SelfPortrait) and
         function_exported?(Brain.SelfPortrait, :snapshot, 0) do
      safe_call(fn -> Brain.SelfPortrait.snapshot() end, %{})
    else
      %{}
    end
  end

  defp safe_wm_snapshot do
    if Code.ensure_loaded?(Brain) and function_exported?(Brain, :snapshot_wm, 0) do
      safe_call(fn -> Brain.snapshot_wm() end, %{})
    else
      %{}
    end
  end

  defp safe_region_snapshot(region) do
    if Code.ensure_loaded?(Brain.Introspect) and
         function_exported?(Brain.Introspect, :snapshot, 1) do
      safe_call(fn -> Brain.Introspect.snapshot(region) end, %{})
    else
      %{}
    end
  end

  defp safe_call(fun, fallback) do
    fun.()
  rescue
    _ -> fallback
  catch
    :exit, _ -> fallback
  end

  defp summarize_wm(wm) when is_list(wm) do
    wm
    |> Core.Response.LlmPrompt.summarize_wm()
    |> Enum.take(5)
  end

  defp nonzero_pairs(map) when is_map(map) do
    map
    |> Enum.filter(fn {_key, value} -> is_number(value) and value > 0 end)
    |> Enum.sort_by(fn {_key, value} -> -value end)
  end

  defp nonzero_pairs(_), do: []

  defp maybe_add(list, nil), do: list
  defp maybe_add(list, ""), do: list
  defp maybe_add(list, value), do: list ++ [value]

  defp maybe_add(list, true, value), do: maybe_add(list, value)
  defp maybe_add(list, false, _value), do: list

  defp maybe_add_number(list, label, value) when is_number(value) do
    list ++ ["#{label}=#{Float.round(value * 1.0, 2)}"]
  end

  defp maybe_add_number(list, _label, _value), do: list

  defp maybe_add_present(list, _label, nil), do: list
  defp maybe_add_present(list, _label, ""), do: list
  defp maybe_add_present(list, label, value), do: list ++ ["#{label}=#{value}"]

  defp format_index_with_delta(%{} = mood, %{} = baseline_mood, key) do
    value = map_get(mood, key)
    baseline = map_get(baseline_mood, key)

    case {value, baseline} do
      {v, b} when is_number(v) and is_number(b) ->
        "#{format_index(v)} (#{format_delta(v - b)} from baseline)"

      {v, _} ->
        format_index(v)
    end
  end

  defp format_index_with_delta(_mood, _baseline_mood, _key), do: "n/a"

  defp format_index(value) when is_number(value),
    do: value |> Float.round(2) |> :erlang.float_to_binary(decimals: 2)

  defp format_index(_), do: "n/a"

  defp format_delta(delta) when is_number(delta) do
    rounded = Float.round(delta * 1.0, 2)
    sign = if rounded >= 0, do: "+", else: ""
    sign <> :erlang.float_to_binary(rounded, decimals: 2)
  end

  defp format_delta(_), do: "n/a"

  defp number(value, _default) when is_number(value), do: value * 1.0
  defp number(_value, default), do: default * 1.0

  defp map_get(map, key, default \\ nil)

  defp map_get(map, key, default) when is_map(map) and is_atom(key) do
    Map.get(map, key, Map.get(map, Atom.to_string(key), default))
  end

  defp map_get(_, _, default), do: default
end
