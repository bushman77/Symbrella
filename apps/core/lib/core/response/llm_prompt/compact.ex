defmodule Core.Response.LlmPrompt.Compact do
  @moduledoc """
  Prompt-facing compaction and value formatting.

  This module is the boundary between structured runtime evidence and prompt
  strings. It accepts maps, tuples, lists, atoms, and numbers without relying on
  `String.Chars` for arbitrary terms.
  """

  alias Core.Response.Topics

  @spec runtime_state(term()) :: String.t()
  def runtime_state(nil), do: ""
  def runtime_state(state) when state == %{}, do: ""

  def runtime_state(state) when is_map(state) do
    parts =
      []
      |> maybe_add_present("scope", map_get(state, :scope))
      |> maybe_add_present("pressure_label", map_get(state, :pressure_label))
      |> maybe_add_present("mood", compact_mood(map_get(state, :mood)))
      |> maybe_add_present(
        "neuromodulators",
        compact_modulators(map_get(state, :neuromodulators))
      )
      |> maybe_add_present("mood_trace", compact_mood_trace(map_get(state, :mood_trace)))
      |> maybe_add_present("lifg", compact_lifg(map_get(state, :lifg)))

    case parts do
      [] -> ""
      _ -> "Runtime state: #{Enum.join(parts, "; ")}."
    end
  end

  def runtime_state(_), do: ""

  @spec comprehension(term()) :: String.t()
  def comprehension(nil), do: ""
  def comprehension(summary) when summary == %{}, do: ""

  def comprehension(summary) when is_map(summary) do
    parts =
      []
      |> maybe_add_present("intent", map_get(summary, :intent))
      |> maybe_add_present("understood", values(List.wrap(map_get(summary, :understood, []))))
      |> maybe_add_present("uncertain", values(List.wrap(map_get(summary, :uncertain, []))))
      |> maybe_add_present("degraded", map_get(summary, :degraded?))

    case parts do
      [] -> ""
      _ -> "Comprehension: #{Enum.join(parts, "; ")}."
    end
  end

  def comprehension(_), do: ""

  @spec symbolic_frame(term()) :: String.t()
  def symbolic_frame(nil), do: ""
  def symbolic_frame(frame) when frame == %{}, do: ""

  def symbolic_frame(frame) when is_map(frame) do
    lexical = map_get(frame, :lexical, %{})
    lifg = map_get(frame, :lifg, %{})

    parts =
      []
      |> maybe_add_present("intent", map_get(frame, :intent))
      |> maybe_add_present("confidence", compact_number(map_get(frame, :confidence)))
      |> maybe_add_present("keyword", map_get(frame, :keyword))
      |> maybe_add_present("event", map_get(frame, :event))
      |> maybe_add_present("subject", map_get(frame, :subject))
      |> maybe_add_present("medication", map_get(frame, :medication))
      |> maybe_add_present("consequence", map_get(frame, :consequence))
      |> maybe_add_present("temporal_context", map_get(frame, :temporal_context))
      |> maybe_add_present("domain", map_get(frame, :domain))
      |> maybe_add_present("polarity", map_get(frame, :polarity))
      |> maybe_add_present("top_terms", values(List.wrap(map_get(lexical, :top_terms, []))))
      |> maybe_add_present("lifg_degraded", map_get(lifg, :degraded?))
      |> maybe_add_present("episode", map_get(frame, :episode))

    case parts do
      [] -> ""
      _ -> "Symbolic frame: #{Enum.join(parts, "; ")}."
    end
  end

  def symbolic_frame(_), do: ""

  @spec response_policy(term()) :: String.t()
  def response_policy(nil), do: ""
  def response_policy(policy) when policy == %{}, do: ""

  def response_policy(policy) when is_map(policy) do
    parts =
      []
      |> maybe_add_present("tone", map_get(policy, :tone))
      |> maybe_add_present("verbosity", map_get(policy, :verbosity))
      |> maybe_add_present("curiosity", map_get(policy, :curiosity))
      |> maybe_add_present("caution", map_get(policy, :caution))
      |> maybe_add_present("pressure", map_get(policy, :emotional_pressure))

    instruction = map_get(policy, :instruction)

    case parts do
      [] -> ""
      _ -> "Response policy: #{Enum.join(parts, "; ")}. #{instruction}"
    end
  end

  def response_policy(_), do: ""

  @spec affect(term()) :: String.t()
  def affect(%{} = affect) do
    label = map_get(affect, :label)

    if present?(label) do
      "Simulated affect: label=#{value(label)}; expression=#{value(map_get(affect, :expression))}."
    else
      ""
    end
  end

  def affect(_), do: ""

  @spec response_posture(map(), map(), atom(), term(), term(), boolean()) :: String.t()
  def response_posture(features, decision, profile, comprehension, frame, guardrail?) do
    intent = map_get(features, :intent)
    mode = map_get(decision, :mode)
    action = map_get(decision, :action)
    confidence = map_get(features, :confidence_bucket)
    move = compact_move(features, decision, profile, comprehension, frame, guardrail?)
    terms = frame |> frame_terms() |> values()
    control = compact_control(map_get(features, :control_signals))

    parts =
      [
        "use these internal labels only as hidden shaping context",
        "do not quote them directly"
      ]
      |> maybe_add_present("intent", intent)
      |> maybe_add_present("mode", mode)
      |> maybe_add_present("action", action)
      |> maybe_add_present("confidence", confidence)
      |> maybe_add(guardrail?, "guardrail=true")
      |> maybe_add(degraded_posture?(comprehension, frame), "comprehension=degraded")
      |> maybe_add_present("move", move)
      |> maybe_add_present("terms", terms)
      |> maybe_add_present("control", control)

    "Response posture: #{Enum.join(parts, "; ")}."
  end

  @spec values(list()) :: String.t()
  def values(values) do
    values
    |> Enum.map(&value/1)
    |> Enum.reject(&(&1 == ""))
    |> Enum.join(", ")
  end

  @spec value(term()) :: String.t()
  def value(nil), do: ""
  def value(value) when is_binary(value), do: value
  def value(value) when is_atom(value), do: Atom.to_string(value)
  def value(value) when is_integer(value), do: Integer.to_string(value)
  def value(value) when is_float(value), do: compact_number(value)
  def value(value) when is_boolean(value), do: to_string(value)
  def value(value), do: inspect(value, limit: 8, printable_limit: 120)

  defp compact_move(features, decision, profile, comprehension, frame, guardrail?) do
    mode = map_get(decision, :mode)
    action = map_get(decision, :action)
    confidence = map_get(features, :confidence_bucket)
    intent = map_get(features, :intent)

    cond do
      guardrail? ->
        "brief safe redirect"

      skill_moves_to?(features, :personal_life_update) ->
        "acknowledge the personal milestone warmly"

      alien_life_thread_followup?(features) ->
        "continue the alien-life conversation"

      cosmic_life_text?(map_get(features, :text)) ->
        "answer the alien-life question directly; plausible but unconfirmed"

      degraded_posture?(comprehension, frame) or confidence == :low ->
        "state what is understood, then ask one targeted question only if necessary"

      profile == :brain_explainer ->
        "explain Symbrella as software control signals and evidence, not sentience"

      mode == :supportive_care or intent == :health_support ->
        "acknowledge health concern; avoid dose instructions; suggest pharmacist or prescriber if unsure"

      technical_posture?(mode, action, profile, confidence) ->
        "make the next concrete engineering action"

      true ->
        nil
    end
  end

  defp compact_mood(mood) when is_map(mood) do
    [
      "exploration=#{compact_number(map_get(mood, :exploration))}",
      "inhibition=#{compact_number(map_get(mood, :inhibition))}",
      "vigilance=#{compact_number(map_get(mood, :vigilance))}",
      "plasticity=#{compact_number(map_get(mood, :plasticity))}"
    ]
    |> Enum.join(", ")
  end

  defp compact_mood(_), do: nil

  defp compact_modulators(mods) when is_map(mods) do
    [
      "da=#{compact_number(map_get(mods, :dopamine, map_get(mods, :da)))}",
      "5ht=#{compact_number(map_get(mods, :serotonin, map_get(mods, :"5ht")))}",
      "glu=#{compact_number(map_get(mods, :glutamate, map_get(mods, :glu)))}",
      "ne=#{compact_number(map_get(mods, :norepinephrine, map_get(mods, :ne)))}"
    ]
    |> Enum.join(", ")
  end

  defp compact_modulators(_), do: nil

  defp compact_mood_trace(trace) when is_list(trace) do
    trace
    |> Enum.filter(&is_map/1)
    |> Enum.take(2)
    |> Enum.map(fn item ->
      deltas =
        item
        |> map_get(:deltas, %{})
        |> ordered_delta_pairs()
        |> Enum.map(fn {key, delta} -> "#{value(key)}=#{signed_number(delta)}" end)
        |> Enum.join(",")

      source = value(map_get(item, :source))
      pressure_label = value(map_get(item, :pressure_label))

      ":#{source}:#{pressure_label}:#{deltas}"
    end)
    |> Enum.join(" | ")
  end

  defp compact_mood_trace(_), do: nil

  defp compact_lifg(lifg) when is_map(lifg) do
    [
      "choices=#{map_get(lifg, :choices_count)}",
      "missing=#{map_get(lifg, :missing_candidates)}",
      "weak=#{map_get(lifg, :weak_decisions)}",
      "fallback=#{map_get(lifg, :fallback_winners)}",
      "degraded=#{map_get(lifg, :degraded?)}"
    ]
    |> Enum.reject(&String.ends_with?(&1, "="))
    |> Enum.join(", ")
  end

  defp compact_lifg(_), do: nil

  defp frame_terms(frame) when is_map(frame) do
    frame
    |> map_get(:lexical, %{})
    |> map_get(:top_terms, [])
    |> List.wrap()
  end

  defp frame_terms(_), do: []

  defp compact_control(control) when is_map(control) do
    control
    |> Enum.sort_by(fn {key, _value} -> value(key) end)
    |> Enum.map(fn {key, control_value} -> "#{value(key)}=#{value(control_value)}" end)
    |> Enum.join(", ")
  end

  defp compact_control(_), do: nil

  defp compact_number(value) when is_float(value),
    do: value |> Float.round(2) |> :erlang.float_to_binary(decimals: 2) |> trim_number()

  defp compact_number(value) when is_integer(value), do: Integer.to_string(value)
  defp compact_number(nil), do: nil
  defp compact_number(value), do: value(value)

  defp signed_number(value) when is_number(value) and value >= 0, do: "+" <> compact_number(value)
  defp signed_number(value), do: compact_number(value)

  defp trim_number(value) do
    value
    |> String.trim_trailing("0")
    |> String.trim_trailing(".")
  end

  defp ordered_delta_pairs(deltas) when is_map(deltas) do
    preferred = [:ne, "ne", :"5ht", "5ht", :da, "da", :glu, "glu"]

    Enum.sort_by(deltas, fn {key, _value} ->
      case Enum.find_index(preferred, &(&1 == key)) do
        nil -> 100 + :erlang.phash2(key, 100)
        index -> index
      end
    end)
  end

  defp ordered_delta_pairs(_), do: []

  defp degraded_posture?(comprehension, frame) do
    comprehension_degraded? =
      is_map(comprehension) and map_get(comprehension, :degraded?) == true

    lifg_degraded? =
      if is_map(frame) do
        frame |> map_get(:lifg, %{}) |> map_get(:degraded?) == true
      else
        false
      end

    comprehension_degraded? or lifg_degraded?
  end

  defp alien_life_thread_followup?(features) when is_map(features) do
    context_topic?(map_get(features, :context_status), :alien_life) and
      Topics.followup?(to_string(map_get(features, :text, "")), :alien_life)
  end

  defp alien_life_thread_followup?(_), do: false

  defp context_topic?(context_status, topic) when is_map(context_status) do
    context_status
    |> map_get(:topics, %{})
    |> Topics.has?(topic)
  end

  defp context_topic?(_, _), do: false

  defp cosmic_life_text?(text) when is_binary(text) do
    Regex.match?(
      ~r/\b(aliens?|extraterrestrial|life\s+elsewhere|universe|galax(?:y|ies)|solar\s+system|planet|planets|exoplanets?|ufos?|uaps?|unidentified\s+(?:flying\s+)?objects?|disclosure|declassif(?:y|ied|ication)|footage)\b/iu,
      text
    )
  end

  defp cosmic_life_text?(_), do: false

  defp technical_posture?(mode, action, profile, confidence) do
    confidence == :high and
      (profile == :technical_work or
         (mode == :collaborator and action in [:act_first, :answer]))
  end

  defp skill_moves_to?(features, skill_id) do
    map_get(features, :skill) == skill_id
  end

  defp maybe_add(notes, true, note), do: notes ++ [note]
  defp maybe_add(notes, false, _), do: notes

  defp maybe_add_present(notes, _label, nil), do: notes
  defp maybe_add_present(notes, _label, ""), do: notes

  defp maybe_add_present(notes, label, item) do
    notes ++ ["#{value(label)}=#{value(item)}"]
  end

  defp present?(nil), do: false
  defp present?(""), do: false
  defp present?(_), do: true

  defp map_get(map, key, default \\ nil)

  defp map_get(map, key, default) when is_map(map) and is_atom(key) do
    Map.get(map, key, Map.get(map, Atom.to_string(key), default))
  end

  defp map_get(_, _, default), do: default
end
