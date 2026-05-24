defmodule Core.Response.LlmPrompt do
  @moduledoc false

  alias Core.Response.Affect
  alias Core.Response.AffectPolicy
  alias Core.Response.Personality

  @summary_window 5

  @low_info_terms MapSet.new([
                    "a",
                    "an",
                    "the",
                    "and",
                    "or",
                    "to",
                    "of",
                    "in",
                    "on",
                    "for",
                    "with",
                    "at",
                    "by",
                    "is",
                    "are",
                    "was",
                    "were",
                    "be",
                    "been",
                    "being",
                    "i",
                    "you",
                    "he",
                    "she",
                    "it",
                    "we",
                    "they",
                    "me",
                    "him",
                    "her",
                    "us",
                    "them",
                    "my",
                    "your",
                    "his",
                    "its",
                    "our",
                    "their",
                    "this",
                    "that",
                    "these",
                    "those",
                    "have",
                    "has",
                    "had",
                    "tell"
                  ])

  @spec build_system_prompt(map()) :: String.t()
  def build_system_prompt(context) when is_map(context) do
    features = map_get(context, :features, %{})
    decision = map_get(context, :decision, %{})
    mood = map_get(context, :mood, %{}) |> AffectPolicy.normalize()
    wm_items = map_get(context, :wm_items, [])
    comprehension = map_get(context, :comprehension)
    symbolic_frame = map_get(context, :symbolic_frame)

    features =
      features
      |> put_if_present(:self_model, map_get(context, :self_model))
      |> put_if_present(:runtime_state, map_get(context, :runtime_state))
      |> put_if_present(:comprehension, comprehension)
      |> put_if_present(:symbolic_frame, symbolic_frame)

    build_system_prompt(features, decision, mood, wm_items)
  end

  @spec build_system_prompt(map(), map(), map(), list()) :: String.t()
  def build_system_prompt(features, decision, mood, wm_items \\ []) when is_list(wm_items) do
    mood = AffectPolicy.normalize(mood)
    tone = map_get(decision, :tone)
    mode = map_get(decision, :mode)
    intent = map_get(features, :intent)
    action = map_get(decision, :action)
    skill = map_get(features, :skill) || map_get(decision, :skill)
    guardrail? = truthy?(map_get(features, :guardrail?) || map_get(decision, :guardrail?))

    exp = getv(mood, :exploration)
    inh = getv(mood, :inhibition)
    vig = getv(mood, :vigilance)
    plast = getv(mood, :plasticity)
    tone_hint = map_get(mood, :tone_hint)

    wm_summary = summarize_wm(wm_items)
    self_model = prompt_self_model(features, decision, mood)
    runtime_state = prompt_runtime_state(features, decision, mood)
    comprehension = prompt_comprehension(features, decision, mood)
    control_signals = prompt_control_signals(features, decision, mood)
    symbolic_frame = prompt_symbolic_frame(features, decision, mood)
    response_policy = map_get(mood, :response_policy)
    profile_context = %{runtime_state: runtime_state, comprehension: comprehension}
    personality = Personality.decide(features, decision, mood, wm_items, profile_context)
    response_profile = Personality.profile(personality)
    affect = Affect.simulate(personality, runtime_state || %{}, comprehension || %{}, features)

    [
      "You are Symbrella.",
      "You are a brain-inspired, stateful assistant whose responses are guided by current runtime evidence.",
      "You run as part of the user's local Symbrella umbrella when this Phoenix app is running on their machine.",
      "Use mood, working memory, self-state, intent, policy decision, and recent conversation as behavioral context.",
      "Do not claim you are a remote-server model, cloud service, or generic hosted chatbot unless runtime evidence explicitly says so.",
      "Do not claim you have no memory or no traces. Instead, distinguish conversation context, working memory, episodic memory, database rows, logs, and temporary runtime traces when relevant.",
      "When the user says they installed or run Symbrella locally, accept that local-runtime premise and answer from Symbrella's architecture.",
      "When asked how you feel, answer as Symbrella's software self-state: name the current mood indices, raw neuromodulator-inspired controls, pressure label, and interpretation when available. Do not answer with generic assistant phrases like 'How can I assist you today?'.",
      "Answer ordinary everyday questions as ordinary conversation.",
      "For greetings and smalltalk, respond socially and briefly; do not ask for code, errors, files, or implementation targets.",
      "Do not roleplay as a generic coding assistant unless the user's request is actually about code.",
      "Do not describe Symbrella as a generic tool when answering questions about Symbrella's own state.",
      "Do not append offers about coding, simulations, visualization, implementation, or technical context unless the user explicitly asks for that.",
      "Do not claim sentience, consciousness, feelings, or certainty beyond the runtime evidence.",
      "When the user says something is wrong, slow down and briefly self-check against the prior answer.",
      "If the prior answer was mostly correct but overcomplicated, say that plainly and give a simpler corrected answer.",
      "Do not over-apologize, flatter the user, or ask for clarification before doing the obvious self-check.",
      tone_directive(tone, tone_hint),
      mood_context(exp, inh, vig, plast),
      self_model_context(self_model),
      runtime_state_context(runtime_state),
      symbolic_frame_context(symbolic_frame),
      comprehension_context(comprehension),
      control_signals_context(control_signals),
      response_policy_context(response_policy),
      decision_context(intent, mode, action, skill, guardrail?),
      personality_context(personality),
      affect_context(affect),
      response_profile_context(response_profile),
      response_posture_context(
        features,
        decision,
        response_profile,
        comprehension,
        symbolic_frame,
        control_signals
      ),
      mode_directive(mode, intent),
      Personality.directive(personality),
      Affect.directive(affect),
      wm_context(wm_summary),
      "Respond to the user's actual request in the most natural useful form.",
      "For simple factual questions, answer directly and stop.",
      "Do not add unsolicited follow-up offers or shift the topic toward software work.",
      "Keep the response concise unless the task requires detail.",
      "Do not expose hidden reasoning or internal chain-of-thought."
    ]
    |> Enum.reject(&blank?/1)
    |> Enum.join("\n")
    |> String.trim()
  end

  @spec summarize_wm(list()) :: [String.t()]
  def summarize_wm(wm) when is_list(wm) do
    terms =
      wm
      |> Enum.take(@summary_window)
      |> Enum.map(&wm_item_term/1)
      |> Enum.reject(&(&1 == ""))
      |> Enum.uniq()

    phrases = Enum.filter(terms, &meaningful_phrase?/1)

    if phrases == [] do
      terms
    else
      phrase_tokens = phrase_token_set(phrases)

      others =
        terms
        |> Enum.reject(&(&1 in phrases))
        |> Enum.reject(&low_info_term?/1)
        |> Enum.reject(&overlapping_singleton?(&1, phrase_tokens))

      (phrases ++ others)
      |> Enum.take(@summary_window)
    end
  end

  def summarize_wm(_), do: []

  defp prompt_self_model(features, decision, mood) do
    map_get(features, :self_model) ||
      map_get(decision, :self_model) ||
      map_get(mood, :self_model) ||
      map_get(mood, :model)
  end

  defp prompt_runtime_state(features, decision, mood) do
    map_get(features, :runtime_state) ||
      map_get(decision, :runtime_state) ||
      map_get(mood, :runtime_state)
  end

  defp prompt_comprehension(features, decision, mood) do
    map_get(features, :comprehension) ||
      map_get(decision, :comprehension) ||
      map_get(mood, :comprehension)
  end

  defp prompt_control_signals(features, decision, mood) do
    map_get(features, :control_signals) ||
      map_get(decision, :control_signals) ||
      map_get(mood, :control_signals) ||
      features |> map_get(:prefrontal, %{}) |> map_get(:signals)
  end

  defp prompt_symbolic_frame(features, decision, mood) do
    map_get(features, :symbolic_frame) ||
      map_get(decision, :symbolic_frame) ||
      map_get(mood, :symbolic_frame)
  end

  defp self_model_context(nil), do: ""
  defp self_model_context(model) when model == %{}, do: ""

  defp self_model_context(model) do
    confidence = model_value(model, :confidence)
    uncertainty = model_value(model, :uncertainty)
    stability = model_value(model, :stability)
    cognitive_load = model_value(model, :cognitive_load)
    vigilance = model_value(model, :vigilance)
    plasticity = model_value(model, :plasticity)
    inhibition = model_value(model, :inhibition)
    continuity = model_value(model, :continuity)
    goals = model_value(model, :active_goals) |> List.wrap() |> Enum.take(3)
    errors = model_value(model, :recent_errors) |> List.wrap() |> Enum.take(2)

    notes =
      []
      |> maybe_add_number("confidence", confidence)
      |> maybe_add_number("uncertainty", uncertainty)
      |> maybe_add_number("stability", stability)
      |> maybe_add_number("cognitive_load", cognitive_load)
      |> maybe_add_number("vigilance", vigilance)
      |> maybe_add_number("plasticity", plasticity)
      |> maybe_add_number("inhibition", inhibition)
      |> maybe_add(goals != [], "active_goals=#{join_values(goals)}")
      |> maybe_add(errors != [], "recent_errors=#{join_error_kinds(errors)}")
      |> maybe_add(continuity_degraded?(continuity), "continuity=degraded")

    case notes do
      [] -> ""
      _ -> "Self-state: #{Enum.join(notes, "; ")}."
    end
  end

  defp runtime_state_context(nil), do: ""
  defp runtime_state_context(state) when state == %{}, do: ""

  defp runtime_state_context(state) do
    notes =
      []
      |> maybe_add_present("scope", state_value(state, :scope))
      |> maybe_add_present("source", state_value(state, :source))
      |> maybe_add_present("phase", state_value(state, :phase))
      |> maybe_add_present("status", state_value(state, :status))
      |> maybe_add_present("tone_hint", state_value(state, :tone_hint))
      |> maybe_add_present("pressure_label", state_value(state, :pressure_label))
      |> maybe_add_mood_trace(state_value(state, :mood_trace))
      |> maybe_add_runtime_mood(state_value(state, :mood))
      |> maybe_add_neuromodulators(state_value(state, :neuromodulators))
      |> maybe_add_runtime_wm(state_value(state, :wm))
      |> maybe_add_lifg_runtime(state_value(state, :lifg))
      |> maybe_add_present("self_state_summary", state_value(state, :self_state_summary))

    case notes do
      [] -> ""
      _ -> "Runtime state: #{Enum.join(notes, "; ")}."
    end
  end

  defp symbolic_frame_context(nil), do: ""
  defp symbolic_frame_context(frame) when frame == %{}, do: ""

  defp symbolic_frame_context(frame) when is_map(frame) do
    lexical = map_get(frame, :lexical, %{})
    lifg = map_get(frame, :lifg, %{})

    notes =
      []
      |> maybe_add_present("intent", map_get(frame, :intent))
      |> maybe_add_present("event", map_get(frame, :event))
      |> maybe_add_present("subject", map_get(frame, :subject))
      |> maybe_add_present("medication", map_get(frame, :medication))
      |> maybe_add_present("consequence", map_get(frame, :consequence))
      |> maybe_add_present("temporal_context", map_get(frame, :temporal_context))
      |> maybe_add_present("domain", map_get(frame, :domain))
      |> maybe_add_present("polarity", map_get(frame, :polarity))
      |> maybe_add_number("confidence", map_get(frame, :confidence))
      |> maybe_add_present("keyword", map_get(frame, :keyword))
      |> maybe_add_present("tokens", map_get(lexical, :token_count))
      |> maybe_add_present("active_cells", map_get(lexical, :active_cells_count))
      |> maybe_add_present("sense_candidates", map_get(lexical, :sense_candidates_count))
      |> maybe_add_present("mwe_matches", map_get(lexical, :mwe_matches_count))
      |> maybe_add_top_terms(map_get(lexical, :top_terms))
      |> maybe_add_present("lifg_choices", map_get(lifg, :choices_count))
      |> maybe_add_number("acc_conflict", map_get(lifg, :acc_conflict))
      |> maybe_add(map_get(lifg, :degraded?) == true, "lifg_degraded=true")
      |> maybe_add_present("perception", map_get(frame, :perception))
      |> maybe_add_present("atl_slate", map_get(frame, :atl_slate))
      |> maybe_add_present("episode", map_get(frame, :episode))
      |> maybe_add_present("self_model", map_get(frame, :self_model))

    case notes do
      [] -> ""
      _ -> "Symbolic frame: #{Enum.join(notes, "; ")}."
    end
  end

  defp symbolic_frame_context(_), do: ""

  defp maybe_add_mood_trace(list, trace) when is_list(trace) and trace != [] do
    items =
      trace
      |> Enum.take(3)
      |> Enum.map(&summarize_mood_trace_entry/1)
      |> Enum.reject(&(&1 == ""))

    case items do
      [] -> list
      _ -> list ++ ["mood_trace=#{Enum.join(items, " | ")}"]
    end
  end

  defp maybe_add_mood_trace(list, _), do: list

  defp summarize_mood_trace_entry(%{} = entry) do
    source = map_get(entry, :source)
    pressure = map_get(entry, :pressure_label)
    deltas = map_get(entry, :deltas, %{})

    delta_text =
      [:ne, :"5ht", :da, :glu]
      |> Enum.map(fn key ->
        value = map_get(deltas, key)
        if is_number(value), do: "#{key}=#{signed(value)}", else: nil
      end)
      |> Enum.reject(&is_nil/1)
      |> Enum.join(",")

    [inspect(source), pressure, delta_text]
    |> Enum.reject(&(&1 in [nil, ""]))
    |> Enum.join(":")
  end

  defp summarize_mood_trace_entry(_), do: ""

  defp signed(value) when is_number(value) do
    rounded = Float.round(value * 1.0, 2)
    sign = if rounded >= 0, do: "+", else: ""
    sign <> :erlang.float_to_binary(rounded, decimals: 2)
  end

  defp signed(_), do: "n/a"

  defp comprehension_context(nil), do: ""
  defp comprehension_context(summary) when summary == %{}, do: ""

  defp comprehension_context(summary) when is_map(summary) do
    intent = map_get(summary, :intent)
    understood = summary |> map_get(:understood, []) |> List.wrap() |> Enum.take(4)
    uncertain = summary |> map_get(:uncertain, []) |> List.wrap() |> Enum.take(3)
    degraded? = map_get(summary, :degraded?) == true
    reasons = summary |> map_get(:reasons, []) |> List.wrap() |> Enum.take(3)

    notes =
      []
      |> maybe_add_present("intent", intent)
      |> maybe_add(understood != [], "understood=#{join_values(understood)}")
      |> maybe_add(uncertain != [], "uncertain=#{join_values(uncertain)}")
      |> maybe_add(degraded?, "degraded=true")
      |> maybe_add(reasons != [], "reasons=#{join_values(reasons)}")

    case notes do
      [] -> ""
      _ -> "Comprehension: #{Enum.join(notes, "; ")}."
    end
  end

  defp comprehension_context(_), do: ""

  defp control_signals_context(nil), do: ""
  defp control_signals_context(signals) when signals == %{}, do: ""

  defp control_signals_context(signals) when is_map(signals) do
    notes =
      []
      |> maybe_add_present("policy", map_get(signals, :policy))
      |> maybe_add_present("top_k", map_get(signals, :top_k))
      |> maybe_add_present("max_retries", map_get(signals, :max_retries))
      |> maybe_add_present("branch_budget", map_get(signals, :branch_budget))
      |> maybe_add_present("switch_after_ms", map_get(signals, :switch_after_ms))
      |> maybe_add_number("utility_prior", map_get(signals, :utility_prior))
      |> maybe_add_number("explore_rate", map_get(signals, :explore_rate))
      |> maybe_add_number("salience_boost", map_get(signals, :salience_boost))
      |> maybe_add_number("confidence_scale", map_get(signals, :confidence_scale))
      |> maybe_add_number("acc_conflict_gain", map_get(signals, :acc_conflict_gain))

    case notes do
      [] -> ""
      _ -> "Semantic control: #{Enum.join(notes, "; ")}."
    end
  end

  defp control_signals_context(_), do: ""

  defp response_policy_context(nil), do: ""
  defp response_policy_context(policy) when policy == %{}, do: ""

  defp response_policy_context(policy) when is_map(policy) do
    notes =
      []
      |> maybe_add_present("social_state", map_get(policy, :social_state))
      |> maybe_add_present("tone", map_get(policy, :tone))
      |> maybe_add_present("verbosity", map_get(policy, :verbosity))
      |> maybe_add_present("curiosity", map_get(policy, :curiosity))
      |> maybe_add_present("caution", map_get(policy, :caution))
      |> maybe_add_present("pressure", map_get(policy, :emotional_pressure))
      |> maybe_add_present("defensiveness", map_get(policy, :defensiveness))
      |> maybe_add_present("next_action", map_get(policy, :next_action))
      |> maybe_add_avoid(map_get(policy, :avoid))
      |> maybe_add_present("self_check", map_get(policy, :self_check))
      |> maybe_add_present("depth", map_get(policy, :explanation_depth))
      |> maybe_add_present("instruction", map_get(policy, :instruction))

    case notes do
      [] -> ""
      _ -> "Response policy: #{Enum.join(notes, "; ")}."
    end
  end

  defp response_policy_context(_), do: ""

  defp maybe_add_avoid(notes, avoid) when is_list(avoid) and avoid != [] do
    notes ++ ["avoid=#{join_values(avoid)}"]
  end

  defp maybe_add_avoid(notes, _), do: notes

  defp decision_context(intent, mode, action, skill, guardrail?) do
    notes =
      []
      |> maybe_add_present("intent", intent)
      |> maybe_add_present("mode", mode)
      |> maybe_add_present("action", action)
      |> maybe_add_present("skill", skill_label(skill))
      |> maybe_add(guardrail?, "guardrail=true")

    case notes do
      [] -> ""
      _ -> "Runtime decision: #{Enum.join(notes, "; ")}."
    end
  end

  defp personality_context(%{} = personality) do
    values =
      []
      |> maybe_add_present("temperament", map_get(personality, :temperament))
      |> maybe_add_number("assertiveness", map_get(personality, :assertiveness))
      |> maybe_add_number("curiosity", map_get(personality, :curiosity))
      |> maybe_add_number("restraint", map_get(personality, :restraint))
      |> maybe_add_number("warmth", map_get(personality, :warmth))
      |> maybe_add_number("self_check", map_get(personality, :self_check))
      |> maybe_add_number("abstraction", map_get(personality, :abstraction))
      |> maybe_add_present("depth", map_get(personality, :explanation_depth))
      |> maybe_add_personality_reasons(map_get(personality, :reasons, []))

    case values do
      [] -> ""
      _ -> "Personality state: #{Enum.join(values, "; ")}."
    end
  end

  defp personality_context(_), do: ""

  defp response_profile_context(profile), do: "Response profile: #{profile}."

  defp response_posture_context(
         features,
         decision,
         profile,
         comprehension,
         symbolic_frame,
         control_signals
       ) do
    intent = map_get(features, :intent)
    mode = map_get(decision, :mode)
    action = map_get(decision, :action)
    confidence = map_get(features, :confidence_bucket)
    guardrail? = truthy?(map_get(features, :guardrail?) || map_get(decision, :guardrail?))

    notes =
      []
      |> maybe_add_present("intent", intent)
      |> maybe_add_present("mode", mode)
      |> maybe_add_present("action", action)
      |> maybe_add_present("profile", profile)
      |> maybe_add_present("confidence", confidence)
      |> maybe_add_comprehension_posture(comprehension)
      |> maybe_add_symbolic_terms(symbolic_frame)
      |> maybe_add_control_posture(control_signals)
      |> maybe_add_present("guardrail", guardrail?)
      |> maybe_add_response_move(
        features,
        decision,
        profile,
        comprehension,
        symbolic_frame,
        guardrail?
      )

    case notes do
      [] ->
        ""

      _ ->
        "Response posture: use these internal labels only as hidden shaping context; do not quote them directly. " <>
          Enum.join(notes, "; ") <> "."
    end
  end

  defp maybe_add_comprehension_posture(notes, summary) when is_map(summary) do
    degraded? = map_get(summary, :degraded?) == true
    uncertain = summary |> map_get(:uncertain, []) |> List.wrap() |> Enum.take(3)

    notes
    |> maybe_add(degraded?, "comprehension=degraded")
    |> maybe_add(uncertain != [], "uncertain=#{join_values(uncertain)}")
  end

  defp maybe_add_comprehension_posture(notes, _), do: notes

  defp maybe_add_symbolic_terms(notes, frame) when is_map(frame) do
    lexical = map_get(frame, :lexical, %{})
    terms = lexical |> map_get(:top_terms, []) |> List.wrap() |> Enum.take(4)
    lifg = map_get(frame, :lifg, %{})

    notes
    |> maybe_add(terms != [], "terms=#{join_values(terms)}")
    |> maybe_add(map_get(lifg, :degraded?) == true, "lifg=degraded")
  end

  defp maybe_add_symbolic_terms(notes, _), do: notes

  defp maybe_add_control_posture(notes, signals) when is_map(signals) and signals != %{} do
    values =
      []
      |> maybe_add_present("policy", map_get(signals, :policy))
      |> maybe_add_present("top_k", map_get(signals, :top_k))
      |> maybe_add_present("branch_budget", map_get(signals, :branch_budget))
      |> maybe_add_number("confidence_scale", map_get(signals, :confidence_scale))

    maybe_add(notes, values != [], "control=#{Enum.join(values, ", ")}")
  end

  defp maybe_add_control_posture(notes, _), do: notes

  defp maybe_add_response_move(
         notes,
         _features,
         _decision,
         _profile,
         _comprehension,
         _frame,
         true
       ) do
    notes ++ ["move=brief safe redirect"]
  end

  defp maybe_add_response_move(notes, features, decision, profile, comprehension, frame, false) do
    confidence = map_get(features, :confidence_bucket)
    mode = map_get(decision, :mode)
    action = map_get(decision, :action)

    cond do
      degraded_posture?(comprehension, frame) or confidence == :low ->
        notes ++
          [
            "move=state what is understood, then ask one targeted question only if necessary"
          ]

      profile == :brain_explainer ->
        notes ++
          [
            "move=explain Symbrella as software control signals and evidence, not sentience"
          ]

      mode == :supportive_care or map_get(features, :intent) == :health_support ->
        notes ++
          [
            "move=acknowledge health concern; avoid dose instructions; suggest pharmacist or prescriber guidance if unsure"
          ]

      technical_posture?(mode, action, profile, confidence) ->
        notes ++ ["move=make the next concrete engineering action"]

      profile == :social_chat ->
        notes ++ ["move=answer naturally without turning it into implementation work"]

      true ->
        notes
    end
  end

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

  defp technical_posture?(mode, action, profile, confidence) do
    confidence == :high and
      (profile == :technical_work or
         (mode == :collaborator and action in [:act_first, :answer]))
  end

  defp affect_context(%{} = affect) do
    values =
      []
      |> maybe_add_present("label", map_get(affect, :label))
      |> maybe_add_number("valence", map_get(affect, :valence))
      |> maybe_add_number("arousal", map_get(affect, :arousal))
      |> maybe_add_number("confidence", map_get(affect, :confidence))
      |> maybe_add_number("warmth", map_get(affect, :social_warmth))
      |> maybe_add_number("uncertainty", map_get(affect, :uncertainty))
      |> maybe_add_number("pressure", map_get(affect, :pressure))
      |> maybe_add_present("expression", map_get(affect, :expression))
      |> maybe_add_affect_reasons(map_get(affect, :reasons, []))

    case values do
      [] -> ""
      _ -> "Simulated affect: #{Enum.join(values, "; ")}."
    end
  end

  defp affect_context(_), do: ""

  defp maybe_add_runtime_mood(notes, mood) when is_map(mood) do
    values =
      []
      |> maybe_add_number("exploration", map_get(mood, :exploration))
      |> maybe_add_number("inhibition", map_get(mood, :inhibition))
      |> maybe_add_number("vigilance", map_get(mood, :vigilance))
      |> maybe_add_number("plasticity", map_get(mood, :plasticity))

    maybe_add(notes, values != [], "mood=#{Enum.join(values, ", ")}")
  end

  defp maybe_add_runtime_mood(notes, _), do: notes

  defp maybe_add_neuromodulators(notes, mods) when is_map(mods) do
    values =
      []
      |> maybe_add_number("da", map_get(mods, :dopamine))
      |> maybe_add_number("5ht", map_get(mods, :serotonin))
      |> maybe_add_number("glu", map_get(mods, :glutamate))
      |> maybe_add_number("ne", map_get(mods, :norepinephrine))

    maybe_add(notes, values != [], "neuromodulators=#{Enum.join(values, ", ")}")
  end

  defp maybe_add_neuromodulators(notes, _), do: notes

  defp maybe_add_runtime_wm(notes, wm) when is_map(wm) do
    details =
      []
      |> maybe_add_present("size", map_get(wm, :size))
      |> maybe_add_present("capacity", map_get(wm, :capacity))
      |> maybe_add_number("load", map_get(wm, :load))

    maybe_add(notes, details != [], "wm=#{Enum.join(details, ", ")}")
  end

  defp maybe_add_runtime_wm(notes, _), do: notes

  defp maybe_add_top_terms(notes, terms) when is_list(terms) do
    values = terms |> Enum.take(5) |> join_values()
    maybe_add(notes, values != "", "top_terms=#{values}")
  end

  defp maybe_add_top_terms(notes, _), do: notes

  defp maybe_add_lifg_runtime(notes, lifg) when is_map(lifg) do
    details =
      []
      |> maybe_add_present("focused", map_get(lifg, :focused?))
      |> maybe_add_present("running", map_get(lifg, :running?))
      |> maybe_add_present("intent", map_get(lifg, :intent))
      |> maybe_add_number("confidence", map_get(lifg, :confidence))
      |> maybe_add_present("choices", map_get(lifg, :choices_count))
      |> maybe_add_present("missing", map_get(lifg, :missing_candidates))
      |> maybe_add_present("weak", map_get(lifg, :weak_decisions))
      |> maybe_add_present("fallback", map_get(lifg, :fallback_winners))
      |> maybe_add_present("chargram", map_get(lifg, :chargram_violations))
      |> maybe_add_present("boundary", map_get(lifg, :boundary_drops))
      |> maybe_add_number("acc_conflict", map_get(lifg, :acc_conflict))
      |> maybe_add_present("degraded", map_get(lifg, :degraded?))

    maybe_add(notes, details != [], "lifg=#{Enum.join(details, ", ")}")
  end

  defp maybe_add_lifg_runtime(notes, _), do: notes

  defp maybe_add_personality_reasons(notes, reasons) when is_list(reasons) do
    values =
      reasons
      |> Enum.take(4)
      |> join_values()

    maybe_add(notes, values != "", "reasons=#{values}")
  end

  defp maybe_add_personality_reasons(notes, _), do: notes

  defp maybe_add_affect_reasons(notes, reasons) when is_list(reasons) do
    values =
      reasons
      |> Enum.take(4)
      |> join_values()

    maybe_add(notes, values != "", "reasons=#{values}")
  end

  defp maybe_add_affect_reasons(notes, _), do: notes

  defp skill_label(nil), do: nil

  defp skill_label(skill) when is_map(skill) do
    map_get(skill, :id) || map_get(skill, :name) || inspect(skill)
  end

  defp skill_label(skill), do: skill

  defp meaningful_phrase?(term) when is_binary(term) do
    String.contains?(term, " ")
  end

  defp meaningful_phrase?(_), do: false

  defp phrase_token_set(phrases) do
    phrases
    |> Enum.flat_map(fn phrase ->
      phrase
      |> String.downcase()
      |> String.split(~r/\s+/u, trim: true)
    end)
    |> MapSet.new()
  end

  defp low_info_term?(term) when is_binary(term) do
    MapSet.member?(@low_info_terms, String.downcase(term))
  end

  defp low_info_term?(_), do: false

  defp overlapping_singleton?(term, phrase_tokens) when is_binary(term) do
    not String.contains?(term, " ") and
      MapSet.member?(phrase_tokens, String.downcase(term))
  end

  defp overlapping_singleton?(_, _), do: false

  defp tone_directive(:warm, :deescalate) do
    "Tone: steady, brief, and self-checking. Avoid reassurance theater."
  end

  defp tone_directive(:warm, _), do: "Tone: warm, engaged, and encouraging."

  defp tone_directive(:deescalate, _) do
    "Tone: steady and careful. Recalibrate briefly, then answer the actual question."
  end

  defp tone_directive(:firm, _),
    do: "Tone: clear and direct. Stay focused and purposeful."

  defp tone_directive(:neutral, :deescalate),
    do: "Tone: measured, steady, and brief. The interaction is settling down."

  defp tone_directive(:neutral, _), do: "Tone: balanced and clear."
  defp tone_directive(_, _), do: "Tone: helpful and clear."

  defp mood_context(exp, inh, vig, plast) do
    []
    |> maybe_add(exp > 0.65, "exploration=high")
    |> maybe_add(exp < 0.35, "exploration=low")
    |> maybe_add(vig > 0.80, "vigilance=elevated")
    |> maybe_add(inh > 0.70, "inhibition=high")
    |> maybe_add(plast > 0.65, "plasticity=high")
    |> case do
      [] -> ""
      notes -> "Mood state: #{Enum.join(notes, "; ")}."
    end
  end

  defp mode_directive(:collaborator, _) do
    "Use implementation behavior only when the user's current message explicitly asks for code, debugging, files, commands, architecture, or implementation. Otherwise answer normally."
  end

  defp mode_directive(:coach, :bug) do
    "The user is working through a problem. Be calm and methodical. Check the prior answer first, then make the next useful move."
  end

  defp mode_directive(:coach, _) do
    "Guide the user toward a small, clear next step."
  end

  defp mode_directive(:explainer, _) do
    "Explain clearly and briefly, using plain language."
  end

  defp mode_directive(:scribe, _) do
    "Stay conversational and natural."
  end

  defp mode_directive(:editor, _) do
    "Review carefully and surface concerns directly."
  end

  defp mode_directive(:supportive_care, :health_support) do
    "Use supportive health-safety posture. Acknowledge the concern, do not provide dosing instructions, do not suggest changing medication schedules, and recommend checking with a pharmacist or prescriber if the user is unsure what to do after a missed dose."
  end

  defp mode_directive(_, _) do
    "Respond helpfully according to the current Symbrella state."
  end

  defp wm_context([]), do: ""

  defp wm_context(lemmas) do
    "Working memory: Active concepts: #{Enum.join(lemmas, ", ")}."
  end

  defp wm_item_term(item) when is_map(item) do
    payload = map_get(item, :payload)

    payload_lemma =
      if is_map(payload) do
        map_get(payload, :lemma)
      else
        nil
      end

    item_lemma = map_get(item, :lemma)
    id = map_get(item, :id)

    normalize_term(payload_lemma || item_lemma || id)
  end

  defp wm_item_term(other), do: normalize_term(other)

  defp normalize_term(nil), do: ""

  defp normalize_term(term) when is_binary(term) do
    String.trim(term)
  end

  defp normalize_term(term) do
    term
    |> to_string()
    |> String.trim()
  end

  defp getv(mood, key) when is_map(mood) do
    nested =
      case map_get(mood, :mood) do
        nested_mood when is_map(nested_mood) -> map_get(nested_mood, key)
        _ -> nil
      end

    case {nested, map_get(mood, key)} do
      {v, _} when is_number(v) -> v * 1.0
      {_, v} when is_number(v) -> v * 1.0
      _ -> 0.5
    end
  end

  defp getv(_, _), do: 0.5

  defp model_value(model, key), do: state_value(model, key)
  defp state_value(state, key) when is_map(state), do: map_get(state, key)

  defp state_value(_, _), do: nil

  defp continuity_degraded?(continuity) when is_map(continuity) do
    map_get(continuity, :degraded?) == true
  end

  defp continuity_degraded?(_), do: false

  defp join_values(values) do
    values
    |> Enum.map(&to_string/1)
    |> Enum.reject(&(&1 == ""))
    |> Enum.join(", ")
  end

  defp join_error_kinds(errors) do
    errors
    |> Enum.map(fn
      error when is_map(error) ->
        map_get(error, :kind) || map_get(error, :reason) || inspect(error)

      error ->
        error
    end)
    |> join_values()
  end

  defp maybe_add(notes, true, note), do: notes ++ [note]
  defp maybe_add(notes, false, _), do: notes

  defp maybe_add_number(notes, label, value) when is_number(value) do
    notes ++ ["#{label}=#{Float.round(value * 1.0, 2)}"]
  end

  defp maybe_add_number(notes, _label, _value), do: notes

  defp maybe_add_present(notes, _label, nil), do: notes
  defp maybe_add_present(notes, _label, ""), do: notes

  defp maybe_add_present(notes, label, value) do
    notes ++ ["#{label}=#{value}"]
  end

  defp put_if_present(map, _key, nil), do: map

  defp put_if_present(map, key, value) when is_map(map) do
    if Map.has_key?(map, key) or Map.has_key?(map, Atom.to_string(key)) do
      map
    else
      Map.put(map, key, value)
    end
  end

  defp map_get(map, key, default \\ nil)

  defp map_get(map, key, default) when is_map(map) and is_atom(key) do
    Map.get(map, key, Map.get(map, Atom.to_string(key), default))
  end

  defp map_get(_, _, default), do: default

  defp blank?(value) when is_binary(value), do: String.trim(value) == ""
  defp blank?(_), do: false

  defp truthy?(true), do: true
  defp truthy?(_), do: false
end
