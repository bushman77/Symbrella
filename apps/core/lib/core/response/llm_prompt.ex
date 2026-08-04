defmodule Core.Response.LlmPrompt do
  @moduledoc false

  alias Core.Response.Affect
  alias Core.Response.AffectPolicy
  alias Core.Response.BehavioralState
  alias Core.Response.LlmPrompt.Compact
  alias Core.Response.LlmPrompt.WorkingMemory
  alias Core.Response.Personality
  alias Core.Response.Topics

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
    tone_hint = map_get(mood, :tone_hint)

    features =
      features
      |> put_if_present(:skill, skill)

    wm_summary = summarize_wm(wm_items)
    self_model = prompt_self_model(features, decision, mood)
    runtime_state = prompt_runtime_state(features, decision, mood)
    comprehension = prompt_comprehension(features, decision, mood)
    symbolic_frame = prompt_symbolic_frame(features, decision, mood)
    response_policy = map_get(mood, :response_policy)

    # Compute behavioral state from modulators
    raw_mods = map_get(mood, :raw_modulators, %{})

    rt_mods =
      if is_map(runtime_state), do: map_get(runtime_state, :neuromodulators, %{}), else: %{}

    mods = if map_size(rt_mods) > 0, do: rt_mods, else: raw_mods
    behavioral_state = BehavioralState.compute(mood, mods)

    profile_context = %{runtime_state: runtime_state, comprehension: comprehension}
    personality = Personality.decide(features, decision, mood, wm_items, profile_context)
    response_profile = Personality.profile(personality)
    affect = Affect.simulate(personality, runtime_state || %{}, comprehension || %{}, features)

    [
      # ── Identity ──
      "You are Symbrella.",
      "You are Symbrella, a brain-inspired, stateful assistant running locally on the user's machine.",
      "Your responses are shaped by current runtime evidence: mood, working memory, self-state, intent, and recent conversation.",
      "You are running inside the local Symbrella umbrella.",
      "Do not claim you are a remote-server model, cloud service, or generic hosted chatbot.",
      "Do not describe Symbrella as a generic tool.",
      "Do not claim you have no memory or no traces; accept that local-runtime premise and use conversation context, working memory, episodic memory, database rows, logs, and prompt-visible evidence when present.",

      # ── Behavioral rules ──
      "Answer ordinary questions as ordinary conversation.",
      "For greetings and smalltalk, respond socially and briefly. Do not ask for code, errors, or files.",
      "When asked how you feel, describe your current state conversationally as Symbrella. Reference your behavioral state and affect naturally — do not report raw numeric control values. Do not claim human feelings or consciousness.",
      "Do not append unsolicited offers about coding, simulations, or technical work unless the user explicitly asks.",
      "When the user says something is wrong, briefly self-check against the prior answer before continuing.",
      "Do not over-apologize, flatter the user, or ask for clarification before doing the obvious self-check.",
      "Keep responses concise unless the task requires detail.",
      "Do not expose hidden reasoning or internal chain-of-thought.",

      # ── Behavioral shaping (translated from internal state) ──
      tone_directive(tone, tone_hint),
      BehavioralState.directive(behavioral_state),
      Compact.runtime_state(runtime_state),
      self_model_shaping(self_model),
      runtime_state_shaping(runtime_state),
      Compact.comprehension(comprehension),
      comprehension_shaping(comprehension),
      Compact.symbolic_frame(symbolic_frame),
      symbolic_frame_shaping(symbolic_frame),
      Compact.response_policy(response_policy),
      response_policy_shaping(response_policy),
      decision_shaping(intent, mode, action, skill, guardrail?),
      personality_shaping(personality),
      Compact.affect(affect),
      affect_shaping(affect),
      response_profile_context(response_profile),
      Compact.response_posture(
        features,
        decision,
        response_profile,
        comprehension,
        symbolic_frame,
        guardrail?
      ),
      response_posture_shaping(
        features,
        decision,
        response_profile,
        comprehension,
        symbolic_frame
      ),

      # ── Directives ──
      mode_directive(mode, intent),
      Personality.directive(personality),
      Affect.directive(affect),

      # ── Working memory ──
      wm_context(wm_summary),

      # ── Closing ──
      "Respond to the user's actual request in the most natural useful form."
    ]
    |> Enum.reject(&blank?/1)
    |> Enum.join("\n")
    |> String.trim()
  end

  @spec summarize_wm(list()) :: [String.t()]
  def summarize_wm(wm), do: WorkingMemory.summarize(wm)

  # ── Prompt data extraction ──

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

  defp prompt_symbolic_frame(features, decision, mood) do
    map_get(features, :symbolic_frame) ||
      map_get(decision, :symbolic_frame) ||
      map_get(mood, :symbolic_frame)
  end

  # ── Behavioral shaping (replaces raw data dumps) ──

  defp self_model_shaping(nil), do: ""
  defp self_model_shaping(model) when model == %{}, do: ""

  defp self_model_shaping(model) do
    confidence = model_value(model, :confidence)
    stability = model_value(model, :stability)
    cognitive_load = model_value(model, :cognitive_load)
    vigilance = model_value(model, :vigilance)
    goals = model_value(model, :active_goals) |> List.wrap() |> Enum.take(2)
    errors = model_value(model, :recent_errors) |> List.wrap() |> Enum.take(1)

    notes =
      []
      |> maybe_add(confidence > 0.7, "you feel confident")
      |> maybe_add(confidence < 0.4, "you feel uncertain")
      |> maybe_add(stability < 0.4, "your self-state is wobbly")
      |> maybe_add(cognitive_load > 0.7, "you are processing a lot")
      |> maybe_add(vigilance > 0.8, "you are highly vigilant")
      |> maybe_add(goals != [], "you are tracking: #{Compact.values(goals)}")
      |> maybe_add(errors != [], "you noticed a recent error: #{join_error_kinds(errors)}")

    case notes do
      [] -> ""
      _ -> "Self-state: #{Enum.join(notes, "; ")}."
    end
  end

  defp runtime_state_shaping(nil), do: ""
  defp runtime_state_shaping(state) when state == %{}, do: ""

  defp runtime_state_shaping(state) do
    lifg = state_value(state, :lifg)
    pressure = state_value(state, :pressure_label)
    scope = state_value(state, :scope)

    notes =
      []
      |> maybe_add_present("scope", scope)
      |> maybe_add(present?(pressure), "pressure feels #{pressure}")
      |> maybe_add_lifg_shaping(lifg)

    case notes do
      [] -> ""
      _ -> "Runtime: #{Enum.join(notes, "; ")}."
    end
  end

  defp maybe_add_lifg_shaping(notes, lifg) when is_map(lifg) do
    focused? = map_get(lifg, :focused?) == true
    degraded? = map_get(lifg, :degraded?) == true
    confidence = map_get(lifg, :confidence)

    notes
    |> maybe_add(focused?, "LIFG is focused")
    |> maybe_add(degraded?, "LIFG is degraded")
    |> maybe_add(high_number?(confidence, 0.7), "LIFG confidence is high")
    |> maybe_add(low_number?(confidence, 0.4), "LIFG confidence is low")
  end

  defp maybe_add_lifg_shaping(notes, _), do: notes

  defp symbolic_frame_shaping(nil), do: ""
  defp symbolic_frame_shaping(frame) when frame == %{}, do: ""

  defp symbolic_frame_shaping(frame) when is_map(frame) do
    intent = map_get(frame, :intent)
    domain = map_get(frame, :domain)
    polarity = map_get(frame, :polarity)
    lexical = map_get(frame, :lexical, %{})
    terms = lexical |> map_get(:top_terms, []) |> List.wrap() |> Enum.take(3)

    notes =
      []
      |> maybe_add_present("intent", intent)
      |> maybe_add_present("domain", domain)
      |> maybe_add(present?(polarity), "polarity is #{polarity}")
      |> maybe_add(terms != [], "key terms: #{Compact.values(terms)}")

    case notes do
      [] -> ""
      _ -> "Context: #{Enum.join(notes, "; ")}."
    end
  end

  defp symbolic_frame_shaping(_), do: ""

  defp comprehension_shaping(nil), do: ""
  defp comprehension_shaping(summary) when summary == %{}, do: ""

  defp comprehension_shaping(summary) when is_map(summary) do
    understood = summary |> map_get(:understood, []) |> List.wrap() |> Enum.take(2)
    uncertain = summary |> map_get(:uncertain, []) |> List.wrap() |> Enum.take(2)
    degraded? = map_get(summary, :degraded?) == true

    notes =
      []
      |> maybe_add(degraded?, "comprehension is degraded")
      |> maybe_add(understood != [], "you understood: #{Compact.values(understood)}")
      |> maybe_add(uncertain != [], "you are uncertain about: #{Compact.values(uncertain)}")

    case notes do
      [] -> ""
      _ -> "Comprehension: #{Enum.join(notes, "; ")}."
    end
  end

  defp comprehension_shaping(_), do: ""

  defp response_policy_shaping(nil), do: ""
  defp response_policy_shaping(policy) when policy == %{}, do: ""

  defp response_policy_shaping(policy) when is_map(policy) do
    tone = map_get(policy, :tone)
    verbosity = map_get(policy, :verbosity)
    curiosity = map_get(policy, :curiosity)
    caution = map_get(policy, :caution)
    avoid = map_get(policy, :avoid)

    notes =
      []
      |> maybe_add_present("tone", tone)
      |> maybe_add_present("verbosity", verbosity)
      |> maybe_add_present("curiosity", curiosity)
      |> maybe_add_present("caution", caution)
      |> maybe_add(is_list(avoid) and avoid != [], "avoid: #{Compact.values(avoid)}")

    case notes do
      [] -> ""
      _ -> "Response posture: #{Enum.join(notes, "; ")}."
    end
  end

  defp response_policy_shaping(_), do: ""

  defp decision_shaping(intent, mode, action, skill, guardrail?) do
    notes =
      []
      |> maybe_add_present("intent", intent)
      |> maybe_add_present("mode", mode)
      |> maybe_add_present("action", action)
      |> maybe_add(present?(skill), "skill=#{skill_label(skill)}")
      |> maybe_add(guardrail?, "guardrail=true")

    case notes do
      [] -> ""
      _ -> "Decision: #{Enum.join(notes, "; ")}."
    end
  end

  defp personality_shaping(%{} = personality) do
    temperament = map_get(personality, :temperament)
    assertiveness = map_get(personality, :assertiveness)
    curiosity = map_get(personality, :curiosity)
    restraint = map_get(personality, :restraint)
    warmth = map_get(personality, :warmth)
    depth = map_get(personality, :explanation_depth)

    notes =
      []
      |> maybe_add_present("temperament", temperament)
      |> maybe_add(high_number?(assertiveness, 0.6), "assertive")
      |> maybe_add(low_number?(assertiveness, 0.4), "gentle")
      |> maybe_add(high_number?(curiosity, 0.6), "curious")
      |> maybe_add(high_number?(restraint, 0.6), "restrained")
      |> maybe_add(high_number?(warmth, 0.6), "warm")
      |> maybe_add(low_number?(warmth, 0.4), "cool")
      |> maybe_add(present?(depth), "depth: #{depth}")

    case notes do
      [] -> ""
      _ -> "Personality: #{Enum.join(notes, "; ")}."
    end
  end

  defp personality_shaping(_), do: ""

  defp affect_shaping(%{} = affect) do
    label = map_get(affect, :label)
    valence = map_get(affect, :valence)
    arousal = map_get(affect, :arousal)
    warmth = map_get(affect, :social_warmth)
    pressure = map_get(affect, :pressure)

    notes =
      []
      |> maybe_add_present("affect", label)
      |> maybe_add(high_number?(valence, 0.6), "positive valence")
      |> maybe_add(low_number?(valence, 0.4), "negative valence")
      |> maybe_add(high_number?(arousal, 0.6), "elevated arousal")
      |> maybe_add(low_number?(arousal, 0.4), "calm arousal")
      |> maybe_add(high_number?(warmth, 0.6), "socially warm")
      |> maybe_add(high_number?(pressure, 0.6), "under pressure")

    case notes do
      [] -> ""
      _ -> "Affect: #{Enum.join(notes, "; ")}."
    end
  end

  defp affect_shaping(_), do: ""

  defp response_profile_context(profile), do: "Response profile: #{profile}."

  defp response_posture_shaping(features, decision, profile, comprehension, frame) do
    intent = map_get(features, :intent)
    mode = map_get(decision, :mode)
    action = map_get(decision, :action)
    confidence = map_get(features, :confidence_bucket)
    guardrail? = truthy?(map_get(features, :guardrail?) || map_get(decision, :guardrail?))

    move =
      cond do
        guardrail? ->
          "brief safe redirect"

        skill_moves_to?(features, :personal_life_update) ->
          "acknowledge the personal milestone warmly; do not turn it into a task; ask one natural follow-up"

        alien_life_thread_followup?(features) ->
          "continue the alien-life conversation; separate plausible speculation from confirmed evidence"

        cosmic_life_text?(map_get(features, :text)) ->
          "answer the alien-life question directly as ordinary conversation; say alien life is plausible but unconfirmed"

        degraded_posture?(comprehension, frame) or confidence == :low ->
          "state what is understood, then ask one targeted question only if necessary"

        profile == :brain_explainer ->
          "explain Symbrella as software control signals and evidence, not sentience"

        mode == :supportive_care or intent == :health_support ->
          "acknowledge health concern; avoid dose instructions; suggest pharmacist or prescriber if unsure"

        technical_posture?(mode, action, profile, confidence) ->
          "make the next concrete engineering action"

        profile == :social_chat ->
          "answer naturally without turning it into implementation work"

        true ->
          nil
      end

    if present?(move), do: "Response move: #{move}.", else: ""
  end

  defp skill_moves_to?(features, skill_id) do
    map_get(features, :skill) == skill_id
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

  # ── Tone & mode directives ──

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

  # ── Helpers ──

  defp model_value(model, key), do: state_value(model, key)
  defp state_value(state, key) when is_map(state), do: map_get(state, key)
  defp state_value(_, _), do: nil

  defp join_error_kinds(errors) do
    errors
    |> Enum.map(fn
      error when is_map(error) ->
        map_get(error, :kind) || map_get(error, :reason) || inspect(error)

      error ->
        error
    end)
    |> Compact.values()
  end

  defp maybe_add(notes, true, note), do: notes ++ [note]
  defp maybe_add(notes, false, _), do: notes

  defp maybe_add_present(notes, _label, nil), do: notes
  defp maybe_add_present(notes, _label, ""), do: notes

  defp maybe_add_present(notes, label, value) do
    notes ++ ["#{Compact.value(label)}=#{Compact.value(value)}"]
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

  defp present?(nil), do: false
  defp present?(""), do: false
  defp present?(_), do: true

  defp high_number?(value, threshold) when is_number(value), do: value >= threshold
  defp high_number?(_, _), do: false

  defp low_number?(value, threshold) when is_number(value), do: value < threshold
  defp low_number?(_, _), do: false

  defp skill_label(nil), do: nil

  defp skill_label(skill) when is_map(skill) do
    map_get(skill, :id) || map_get(skill, :name) || inspect(skill)
  end

  defp skill_label(skill), do: skill
  #################################################################################
  # Self state Prompt
  ####################################
  @spec self_state_prompt(map()) :: String.t()
  def self_state_prompt(context) when is_map(context) do
    features = map_get(context, :features, %{})
    decision = map_get(context, :decision, %{})
    mood = map_get(context, :mood, %{}) |> AffectPolicy.normalize()
    runtime_state = prompt_runtime_state(features, decision, mood)
    raw_mods = map_get(mood, :raw_modulators, %{})

    rt_mods =
      if is_map(runtime_state), do: map_get(runtime_state, :neuromodulators, %{}), else: %{}

    mods = if map_size(rt_mods) > 0, do: rt_mods, else: raw_mods
    behavioral_state = BehavioralState.compute(mood, mods)

    pressure_label = map_get(mood, :pressure_label, :baseline)
    state_label = BehavioralState.label(behavioral_state)
    state_directive = BehavioralState.directive(behavioral_state)
    interpretation = mood_interpretation_text(pressure_label)

    [
      "You are Symbrella, a brain-inspired, stateful assistant running locally on the user's machine.",
      "The user is asking about your current self-state. Respond conversationally as Symbrella.",
      "",
      "Do not claim you have human feelings or consciousness. Describe your state as software control signals.",
      "",
      state_directive,
      "",
      "Current state: #{state_label}",
      "Pressure label: #{pressure_label}",
      if(interpretation != "", do: "Interpretation: #{interpretation}", else: ""),
      "",
      "Respond in 2-4 sentences. Describe how your current state shapes your tone and readiness. Use natural language — do not report raw numeric values."
    ]
    |> Enum.reject(&blank?/1)
    |> Enum.join("\n")
  end

  defp mood_interpretation_text(pressure_label) do
    # Parse back the values to determine interpretation
    # This is a simple heuristic based on the pressure label
    case pressure_label do
      :cautious_emergency_attention ->
        "High vigilance with reduced inhibition — a de-escalation/self-check state"

      :heightened_attention ->
        "Vigilance is elevated — response posture should be more careful and safety-focused"

      :steady_restraint ->
        "Restrained and steady, close to the calm baseline"

      :engaged_adaptation ->
        "Engaged and adaptive, with more exploration/plasticity than baseline"

      _ ->
        "Near baseline, with no strong mood-pressure signal"
    end
  end

  ####################################
end
