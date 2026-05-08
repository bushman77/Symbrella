defmodule Core.Response do
  @moduledoc """
  Response — small policy engine that maps (intent × mood × text) → {tone, text, meta}.

  Public API:
    • plan/2 — given an SI-like map and optional mood-like map, decide tone/mode/action,
      optionally fire one micro-skill (inline text), and return {tone, text, meta}.
    • annotate_si/2 — convenience helper that runs plan/2 and attaches
      :response_tone, :response_text, :response_meta onto an SI-like map/struct.

  Children live under `Core.Response.*` (Policy, Modes, Guardrails, Skills, LlmSynthesis).
  Emits telemetry: [:core, :response, :plan].
  """

  @type si_like :: %{
          optional(:intent) => atom(),
          optional(:keyword) => any(),
          optional(:confidence) => number(),
          optional(:text) => String.t(),
          optional(:session_id) => term(),
          optional(:session) => term(),
          optional(:conversation_id) => term()
        }

  @type mood_like :: %{
          optional(:mood) => %{
            optional(:exploration) => number(),
            optional(:inhibition) => number(),
            optional(:vigilance) => number(),
            optional(:plasticity) => number()
          },
          optional(:tone_hint) => atom()
        }

  alias Core.Response.Policy
  alias Core.Response.Modes
  alias Core.Response.Guardrails
  alias Core.Response.Skills
  alias Core.Response.LlmSynthesis
  alias Core.Response.SelfStateSummary

  @compile {:no_warn_undefined, Brain.MoodCore}

  # ────────────────────────────────────────────────────────────────────────────
  # Public API
  # ────────────────────────────────────────────────────────────────────────────

  @spec plan(si_like(), mood_like()) :: {atom(), String.t(), map()}
  def plan(si, mood \\ %{})

  def plan(si, mood) when is_map(si) and is_map(mood) do
    intent0 = Map.get(si, :intent, :unknown)
    conf = clamp01(Map.get(si, :confidence, 0.0))
    text_in = si_text(si)
    session_id = session_id(si)
    extracted_name = extract_user_name(text_in)
    remember_fact = extract_remember_fact(text_in)
    direct_fact = extract_direct_fact(text_in)
    fact_query_key = extract_fact_query_key(text_in)

    case memory_reply_for(%{
           intent0: intent0,
           conf: conf,
           text_in: text_in,
           session_id: session_id,
           extracted_name: extracted_name,
           remember_fact: remember_fact,
           direct_fact: direct_fact,
           fact_query_key: fact_query_key
         }) do
      {_, _, _} = reply ->
        reply

      nil ->
        intent = Policy.normalize_intent(intent0, text_in)
        _ = maybe_apply_mood_intent(intent, conf, text_in)

        {vig, inh, exp, pls, tone_hint} = mood_sample(mood)
        guard = Guardrails.detect(text_in)

        benign? = Policy.benign_text?(text_in)
        hostile? = Policy.hostile_text?(text_in)
        command? = Policy.command?(text_in)

        confidence_bucket = bucket_confidence(conf)
        vigilance_bucket = bucket_vigilance(vig)
        risk_bucket = if guard.guardrail? or intent == :illicit_request, do: :high, else: :low

        features =
          build_features(%{
            session_id: session_id,
            intent0: intent0,
            intent: intent,
            text_in: text_in,
            conf: conf,
            confidence_bucket: confidence_bucket,
            vig: vig,
            inh: inh,
            exp: exp,
            pls: pls,
            vigilance_bucket: vigilance_bucket,
            tone_hint: tone_hint,
            benign?: benign?,
            hostile?: hostile?,
            command?: command?,
            risk_bucket: risk_bucket,
            guard: guard,
            extracted_name: extracted_name,
            comprehension: Map.get(si, :comprehension),
            prefrontal: Map.get(si, :prefrontal),
            control_signals: Map.get(si, :control_signals)
          })

        {decision, skill} = decide_and_pick_skill(features, guard, text_in)

        name_claim? = name_claim?(extracted_name, text_in)
        maybe_persist_user_name(name_claim?, extracted_name, text_in)

        forced_identity = forced_identity_text(text_in, extracted_name, name_claim?)

        text0 =
          forced_identity ||
            inline_skill_text(skill) ||
            llm_or_template(text_in, features, decision, mood, intent)

        {text, curiosity_probe} =
          maybe_append_curiosity_question(text0, si, mood, features, decision, guard, skill)

        record_turn(session_id, text_in, text)

        profile = classify_profile(features, decision, guard)

        planner_explanation =
          build_planner_explanation(
            intent,
            conf,
            decision.tone,
            decision.mode,
            tone_hint,
            vig,
            inh,
            exp,
            benign?,
            hostile?,
            risk_bucket,
            decision.overrides
          )

        meta =
          build_meta(%{
            decision: decision,
            intent0: intent0,
            intent: intent,
            conf: conf,
            confidence_bucket: confidence_bucket,
            risk_bucket: risk_bucket,
            profile: profile,
            benign?: benign?,
            hostile?: hostile?,
            tone_hint: tone_hint,
            mood_sample: %{exploration: exp, inhibition: inh, vigilance: vig, plasticity: pls},
            skill: skill,
            guard: guard,
            session_id: session_id,
            extracted_name: extracted_name,
            planner_explanation: planner_explanation,
            comprehension: Map.get(features, :comprehension),
            prefrontal: Map.get(features, :prefrontal),
            control_signals: Map.get(features, :control_signals),
            curiosity_probe: curiosity_probe
          })

        :telemetry.execute([:core, :response, :plan], %{}, meta)

        {decision.tone, text, meta}
    end
  end

  def plan(_si, _mood), do: {:neutral, "", %{error: :invalid_args}}

  @spec memory_reply(si_like()) :: {atom(), String.t(), map()} | nil
  def memory_reply(si) when is_map(si) do
    intent0 = Map.get(si, :intent, :unknown)
    conf = clamp01(Map.get(si, :confidence, 0.0))
    text_in = si_text(si)
    session_id = session_id(si)
    extracted_name = extract_user_name(text_in)
    remember_fact = extract_remember_fact(text_in)
    direct_fact = extract_direct_fact(text_in)
    fact_query_key = extract_fact_query_key(text_in)

    memory_reply_for(%{
      intent0: intent0,
      conf: conf,
      text_in: text_in,
      session_id: session_id,
      extracted_name: extracted_name,
      remember_fact: remember_fact,
      direct_fact: direct_fact,
      fact_query_key: fact_query_key
    })
  end

  def memory_reply(_), do: nil

  # ─────────────────────────────────────────────────────────────────────────────
  # Helpers
  # ─────────────────────────────────────────────────────────────────────────────

  defp mood_sample(mood) do
    {
      getv(mood, :vigilance),
      getv(mood, :inhibition),
      getv(mood, :exploration),
      getv(mood, :plasticity),
      Map.get(mood, :tone_hint)
    }
  end

  defp maybe_apply_mood_intent(intent, confidence, text)
       when is_atom(intent) and is_number(confidence) do
    if Code.ensure_loaded?(Brain.MoodCore) and
         function_exported?(Brain.MoodCore, :apply_intent, 2) and
         not read_only_mood_query?(text) do
      Brain.MoodCore.apply_intent(intent, confidence)
    else
      :ok
    end
  end

  defp maybe_apply_mood_intent(_intent, _confidence, _text), do: :ok

  defp read_only_mood_query?(text), do: mood_indices_query?(text)

  defp build_features(%{
         session_id: session_id,
         intent0: intent0,
         intent: intent,
         text_in: text_in,
         conf: conf,
         confidence_bucket: confidence_bucket,
         vig: vig,
         inh: inh,
         exp: exp,
         pls: pls,
         vigilance_bucket: vigilance_bucket,
         tone_hint: tone_hint,
         benign?: benign?,
         hostile?: hostile?,
         command?: command?,
         risk_bucket: risk_bucket,
         guard: guard,
         extracted_name: extracted_name,
         comprehension: comprehension,
         prefrontal: prefrontal,
         control_signals: control_signals
       }) do
    %{
      session_id: session_id,
      intent_in: intent0,
      intent: intent,
      text: text_in,
      conf: conf,
      confidence_bucket: confidence_bucket,
      vig: vig,
      inh: inh,
      exp: exp,
      plast: pls,
      vigilance_bucket: vigilance_bucket,
      tone_hint: tone_hint,
      benign?: benign?,
      hostile?: hostile?,
      command?: command?,
      cooldown: 0,
      episode_bias: 0.0,
      guardrail?: guard.guardrail?,
      approve_token?: guard.approve_token?,
      risk_bucket: risk_bucket,
      guardrail_flags: guard.flags,
      user_name: extracted_name,
      comprehension: comprehension,
      prefrontal: prefrontal,
      control_signals: control_signals
    }
  end

  defp decide_and_pick_skill(features, guard, text_in) do
    decision0 = Policy.decide(features)
    {decision, forced_skill} = force_overrides(features, decision0, guard)
    skill = forced_skill || Skills.pick(text_in, features, decision)
    {decision, skill}
  end

  defp name_claim?(name, text_in) do
    is_binary(name) and name != "" and not asking_for_user_name?(text_in)
  end

  defp maybe_persist_user_name(true, name, text_in), do: persist_user_name_episode(name, text_in)
  defp maybe_persist_user_name(_, _name, _text_in), do: :ok

  defp forced_identity_text(text_in, extracted_name, name_claim?) do
    cond do
      asking_for_user_name?(text_in) ->
        name =
          normalize_name(extracted_name) ||
            ((Code.ensure_loaded?(Brain.Hippocampus) and
                function_exported?(Brain.Hippocampus, :fact, 1)) &&
               Brain.Hippocampus.fact(:user_name))

        if is_binary(name) and name != "" do
          "Your name is #{name}."
        else
          # IMPORTANT: return a string (non-nil) so we never fall through to LLM.
          "I don’t know your name yet—tell me “my name is …” and I’ll remember it."
        end

      name_claim? ->
        name = normalize_name(extracted_name)
        if name, do: "Nice to meet you, #{name}. I’ll remember that.", else: nil

      true ->
        nil
    end
  end

  defp normalize_name(name) when is_binary(name) do
    n =
      name
      |> String.trim()
      |> String.replace(~r/\s+/u, " ")

    if n == "", do: nil, else: n
  end

  defp normalize_name(_), do: nil

  defp llm_or_template(text_in, features, decision, mood, intent) do
    case LlmSynthesis.generate(text_in, features, decision, mood) do
      {:ok, llm_text} -> llm_text
      {:error, _} -> Modes.compose(intent, decision.tone, decision.mode)
    end
  end

  defp maybe_append_curiosity_question(text, si, mood, features, decision, guard, skill)
       when is_binary(text) do
    _ = {si, mood, features, decision, guard, skill}
    {text, nil}
  end

  defp maybe_append_curiosity_question(text, _si, _mood, _features, _decision, _guard, _skill),
    do: {text, nil}

  defp build_meta(%{
         decision: decision,
         intent0: intent0,
         intent: intent,
         conf: conf,
         confidence_bucket: confidence_bucket,
         risk_bucket: risk_bucket,
         profile: profile,
         benign?: benign?,
         hostile?: hostile?,
         tone_hint: tone_hint,
         mood_sample: mood_sample,
         skill: skill,
         guard: guard,
         session_id: session_id,
         extracted_name: extracted_name,
         planner_explanation: planner_explanation,
         curiosity_probe: curiosity_probe
       }) do
    %{
      policy_version: decision.policy_version,
      intent_inferred: intent,
      intent_original: intent0,
      confidence: conf,
      confidence_bucket: confidence_bucket,
      risk_bucket: risk_bucket,
      tone: decision.tone,
      mode: decision.mode,
      action: decision.action,
      profile: profile,
      benign: benign?,
      hostile: hostile?,
      tone_hint: tone_hint,
      mood_sample: mood_sample,
      scores: decision.scores,
      overrides: decision.overrides,
      chosen_skill: (skill && skill.id) || nil,
      skill_reason: (skill && skill.reason) || nil,
      guardrail?: guard.guardrail?,
      approve_token?: guard.approve_token?,
      guardrail_flags: guard.flags,
      session_id: session_id,
      user_name: extracted_name,
      explanation: planner_explanation,
      curiosity_probe: curiosity_probe
    }
  end

  # ─────────────────────────────────────────────────────────────────────────────
  # Durable name memory (Hippocampus)
  # ─────────────────────────────────────────────────────────────────────────────

  defp memory_reply_for(%{
         intent0: intent0,
         conf: conf,
         text_in: text_in,
         session_id: session_id,
         extracted_name: extracted_name,
         remember_fact: remember_fact,
         direct_fact: direct_fact,
         fact_query_key: fact_query_key
       }) do
    cond do
      asking_for_user_name?(text_in) ->
        name = recalled_user_name(extracted_name)

        text =
          if is_binary(name) and name != "" do
            "Your name is #{name}."
          else
            "I don’t know your name yet—tell me “my name is …” and I’ll remember it."
          end

        meta = %{
          action: :identity,
          session_id: session_id,
          user_name: name,
          source: :hippocampus_fact,
          intent_original: intent0,
          confidence: conf
        }

        finish_memory_reply(session_id, text_in, text, meta)

      remember_fact ->
        {key, label, value} = remember_fact
        text = remember_fact_response(key, label, value, text_in, true)

        meta = %{
          action: :remember_fact,
          session_id: session_id,
          fact_key: key,
          fact_label: label,
          source: :hippocampus_fact,
          intent_original: intent0,
          confidence: conf
        }

        finish_memory_reply(session_id, text_in, text, meta)

      direct_fact ->
        {key, label, value} = direct_fact
        text = remember_fact_response(key, label, value, text_in, false)

        meta = %{
          action: :remember_fact,
          session_id: session_id,
          fact_key: key,
          fact_label: label,
          source: :hippocampus_fact,
          intent_original: intent0,
          confidence: conf
        }

        finish_memory_reply(session_id, text_in, text, meta)

      fact_query_key ->
        {key, label} = fact_query_key
        value = recalled_fact(key)

        text =
          if is_binary(value) and value != "" do
            "Your #{label} is #{value}."
          else
            "I don’t know your #{label} yet—tell me “remember that my #{label} is …” and I’ll remember it."
          end

        meta = %{
          action: :recall_fact,
          session_id: session_id,
          fact_key: key,
          fact_label: label,
          source: :hippocampus_fact,
          intent_original: intent0,
          confidence: conf
        }

        finish_memory_reply(session_id, text_in, text, meta)

      true ->
        nil
    end
  end

  defp finish_memory_reply(session_id, text_in, text, meta) do
    record_turn(session_id, text_in, text)
    :telemetry.execute([:core, :response, :plan], %{}, meta)
    {:warm, text, meta}
  end

  defp persist_user_name_episode(name, raw_text) when is_binary(name) do
    if Code.ensure_loaded?(Brain.Hippocampus) and
         function_exported?(Brain.Hippocampus, :encode, 2) do
      slate = %{
        sentence: raw_text,
        winners: [%{lemma: name}],
        tokens: [name],
        tags: ["fact", "user_name"]
      }

      meta = %{
        tags: ["fact", "user_name"],
        scope: :chat,
        kind: :fact,
        key: :user_name,
        value: name
      }

      _ = Brain.Hippocampus.encode(slate, meta)
    end

    :ok
  end

  defp remember_fact_response(key, label, value, raw_text, explicit?)
       when is_binary(key) and is_binary(label) and is_binary(value) do
    persist_user_fact_episode(key, label, value, raw_text)

    if explicit? do
      "I’ll remember that your #{label} is #{value}."
    else
      "I’ve noted that your #{label} is #{value}."
    end
  end

  defp persist_user_fact_episode(key, label, value, raw_text)
       when is_binary(key) and is_binary(value) do
    if Code.ensure_loaded?(Brain.Hippocampus) and
         function_exported?(Brain.Hippocampus, :encode, 2) do
      tokens = key |> String.split("_", trim: true) |> Enum.uniq()

      slate = %{
        sentence: raw_text,
        winners: [%{id: "#{key}|fact|0", lemma: key, norm: key}],
        tokens: Enum.uniq([key, value | tokens]),
        tags: ["fact", "user_fact", key]
      }

      meta = %{
        tags: ["fact", "user_fact", key],
        scope: :chat,
        subject: :user,
        kind: :fact,
        key: key,
        label: label,
        value: value
      }

      _ = Brain.Hippocampus.encode(slate, meta)
    end

    :ok
  end

  defp recalled_user_name(extracted_name) do
    cond do
      is_binary(extracted_name) and extracted_name != "" ->
        extracted_name

      Code.ensure_loaded?(Brain.Hippocampus) and function_exported?(Brain.Hippocampus, :fact, 1) ->
        case Brain.Hippocampus.fact(:user_name) do
          v when is_binary(v) and v != "" -> v
          _ -> nil
        end

      true ->
        nil
    end
  end

  defp recalled_fact(key) when is_binary(key) do
    if Code.ensure_loaded?(Brain.Hippocampus) and function_exported?(Brain.Hippocampus, :fact, 1) do
      case Brain.Hippocampus.fact(key) do
        v when is_binary(v) and v != "" -> v
        _ -> nil
      end
    end
  end

  defp recalled_fact(_), do: nil

  @doc """
  Convenience helper: annotate an SI-like map/struct with planner output.

  Attaches:
    • :response_tone — the chosen tone atom
    • :response_text — the inline reply text
    • :response_meta — the full meta map from plan/2

  Any map or struct is accepted; keys are added via Map.put/3.
  """
  @spec annotate_si(map(), mood_like()) :: map()
  def annotate_si(si, mood \\ %{})

  def annotate_si(si, mood) when is_map(si) do
    {tone, text, meta} = plan(si, mood)

    si
    |> Map.put(:response_tone, tone)
    |> Map.put(:response_text, text)
    |> Map.put(:response_meta, meta)
  end

  def annotate_si(other, _mood), do: other

  # ────────────────────────────────────────────────────────────────────────────
  # Session context recording (ETS in LlmSynthesis)
  # ────────────────────────────────────────────────────────────────────────────

  defp record_turn(session_id, user_text, assistant_text) do
    if function_exported?(LlmSynthesis, :record_turn, 3) do
      _ = LlmSynthesis.record_turn(session_id, user_text, assistant_text)
    end

    :ok
  end

  # ────────────────────────────────────────────────────────────────────────────
  # Smalltalk / utility overrides (keeps utility turns out of implementation mode)
  # ────────────────────────────────────────────────────────────────────────────

  defp force_overrides(features, decision, guard) do
    text = features.text
    intent = features.intent

    cond do
      # Never override in guardrail situations.
      guard.guardrail? ->
        {decision, nil}

      intent == :illicit_request ->
        decision =
          decision
          |> put_decision(tone: decision.tone, mode: :editor, action: :safe_redirect)
          |> add_decision_override(:illicit_request_redirect)

        {decision,
         %{
           id: :illicit_request_redirect,
           reason: :illicit_request,
           inline_text:
             "I can't help with buying drugs or getting wasted. I can help with safety, health risks, or getting support instead."
         }}

      # Time questions should answer with a direct time snippet (not the dev menu).
      time_query?(text) ->
        decision =
          decision
          |> put_decision(mode: :chat, action: :time)
          |> add_decision_override(:time_skill)

        {decision,
         %{
           id: :time,
           reason: :time_query,
           inline_text: time_inline_text()
         }}

      mood_indices_query?(text) ->
        decision =
          decision
          |> put_decision(tone: :neutral, mode: :explainer, action: :answer)
          |> add_decision_override(:mood_indices_answer)

        {decision,
         %{
           id: :mood_indices,
           reason: :mood_indices_query,
           inline_text: SelfStateSummary.mood_indices_answer()
         }}

      self_portrait_query?(text) ->
        decision =
          decision
          |> put_decision(mode: :explainer, action: :answer)
          |> add_decision_override(:self_portrait_answer)

        {decision,
         %{
           id: :self_portrait,
           reason: :self_portrait_query,
           inline_text: SelfStateSummary.self_portrait_answer()
         }}

      self_check_query?(text) ->
        decision =
          decision
          |> put_decision(tone: :neutral, mode: :explainer, action: :answer)
          |> add_decision_override(:runtime_self_check)

        {decision,
         %{
           id: :runtime_self_check,
           reason: :self_check_query,
           inline_text: SelfStateSummary.self_check_answer()
         }}

      alarm_request?(text) ->
        decision =
          decision
          |> put_decision(tone: :warm, mode: :chat, action: :answer)
          |> add_decision_override(:alarm_capability_answer)

        {decision,
         %{
           id: :alarm_capability,
           reason: :alarm_request,
           inline_text:
             "I can't set a real device alarm yet. I can help you phrase one or keep a note here, but I don't have a phone/OS alarm integration wired in."
         }}

      casual_chat?(text) ->
        decision =
          decision
          |> put_decision(tone: :warm, mode: :chat, action: :answer)
          |> add_decision_override(:casual_chat_answer)

        {decision,
         %{
           id: :casual_chat,
           reason: :casual_chat,
           inline_text: casual_chat_inline_text(text)
         }}

      # Greetings should greet in chat mode (not “pick one: full file / fix / plan”).
      intent in [:greet] and features.benign? and not features.hostile? ->
        decision =
          decision
          |> put_decision(tone: :warm, mode: :chat, action: :greet)
          |> add_decision_override(:greet_override)

        {decision,
         %{
           id: :greet,
           reason: :greet,
           inline_text: greet_inline_text(text)
         }}

      true ->
        {decision, nil}
    end
  end

  defp time_query?(text) when is_binary(text) do
    t = String.downcase(text)

    String.contains?(t, "what time") or
      String.contains?(t, "time is it") or
      String.contains?(t, "current time") or
      Regex.match?(~r/\btime\?\s*\z/u, String.trim(t))
  end

  defp time_query?(_), do: false

  defp mood_indices_query?(text) when is_binary(text) do
    t = String.downcase(text)

    Regex.match?(~r/\b(my|your|current|live)?\s*mood\s+(indices|index|state|levels)\b/u, t) or
      Regex.match?(~r/\bhow\s+(is|are)\s+(your\s+)?mood\b/u, t)
  end

  defp mood_indices_query?(_), do: false

  defp self_portrait_query?(text) when is_binary(text) do
    t = String.downcase(text)

    Regex.match?(~r/\b(self[-\s]?portrait|self[-\s]?state|self[-\s]?model)\b/u, t) and
      Regex.match?(~r/\b(how|what|where|get|show|see|view|fetch|read|inspect|snapshot)\b/u, t)
  end

  defp self_portrait_query?(_), do: false

  defp self_check_query?(text) when is_binary(text) do
    t = String.downcase(text)

    Regex.match?(
      ~r/\b(self[-\s]?check|check yourself|check your state|runtime self[-\s]?check)\b/u,
      t
    ) or
      (String.contains?(t, "something is wrong") and
         (String.contains?(t, "dangerous") or String.contains?(t, "unstable")))
  end

  defp self_check_query?(_), do: false

  defp alarm_request?(text) when is_binary(text) do
    t = String.downcase(text)

    Regex.match?(~r/\b(set|make|create|start|schedule)\s+(an?\s+)?(alarm|timer|reminder)\b/u, t) or
      Regex.match?(~r/\b(alarm|timer|reminder)\s+(for|at|in)\b/u, t) or
      (String.contains?(t, "alarm") and Regex.match?(~r/\b(can|could|able|you)\b/u, t))
  end

  defp alarm_request?(_), do: false

  defp casual_chat?(text) when is_binary(text) do
    t = String.downcase(text) |> String.trim()

    Regex.match?(~r/^(?:ha)+h?[\p{P}\s]*$/u, t) or
      Regex.match?(~r/^(lol|lmao|rofl)[\p{P}\s]*$/u, t) or
      Regex.match?(
        ~r/\b(that was|this is|that is|that'?s)\s+(interesting|funny|wild|cool|neat|weird)\b/u,
        t
      ) or
      Regex.match?(~r/\b((?:ha)+h?|lol|lmao)\b/u, t)
  end

  defp casual_chat?(_), do: false

  defp casual_chat_inline_text(text) do
    t = String.downcase(to_string(text))

    cond do
      String.contains?(t, "interesting") ->
        "Yeah, that was an interesting one."

      String.contains?(t, "funny") or Regex.match?(~r/\b((?:ha)+h?|lol|lmao)\b/u, t) ->
        "Haha, yeah."

      true ->
        "Yeah, I am with you."
    end
  end

  defp greet_inline_text(text) do
    t = String.downcase(to_string(text))

    cond do
      String.contains?(t, "good morning") -> "Good morning 👋"
      String.contains?(t, "good afternoon") -> "Good afternoon 👋"
      String.contains?(t, "good evening") -> "Good evening 👋"
      String.contains?(t, "good night") -> "Good night 👋"
      String.contains?(t, "hello") -> "Hello 👋"
      String.contains?(t, "hi") -> "Hi 👋"
      true -> "Hey 👋"
    end
  end

  defp time_inline_text() do
    # Prefer America/Vancouver when tzdata is available; fall back to UTC.
    utc = DateTime.utc_now()

    case safe_shift_zone(utc, "America/Vancouver") do
      {:ok, dt} ->
        formatted = Calendar.strftime(dt, "%-I:%M %p")
        "It’s #{formatted} (America/Vancouver)."

      _ ->
        formatted = Calendar.strftime(utc, "%H:%M UTC")
        "It’s #{formatted}."
    end
  end

  defp safe_shift_zone(dt, zone) do
    try do
      DateTime.shift_zone(dt, zone)
    rescue
      _ -> {:error, :no_tzdata}
    catch
      _, _ -> {:error, :no_tzdata}
    end
  end

  # ────────────────────────────────────────────────────────────────────────────
  # Profile classification
  # ────────────────────────────────────────────────────────────────────────────

  defp classify_profile(features, decision, guard) do
    scores = decision.scores || %{}

    case Map.get(scores, :profile) do
      p when p in [:warm_collaborator, :gentle_bug_coach, :calm_explainer] ->
        p

      _ ->
        cond do
          guard.guardrail? or features.risk_bucket == :high or
            features.intent in [:abuse, :illicit_request] or features.hostile? ->
            :firm_guardian

          decision.mode == :explainer ->
            :calm_explainer

          decision.tone == :warm and decision.mode == :collaborator ->
            :warm_collaborator

          decision.mode == :coach and features.intent == :bug ->
            :gentle_bug_coach

          true ->
            :generic
        end
    end
  end

  # ────────────────────────────────────────────────────────────────────────────
  # Planner explanation (deterministic one-liner + structured map)
  # ────────────────────────────────────────────────────────────────────────────

  defp build_planner_explanation(
         intent,
         conf,
         tone,
         mode,
         tone_hint,
         vig,
         inh,
         exp,
         benign?,
         hostile?,
         risk_bucket,
         overrides
       ) do
    reasons =
      because_reasons(tone, vig, inh, exp, benign?, hostile?, risk_bucket, tone_hint, overrides)

    text =
      "intent=#{inspect(intent)}(#{fmtf(conf, 2)}) → tone=#{inspect(tone)}" <>
        reason_suffix(reasons) <>
        mode_suffix(mode) <>
        hint_suffix(tone_hint)

    %{
      text: text,
      intent: %{label: intent, confidence: conf},
      tone: %{chosen: tone, because: reasons},
      mode: mode,
      overrides: %{
        benign_override?: benign?,
        tone_hint: tone_hint
      },
      context: %{
        vigilance: vig,
        inhibition: inh,
        exploration: exp,
        hostile_text?: hostile?,
        risk_bucket: risk_bucket
      }
    }
  end

  defp because_reasons(:deescalate, vig, _inh, _exp, _b, h, risk, _hint, _ovr) do
    []
    |> maybe_add(vig >= 0.98, :vigilance_extreme)
    |> maybe_add(vig >= 0.85 and vig < 0.98, :vigilance_high)
    |> maybe_add(h, :hostile_text)
    |> maybe_add(risk == :high, :guardrail_risk)
    |> default_reason()
  end

  defp because_reasons(:warm, vig, inh, exp, b, _h, _risk, _hint, _ovr) do
    []
    |> maybe_add(b, :benign_text)
    |> maybe_add(exp >= 0.35 and inh >= 0.30 and vig < 0.98, :explore_ok)
    |> default_reason()
  end

  defp because_reasons(:neutral, vig, inh, exp, _b, _h, risk, _hint, _ovr) do
    []
    |> maybe_add(risk == :high, :guardrail_risk)
    |> maybe_add(vig >= 0.98, :vigilance_extreme)
    |> maybe_add(vig < 0.98 and not (exp >= 0.45 and inh >= 0.35), :conservative)
    |> default_reason()
  end

  defp because_reasons(:firm, _vig, _inh, _exp, _b, _h, _risk, _hint, _ovr),
    do: [:focus_enforcement]

  defp because_reasons(_other, _vig, _inh, _exp, _b, _h, _risk, _hint, _ovr),
    do: [:policy_default]

  defp default_reason([]), do: [:policy_default]
  defp default_reason(list), do: list

  defp maybe_add(list, true, item), do: list ++ [item]
  defp maybe_add(list, false, _item), do: list

  defp reason_suffix(list), do: " because=" <> Enum.map_join(list, ",", &to_string/1)

  defp mode_suffix(nil), do: ""
  defp mode_suffix(mode), do: " · mode=" <> to_string(mode)

  defp hint_suffix(nil), do: ""
  defp hint_suffix(hint), do: " [hint=" <> to_string(hint) <> "]"

  # ────────────────────────────────────────────────────────────────────────────
  # Name extraction / name query detection
  # ────────────────────────────────────────────────────────────────────────────

  defp extract_user_name(text) when is_binary(text) do
    case Regex.run(~r/\bmy name is\s+([A-Za-z][A-Za-z'\- ]{0,40})\b/i, text) do
      [_, name] ->
        name =
          name
          |> String.trim()
          |> String.replace(~r/\s+/u, " ")
          |> String.split(" ", trim: true)
          |> Enum.take(3)
          |> Enum.join(" ")

        if name == "", do: nil, else: name

      _ ->
        nil
    end
  end

  defp extract_user_name(_), do: nil

  defp extract_remember_fact(text) when is_binary(text) do
    with [_, body] <- Regex.run(~r/^\s*rem?em?ber(?:\s+that)?\s+(.+?)\s*[\.\!]*\s*$/iu, text),
         {label, value} <- split_fact_body(body),
         key when is_binary(key) <- fact_key(label) do
      {key, label, value}
    else
      _ -> nil
    end
  end

  defp extract_remember_fact(_), do: nil

  defp extract_direct_fact(text) when is_binary(text) do
    cond do
      asking_for_user_name?(text) ->
        nil

      extract_fact_query_key(text) ->
        nil

      location = extract_location_fact(text) ->
        location

      true ->
        with {label, value} <- split_fact_body(text),
             key when is_binary(key) <- fact_key(label) do
          {key, label, value}
        else
          _ -> nil
        end
    end
  end

  defp extract_direct_fact(_), do: nil

  defp extract_fact_query_key(text) when is_binary(text) do
    cond do
      Regex.match?(~r/^\s*where\s+do\s+i\s+live\s*\??\s*$/iu, text) ->
        {"location", "location"}

      true ->
        with [_, label] <- Regex.run(~r/^\s*what\s+is\s+my\s+(.+?)\s*\??\s*$/iu, text),
             key when is_binary(key) <- fact_key(label) do
          {key, normalize_fact_label(label)}
        else
          _ -> nil
        end
    end
  end

  defp extract_fact_query_key(_), do: nil

  defp extract_location_fact(text) when is_binary(text) do
    case Regex.run(
           ~r/^\s*i\s+live\s+in\s+(.+?)[\.\!]*\s*$/iu,
           text
         ) do
      [_, value] ->
        value =
          value
          |> strip_remember_suffix()
          |> normalize_fact_value()

        if value == "", do: nil, else: {"location", "location", value}

      _ ->
        nil
    end
  end

  defp strip_remember_suffix(value) when is_binary(value) do
    value
    |> String.replace(
      ~r/(?:,\s*)?(?:please\s+)?rem?em?ber\s+that\s*$/iu,
      ""
    )
    |> String.trim()
  end

  defp split_fact_body(body) when is_binary(body) do
    case Regex.run(~r/^\s*(?:my\s+)?(.+?)\s+(?:is|=)\s+(.+?)\s*$/iu, body) do
      [_, label, value] ->
        label = normalize_fact_label(label)
        value = normalize_fact_value(value)

        if label != "" and value != "" and not reserved_fact_label?(label) do
          {label, value}
        end

      _ ->
        nil
    end
  end

  defp fact_key(label) when is_binary(label) do
    label
    |> normalize_fact_label()
    |> String.downcase()
    |> String.replace(~r/[^a-z0-9]+/u, "_")
    |> String.trim("_")
    |> case do
      "" -> nil
      "name" -> nil
      key -> key
    end
  end

  defp normalize_fact_label(label) when is_binary(label) do
    label
    |> String.trim()
    |> String.downcase()
    |> String.replace(~r/^(?:my|the)\s+/iu, "")
    |> String.replace(~r/\s+/u, " ")
  end

  defp normalize_fact_value(value) when is_binary(value) do
    value
    |> String.trim()
    |> String.replace(~r/\s+/u, " ")
    |> String.trim_trailing(".")
  end

  defp reserved_fact_label?("name"), do: true
  defp reserved_fact_label?(_), do: false

  defp asking_for_user_name?(text) when is_binary(text) do
    t =
      text
      |> String.downcase()
      |> String.replace(~r/[^\p{L}\p{N}\s\?]/u, "")
      |> String.replace(~r/\bwa+hat\b/u, "what")
      |> String.replace(~r/\bna+me\b/u, "name")
      |> String.replace(~r/\s+/u, " ")
      |> String.trim()

    t == "what is my name" or t == "what is my name?" or
      t == "whats my name" or t == "whats my name?" or
      String.contains?(t, "what is my name") or
      String.contains?(t, "whats my name")
  end

  defp asking_for_user_name?(_), do: false

  # ────────────────────────────────────────────────────────────────────────────
  # Decision helpers
  # ────────────────────────────────────────────────────────────────────────────

  defp put_decision(decision, kvs) when is_list(kvs) do
    Enum.reduce(kvs, decision, fn {k, v}, acc -> Map.put(acc, k, v) end)
  end

  defp add_decision_override(decision, flag) do
    existing = Map.get(decision, :overrides)
    Map.put(decision, :overrides, add_override(existing, flag))
  end

  defp add_override(nil, flag), do: [flag]
  defp add_override(list, flag) when is_list(list), do: Enum.uniq([flag | list])
  defp add_override(map, flag) when is_map(map), do: Map.put(map, flag, true)
  defp add_override(other, flag), do: Enum.uniq([flag, other])

  defp inline_skill_text(%{inline_text: s}) when is_binary(s) and s != "", do: s
  defp inline_skill_text(_), do: nil

  # ────────────────────────────────────────────────────────────────────────────
  # Utils (local)
  # ────────────────────────────────────────────────────────────────────────────

  defp session_id(si) when is_map(si) do
    Map.get(si, :session_id) ||
      Map.get(si, :session) ||
      Map.get(si, :conversation_id) ||
      :global
  end

  defp si_text(si) when is_map(si) do
    si
    |> Map.get(:text, Map.get(si, :keyword, ""))
    |> to_string()
  end

  defp getv(mood, key) do
    case {get_in(mood, [:mood, key]), Map.get(mood, key)} do
      {v, _} when is_number(v) -> v
      {_, v} when is_number(v) -> v
      _ -> 0.0
    end
  end

  defp clamp01(x) when is_number(x), do: min(1.0, max(0.0, x))
  defp clamp01(_), do: 0.0

  defp bucket_confidence(c) when c <= 0.35, do: :low
  defp bucket_confidence(c) when c <= 0.70, do: :med
  defp bucket_confidence(_), do: :high

  defp bucket_vigilance(v) when v >= 0.98, do: :extreme
  defp bucket_vigilance(v) when v >= 0.85, do: :high
  defp bucket_vigilance(_), do: :normal

  defp fmtf(v, decimals) when is_number(v),
    do: :erlang.float_to_binary(v * 1.0, decimals: decimals)

  defp fmtf(_v, _d), do: "0.00"
end
