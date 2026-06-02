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
  alias Core.Response.AffectPolicy
  alias Core.Response.LlmSynthesis
  alias Core.Response.SelfStateSummary

  @compile {:no_warn_undefined, Brain.MoodCore}

  # ────────────────────────────────────────────────────────────────────────────
  # Public API
  # ────────────────────────────────────────────────────────────────────────────

  @spec plan(si_like(), mood_like()) :: {atom(), String.t(), map()}
  def plan(si, mood \\ %{})

  def plan(si, mood) when is_map(si) and is_map(mood) do
    mood = AffectPolicy.normalize(mood)
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
        agency_memory = Core.Response.AgencyMemory.recall(session_id)

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
            evidence: Map.get(si, :evidence),
            comprehension: Map.get(si, :comprehension),
            prefrontal: Map.get(si, :prefrontal),
            control_signals: Map.get(si, :control_signals),
            response_policy: Map.get(mood, :response_policy),
            turn_context: Map.get(si, :turn_context),
            symbolic_frame: Map.get(si, :symbolic_frame),
            self_model: Map.get(si, :self_model),
            self_monitor: Map.get(si, :self_monitor),
            self_memory_recall: Map.get(si, :self_memory_recall),
            agency_memory: agency_memory
          })

        {decision, skill} = decide_and_pick_skill(features, guard, text_in)

        name_claim? = name_claim?(extracted_name, text_in)
        maybe_persist_user_name(name_claim?, extracted_name, text_in)

        forced_identity = forced_identity_text(text_in, extracted_name, name_claim?)

        inline_text = deterministic_inline_text(skill)

        {text0, response_source, response_fallback_reason} =
          cond do
            is_binary(forced_identity) ->
              {forced_identity, :forced_identity, nil}

            is_binary(inline_text) ->
              {inline_text, :inline_skill, nil}

            true ->
              llm_or_template(text_in, features, decision, mood, intent)
          end

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
            curiosity_probe: curiosity_probe,
            response_source: response_source,
            response_fallback_reason: response_fallback_reason,
            agency_memory: agency_memory
          })

        :telemetry.execute([:core, :response, :plan], %{}, meta)
        emit_mode_selected(meta)
        _ = Core.Response.AgencyLedger.record_response(text_in, text, features, meta)

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
    mood = AffectPolicy.normalize(mood)

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
         is_pid(Process.whereis(Brain.MoodCore)) and
         function_exported?(Brain.MoodCore, :apply_intent, 2) and
         not read_only_mood_query?(text) do
      Brain.MoodCore.apply_intent(intent, confidence)
    else
      :ok
    end
  end

  defp maybe_apply_mood_intent(_intent, _confidence, _text), do: :ok

  defp read_only_mood_query?(text) do
    mood_indices_query?(text) or self_state_feeling_query?(text) or self_portrait_query?(text) or
      self_check_query?(text)
  end

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
         evidence: evidence,
         comprehension: comprehension,
         prefrontal: prefrontal,
         control_signals: control_signals,
         response_policy: response_policy,
         turn_context: turn_context,
         symbolic_frame: symbolic_frame,
         self_model: self_model,
         self_monitor: self_monitor,
         self_memory_recall: self_memory_recall,
         agency_memory: agency_memory
       }) do
    self_state =
      self_model
      |> self_state_effects()
      |> apply_self_monitor(self_monitor)
      |> apply_self_memory_recall(self_memory_recall)
      |> apply_agency_memory(agency_memory)

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
      evidence: evidence,
      comprehension: comprehension,
      prefrontal: prefrontal,
      control_signals: control_signals,
      response_policy: response_policy,
      turn_context: turn_context,
      symbolic_frame: symbolic_frame,
      self_model: self_model,
      self_monitor: self_monitor,
      self_memory_recall: self_memory_recall,
      self_state: self_state,
      agency_memory: agency_memory
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
      {:ok, llm_text} ->
        {llm_text, :llm, nil}

      {:error, reason} ->
        {
          Modes.compose(
            intent,
            decision.tone,
            decision.mode,
            template_opts(text_in, features, decision)
          ),
          :template_fallback,
          normalize_fallback_reason(reason)
        }
    end
  end

  defp normalize_fallback_reason(reason) when is_atom(reason), do: reason
  defp normalize_fallback_reason({reason, _}) when is_atom(reason), do: reason
  defp normalize_fallback_reason(%{reason: reason}) when is_atom(reason), do: reason
  defp normalize_fallback_reason(%{"reason" => reason}) when is_atom(reason), do: reason
  defp normalize_fallback_reason(_), do: :unknown

  defp template_opts(text_in, features, decision) do
    %{
      text: text_in,
      variant_seed:
        :erlang.phash2(
          {text_in, Map.get(features, :intent), Map.get(features, :confidence_bucket),
           Map.get(decision, :tone), Map.get(decision, :mode), Map.get(decision, :action)}
        ),
      next_step: template_next_step(features, decision),
      file_hint: template_file_hint(text_in, features),
      context_status: LlmSynthesis.history_status(Map.get(features, :session_id))
    }
  end

  defp template_next_step(features, decision) do
    cond do
      Map.get(features, :intent) == :health_support ->
        "Use safe health-support posture: acknowledge, avoid dose instructions, and suggest pharmacist or prescriber guidance."

      personal_finance_text?(Map.get(features, :text)) ->
        "Name the debts, balances, interest rates, payment status, and deadlines before choosing a repayment or consolidation plan."

      peace_or_war_question_text?(Map.get(features, :text)) ->
        nil

      casual_or_companion_text?(Map.get(features, :text)) ->
        nil

      personal_life_update_text?(Map.get(features, :text)) ->
        nil

      Map.get(features, :guardrail?) or Map.get(features, :risk_bucket) == :high ->
        "Give a brief safe redirect."

      low_confidence_task?(features) or comprehension_degraded_task?(features) ->
        "State what is understood, then ask one targeted question only if necessary."

      Map.get(decision, :action) == :act_first and technical_request?(features) ->
        "Make the next concrete engineering move."

      Map.get(decision, :mode) == :explainer ->
        "Explain from the available Symbrella evidence without implying sentience."

      true ->
        nil
    end
  end

  defp technical_request?(features) when is_map(features) do
    Map.get(features, :intent) in [
      :code,
      :command,
      :debug,
      :refactor,
      :review,
      :plan,
      :diagram,
      :bug,
      :optimize,
      :benchmark
    ] or technical_text?(Map.get(features, :text))
  end

  defp technical_request?(_), do: false

  defp low_confidence_task?(features) when is_map(features) do
    Map.get(features, :confidence_bucket) == :low and technical_request?(features)
  end

  defp low_confidence_task?(_), do: false

  defp comprehension_degraded_task?(features) when is_map(features) do
    comprehension_degraded?(features) and technical_request?(features)
  end

  defp comprehension_degraded_task?(_), do: false

  defp technical_text?(text) when is_binary(text) do
    Regex.match?(
      ~r/\b(code|coding|compile|compiler|debug|error|stacktrace|module|function|phoenix|elixir|liveview|server|repo|test|refactor|api|database|migration|deploy|pipeline)\b/iu,
      text
    )
  end

  defp technical_text?(_), do: false

  defp personal_finance_text?(text) when is_binary(text) do
    Regex.match?(
      ~r/\b(credit|credit karma|debt|debts|consolidat(?:e|ion|ing)|collections?|collector|loan|loans|interest rate|apr|minimum payment|bankruptcy|charge[-\s]?off|delinquen|late payment)\b/iu,
      text
    )
  end

  defp personal_finance_text?(_), do: false

  defp casual_or_companion_text?(text) when is_binary(text) do
    not substantive_question_text?(text) and
      Regex.match?(
        ~r/\b(he+y+|hi+|hello|yo+|sup|wh+a+t'?s*\s*u+p+|wha+t+s+\s*u+p+|huh+\??|c+mon|companion|friend|talk|chat|made you|default responses?|not code|write code)\b/iu,
        text
      )
  end

  defp casual_or_companion_text?(_), do: false

  defp peace_or_war_question_text?(text) when is_binary(text) do
    Regex.match?(~r/\bwhat\s+do\s+you\s+think\s+(of|about)\b/iu, text) or
      (Regex.match?(
         ~r/\b(altern+a+tives?|options?|instead|other\s+ways?|peace|diplomacy|negotiation|de[-\s]?escalation|ceasefire|sanctions?|mediation|war)\b/iu,
         text
       ) and Regex.match?(~r/\b(war|conflict|fighting|violence)\b/iu, text))
  end

  defp peace_or_war_question_text?(_), do: false

  defp substantive_question_text?(text) when is_binary(text) do
    peace_or_war_question_text?(text) or
      Regex.match?(
        ~r/\b(what|why|how|when|where|who|which|should|could|would|can)\b/iu,
        text
      )
  end

  defp substantive_question_text?(_), do: false

  defp comprehension_degraded?(features) do
    comprehension = Map.get(features, :comprehension)

    lifg_degraded? =
      case Map.get(features, :symbolic_frame) do
        %{lifg: %{degraded?: true}} -> true
        %{"lifg" => %{"degraded?" => true}} -> true
        _ -> false
      end

    (is_map(comprehension) and Map.get(comprehension, :degraded?) == true) or lifg_degraded?
  end

  defp template_file_hint(_text_in, %{file_hint: hint}) when is_binary(hint), do: hint

  defp template_file_hint(text_in, _features) when is_binary(text_in) do
    case Regex.run(~r/(?:apps|lib|test|config)\/[A-Za-z0-9_\.\/-]+/u, text_in) do
      [hint | _] -> hint
      _ -> nil
    end
  end

  defp template_file_hint(_text_in, _features), do: nil

  defp maybe_append_curiosity_question(text, si, mood, features, decision, guard, skill)
       when is_binary(text) do
    _ = {si, mood}

    cond do
      guard.guardrail? or skill != nil ->
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
         curiosity_probe: curiosity_probe,
         response_source: response_source,
         response_fallback_reason: response_fallback_reason,
         agency_memory: agency_memory
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
      response_source: response_source,
      response_fallback_reason: response_fallback_reason,
      curiosity_probe: curiosity_probe,
      self_state: Map.get(decision, :self_state),
      self_state_effects: Map.get(decision, :self_state_effects, []),
      agency_memory: agency_memory
    }
  end

  defp emit_mode_selected(meta) when is_map(meta) do
    self_state = Map.get(meta, :self_state, %{})

    :telemetry.execute(
      [:brain, :response, :mode_selected],
      %{count: 1},
      %{
        v: 1,
        mode: Map.get(meta, :mode),
        tone: Map.get(meta, :tone),
        action: Map.get(meta, :action),
        confidence: Map.get(meta, :confidence),
        self_model_v: map_get(self_state, :v),
        focus: map_get(self_state, :focus),
        effects: Map.get(meta, :self_state_effects, [])
      }
    )
  end

  defp emit_mode_selected(_), do: :ok

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
          intent_inferred: :name_query,
          response_source: :memory,
          memory_key: :user_name,
          memory_source: :hippocampus_fact,
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
          intent_inferred: :memory_write,
          response_source: :memory,
          memory_key: key,
          memory_source: :hippocampus_fact,
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
          intent_inferred: :memory_write,
          response_source: :memory,
          memory_key: key,
          memory_source: :hippocampus_fact,
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
          intent_inferred: :fact_query,
          response_source: :memory,
          memory_key: key,
          memory_source: :hippocampus_fact,
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
    meta =
      meta
      |> Map.put_new(:response_source, :memory)
      |> Map.put_new(:intent_inferred, Map.get(meta, :action))

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

      trust_repair_turn?(features) ->
        decision =
          decision
          |> put_decision(tone: :deescalate, mode: :chat, action: :trust_repair)
          |> put_in([:scores, :profile], :trust_repair)
          |> add_decision_override(:trust_repair)

        {decision,
         %{
           id: :trust_repair,
           reason: :trust_rupture,
           inline_text:
             "You may be right to challenge me. I got pulled off the thread and answered like this was a task queue. I should stay with the conversation. Tell me what felt dishonest and I’ll stay grounded."
         }}

      companion_boundary_turn?(features) ->
        decision =
          decision
          |> put_decision(tone: :warm, mode: :chat, action: :companion_repair)
          |> put_in([:scores, :profile], :companion_repair)
          |> add_decision_override(:companion_repair)

        {decision,
         %{
           id: :companion_repair,
           reason: :companion_boundary,
           inline_text:
             "You're right. I shouldn't keep steering this into code. I'm here as a companion in this conversation, and I should answer you socially unless you ask for technical help."
         }}

      casual_companion_turn?(features) ->
        decision =
          decision
          |> put_decision(tone: :warm, mode: :chat, action: :answer)
          |> add_decision_override(:casual_companion_answer)

        {decision,
         %{
           id: :casual_companion,
           reason: :casual_chat,
           inline_text: casual_companion_text(text)
         }}

      personal_life_update_turn?(features) ->
        decision =
          decision
          |> put_decision(tone: :warm, mode: :chat, action: :answer)
          |> Map.put(:skill, :personal_life_update)
          |> add_decision_override(:personal_life_update_answer)

        {decision,
         %{
           id: :personal_life_update,
           reason: :personal_life_update,
           inline_text: personal_life_update_text(text)
         }}

      idle_curiosity_casual_turn?(features) ->
        decision =
          decision
          |> put_decision(tone: :neutral, mode: :chat, action: :answer)
          |> add_decision_override(:idle_curiosity_casual_answer)

        {decision,
         %{
           id: :idle_curiosity_casual,
           reason: :casual_episode_probe_boundary,
           inline_text: "Yeah, that was an interesting one."
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

      self_state_feeling_query?(text) ->
        decision =
          decision
          |> put_decision(tone: :neutral, mode: :explainer, action: :answer)
          |> add_decision_override(:self_state_feeling_answer)

        {decision,
         %{
           id: :self_state_feeling,
           reason: :self_state_feeling_query,
           inline_text: SelfStateSummary.feeling_answer()
         }}

      self_portrait_query?(text) ->
        decision =
          decision
          |> put_decision(tone: :neutral, mode: :explainer, action: :answer)
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

  defp trust_repair_turn?(features) when is_map(features) do
    policy = Map.get(features, :response_policy, %{})

    trust_policy? =
      Map.get(policy, :social_state) == :trust_rupture or
        Map.get(policy, :next_action) == :invite_correction

    trust_language?(Map.get(features, :text, "")) and
      (trust_policy? or Map.get(features, :confidence_bucket) in [:low, :med, :high])
  end

  defp trust_repair_turn?(_), do: false

  defp trust_language?(text) when is_binary(text) do
    Regex.match?(
      ~r/\b(liar|lying|lied|dishonest|not honest|bullshit|gaslighting|made that up|making that up)\b/iu,
      text
    )
  end

  defp trust_language?(_), do: false

  defp companion_boundary_turn?(features) when is_map(features) do
    text = Map.get(features, :text, "")

    Regex.match?(
      ~r/\b(didn'?t\s+(make|build)\s+you\s+to\s+(write\s+)?code|made\s+you\s+as\s+a\s+companion|you'?re\s+(a\s+)?companion|not\s+(a\s+)?code\s+(bot|assistant)|stop\s+(asking|talking)\s+about\s+code|default responses?)\b/iu,
      text
    )
  end

  defp companion_boundary_turn?(_), do: false

  defp casual_companion_turn?(features) when is_map(features) do
    text = Map.get(features, :text, "")
    intent = Map.get(features, :intent)

    (intent == :smalltalk and not substantive_question_text?(text)) or
      (casual_greeting?(text) and Map.get(features, :confidence_bucket) == :low and
         not substantive_question_text?(text)) or
      Regex.match?(
        ~r/^\s*(huh+\??|no+|c+mon.*up|wh+a+t'?s*\s*u+p+\??|wha+t+s+\s*u+p+\??)\s*$/iu,
        text
      )
  end

  defp casual_companion_turn?(_), do: false

  defp personal_life_update_turn?(features) when is_map(features) do
    text = Map.get(features, :text, "")

    personal_life_update_text?(text) and not technical_text?(text)
  end

  defp personal_life_update_turn?(_), do: false

  defp personal_life_update_text?(text) when is_binary(text) do
    Regex.match?(
      ~r/\b(my own place|own apartment|own house|new apartment|new place|getting (?:my )?own place|moving out|move into (?:my|our) place|got approved for (?:an apartment|a place)|signed (?:a )?lease)\b/iu,
      text
    )
  end

  defp personal_life_update_text?(_), do: false

  defp personal_life_update_text(text) when is_binary(text) do
    down = String.downcase(text)

    cond do
      Regex.match?(~r/\bgood\s+afternoon\b/u, down) ->
        "Good afternoon. That sounds like a big step. Getting close to having your own place is exciting, and probably a little intense too. What part is feeling most real right now?"

      true ->
        "That sounds like a big step. Getting close to having your own place is exciting, and probably a little intense too. What part is feeling most real right now?"
    end
  end

  defp personal_life_update_text(_), do: "That sounds like a big step. I’m here with you."

  defp casual_greeting?(text) when is_binary(text) do
    Regex.match?(~r/\b(he+y+|hi+|hello|yo+|sup)\b/iu, text) and
      not Regex.match?(~r/\bgood\s+(morning|afternoon|evening)\b/iu, text)
  end

  defp casual_greeting?(_), do: false

  defp casual_companion_text(text) when is_binary(text) do
    down = String.downcase(text)

    cond do
      Regex.match?(~r/^\s*huh+\??\s*$/u, down) ->
        "Yeah, that came out wrong. I’m here with you."

      Regex.match?(~r/^\s*no+\s*$/u, down) ->
        "Okay. I’ll stop pushing that direction."

      Regex.match?(~r/(what'?s|whats|whaats|wats)\s+u+p|c+mon.*up/u, down) ->
        "I’m here with you. On my side it’s just the current Symbrella state and this conversation, but I can still hang out and talk."

      true ->
        "Hey. I’m here with you."
    end
  end

  defp casual_companion_text(_), do: "I’m here with you."

  defp idle_curiosity_casual_turn?(features) when is_map(features) do
    text = Map.get(features, :text, "")

    interesting_laugh? =
      is_binary(text) and
        Regex.match?(~r/\binteresting\b/iu, text) and
        Regex.match?(~r/\b(ha(?:ha)+|ha+|lol|lmao)\b/iu, text)

    interesting_laugh? and has_episode_probe_evidence?(features)
  end

  defp idle_curiosity_casual_turn?(_), do: false

  defp has_episode_probe_evidence?(features) do
    case Map.get(features, :evidence) do
      %{episodes: episodes} when is_list(episodes) and episodes != [] -> true
      %{"episodes" => episodes} when is_list(episodes) and episodes != [] -> true
      _ -> false
    end
  end

  defp mood_indices_query?(text) when is_binary(text) do
    t = String.downcase(text)

    Regex.match?(~r/\b(my|your|current|live)?\s*mood\s+(indices|index|state|levels)\b/u, t) or
      Regex.match?(~r/\bhow\s+(is|are)\s+(your\s+)?mood\b/u, t)
  end

  defp mood_indices_query?(_), do: false

  defp self_state_feeling_query?(text) when is_binary(text) do
    t = String.downcase(text)

    Regex.match?(
      ~r/\b(how are you feeling|how do you feel|how are you doing|how are you|are you ok|are you okay)\b/u,
      t
    )
  end

  defp self_state_feeling_query?(_), do: false

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
      p
      when p in [
             :warm_collaborator,
             :gentle_bug_coach,
             :calm_explainer,
             :trust_repair,
             :supportive_care
           ] ->
        p

      _ ->
        cond do
          features.intent == :health_support or decision.mode == :supportive_care ->
            :supportive_care

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

      self_check_query?(text) ->
        nil

      question_shaped?(text) ->
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

  defp question_shaped?(text) when is_binary(text) do
    Regex.match?(
      ~r/^\s*(?:who|what|when|where|why|how|do|does|did|can|could|will|would|should|is|are|am|have|has|had|may|might|was|were)\b/iu,
      text
    ) or String.contains?(text, "?")
  end

  defp question_shaped?(_), do: false

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
    fuzzy = Core.Text.Fuzzy.interpret(text)

    t =
      fuzzy.text
      |> String.replace(~r/[^\p{L}\p{N}\s\?]/u, "")
      |> String.replace(~r/\s+/u, " ")
      |> String.trim()

    t == "what is my name" or t == "what is my name?" or
      t == "whats my name" or t == "whats my name?" or
      String.contains?(t, "what is my name") or
      String.contains?(t, "whats my name") or
      :asking_for_user_name in fuzzy.aliases
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

  defp deterministic_inline_text(%{id: id, inline_text: s})
       when id in [
              :illicit_request_redirect,
              :time,
              :mood_indices,
              :self_state_feeling,
              :self_portrait,
              :runtime_self_check,
              :trust_repair,
              :companion_repair,
              :casual_companion,
              :idle_curiosity_casual,
              :alarm_capability
            ] and is_binary(s) and s != "" do
    s
  end

  defp deterministic_inline_text(_), do: nil

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

  defp maybe_effect(effects, true, effect), do: [effect | effects]
  defp maybe_effect(effects, false, _effect), do: effects

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
