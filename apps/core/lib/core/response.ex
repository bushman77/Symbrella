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
  alias Core.Response.Memory
  alias Core.Response.Meta
  alias Core.Response.OverrideSkills
  alias Core.Response.SelfStateFeatures
  alias Core.Response.SideEffects
  alias Core.Response.TemplateFallback

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
    extracted_name = Memory.extract_user_name(text_in)
    remember_fact = Memory.extract_remember_fact(text_in)
    direct_fact = Memory.extract_direct_fact(text_in)
    fact_query_key = Memory.extract_fact_query_key(text_in)

    case Memory.reply_for(%{
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

        _ =
          SideEffects.apply_mood_intent(
            intent,
            conf,
            OverrideSkills.read_only_mood_query?(text_in)
          )

        {vig, inh, exp, pls, tone_hint} = mood_sample(mood)
        guard = Guardrails.detect(text_in)

        benign? = Policy.benign_text?(text_in)
        hostile? = Policy.hostile_text?(text_in)
        command? = Policy.command?(text_in)

        confidence_bucket = Meta.bucket_confidence(conf)
        vigilance_bucket = Meta.bucket_vigilance(vig)
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
            agency_decision: Map.get(si, :agency_decision),
            agency_commands: Map.get(si, :agency_commands),
            agency_command_results: Map.get(si, :agency_command_results),
            agency_memory: agency_memory
          })

        {decision, skill} = decide_and_pick_skill(features, guard, text_in)

        name_claim? = name_claim?(extracted_name, text_in)
        maybe_persist_user_name(name_claim?, extracted_name, text_in)

        forced_identity = forced_identity_text(text_in, extracted_name, name_claim?)

        inline_text = OverrideSkills.deterministic_inline_text(skill)

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
          SelfStateFeatures.append_curiosity_probe(text0, features, decision, guard, skill)

        SideEffects.record_turn(session_id, text_in, text)

        profile = Meta.classify_profile(features, decision, guard)

        planner_explanation =
          Meta.planner_explanation(
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
          Meta.build(%{
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
            agency_decision: Map.get(features, :agency_decision),
            agency_commands: Map.get(features, :agency_commands),
            agency_command_results: Map.get(features, :agency_command_results),
            agency_memory: agency_memory
          })

        SideEffects.emit_plan(meta)
        SideEffects.emit_mode_selected(meta)
        SideEffects.record_agency_response(text_in, text, features, meta)

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
    extracted_name = Memory.extract_user_name(text_in)
    remember_fact = Memory.extract_remember_fact(text_in)
    direct_fact = Memory.extract_direct_fact(text_in)
    fact_query_key = Memory.extract_fact_query_key(text_in)

    Memory.reply_for(%{
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
         agency_decision: agency_decision,
         agency_commands: agency_commands,
         agency_command_results: agency_command_results,
         agency_memory: agency_memory
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
      agency_decision: agency_decision,
      agency_commands: agency_commands,
      agency_command_results: agency_command_results,
      self_state:
        SelfStateFeatures.self_state(
          self_model,
          self_monitor,
          self_memory_recall,
          agency_memory
        ),
      agency_memory: agency_memory
    }
  end

  defp decide_and_pick_skill(features, guard, text_in) do
    decision0 = Policy.decide(features)
    {decision, forced_skill} = OverrideSkills.apply(features, decision0, guard)
    skill = forced_skill || Skills.pick(text_in, features, decision)
    {decision, skill}
  end

  defp name_claim?(name, text_in) do
    is_binary(name) and name != "" and not Memory.asking_for_user_name?(text_in)
  end

  defp maybe_persist_user_name(true, name, text_in),
    do: SideEffects.persist_user_name(name, text_in)

  defp maybe_persist_user_name(_, _name, _text_in), do: :ok

  defp forced_identity_text(text_in, extracted_name, name_claim?) do
    Memory.forced_identity_text(text_in, extracted_name, name_claim?)
  end

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
            TemplateFallback.opts(text_in, features, decision)
          ),
          :template_fallback,
          TemplateFallback.normalize_reason(reason)
        }
    end
  end

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
end
