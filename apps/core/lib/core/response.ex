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
  alias Core.Response.Guardrails
  alias Core.Response.Skills
  alias Core.Response.ActionAlignment
  alias Core.Response.AffectPolicy
  alias Core.Response.LlmSynthesis
  alias Core.Response.Memory
  alias Core.Response.Meta
  alias Core.Response.OverrideSkills
  alias Core.Response.SelfStateFeatures
  alias Core.Response.SideEffects
  alias Core.Response.SymbolicGeneration
  alias Core.Response.SemanticStrategy
  alias Core.Response.LexicalDefinition
  alias Core.Response.AttentionFallback
  alias Core.Response.OpenQuestionBoundary
  alias Core.Response.NewsLookup

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

    memory_reply =
      Memory.reply_for(%{
        intent0: intent0,
        conf: conf,
        text_in: text_in,
        session_id: session_id,
        extracted_name: extracted_name,
        remember_fact: remember_fact,
        direct_fact: direct_fact,
        fact_query_key: fact_query_key,
        semantic_context: Map.take(si, [:sentence, :tokens, :sentence_signatures,
          :sentence_meaning, :provisional_interpretation, :discourse_reference])
      }) ||
        Memory.unresolved_lexical_reply(si, text_in, session_id, intent0, conf) ||
        Memory.acknowledge_projected(si, session_id, text_in, intent0, conf)

    case memory_reply do
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
            extracted_name: user_name_candidate(extracted_name),
            evidence: Map.get(si, :evidence),
            comprehension: Map.get(si, :comprehension),
            prefrontal: Map.get(si, :prefrontal),
            control_signals: Map.get(si, :control_signals),
            response_policy: Map.get(mood, :response_policy),
            turn_context: Map.get(si, :turn_context),
            projected_expression: Map.get(si, :projected_expression),
            symbolic_frame: Map.get(si, :symbolic_frame),
            discourse_reference: Map.get(si, :discourse_reference),
            self_model: Map.get(si, :self_model),
            self_monitor: Map.get(si, :self_monitor),
            self_memory_recall: Map.get(si, :self_memory_recall),
            agency_decision: Map.get(si, :agency_decision),
            agency_commands: Map.get(si, :agency_commands),
            agency_command_results: Map.get(si, :agency_command_results),
            agency_memory: agency_memory
          })

        {decision0, skill} = decide_and_pick_skill(features, guard, text_in)
        request_structure =
          si
          |> Map.get(:turn_context, %{})
          |> then(fn context ->
            if is_map(context), do: Map.get(context, :request_structure), else: nil
          end)

        decision =
          ActionAlignment.align(
            decision0, Map.get(si, :selected_action), text_in,
            guard.guardrail?, request_structure
          )
        features =
          features
          |> put_chosen_skill(skill)
          |> Map.put(:selected_action, Map.get(si, :selected_action))
          |> Map.put(:action_meta, Map.get(si, :action_meta))

        name_claim? = name_claim?(extracted_name, text_in)
        name_persistence = maybe_persist_user_name(name_claim?, extracted_name, text_in)

        forced_identity =
          if name_claim? and match?({:error, _}, name_persistence) and
               is_binary(user_name_candidate(extracted_name)) do
            "I heard your name as #{extracted_name}, but I couldn't save it permanently."
          else
            forced_identity_text(text_in, extracted_name, name_claim?)
          end

        inline_text = OverrideSkills.deterministic_inline_text(skill)
        llm_prompt_type = OverrideSkills.llm_prompt_override(skill)

        # Select the response operation from independently interpreted syntax
        # and semantic evidence. Plans can remain unresolved; they cannot
        # authorize content without the existing generator's revalidation.
        no_llm? = Application.get_env(:core, :llm_inference_enabled?, true) == false
        strategy_plan = if no_llm?, do: SemanticStrategy.select(si), else: :not_applicable

        strategy_response =
          if is_map(strategy_plan) do
            SemanticStrategy.realize(strategy_plan, text_in, features, decision, si)
          else
            :not_applicable
          end

        # In the experimental no-LLM runtime, realize only supported,
        # independently safe speech acts. Keep unsupported turns visibly
        # unavailable instead of silently restoring model inference.
        symbolic_response =
          if no_llm? and strategy_response == :not_applicable do
            SymbolicGeneration.generate(text_in, features, decision, si)
          else
            :not_applicable
          end

        # Explicit lexical questions are answered from this turn's LTM rows.
        # A definition request must not collapse into a generic WM topic.
        lexical_response =
          if no_llm? and strategy_response == :not_applicable and
               symbolic_response == :not_applicable do
            LexicalDefinition.generate(text_in, features, decision, si)
          else
            :not_applicable
          end

        # Current-events questions first request dated sources through the
        # existing evidence resolver, even when local LLM inference is off.
        # Only provider-validated headline discovery may become a response.
        news_lookup_response =
          if no_llm? and strategy_response == :not_applicable and
               symbolic_response == :not_applicable and
               lexical_response == :not_applicable do
            NewsLookup.generate(text_in, features, decision, si)
          else
            :not_applicable
          end

        # A recognizable open question is not necessarily a lexical
        # misunderstanding. Preserve the request type and evidence deficit
        # without synthesizing current facts or restoring the LLM.
        open_question_response =
          if no_llm? and strategy_response == :not_applicable and
               symbolic_response == :not_applicable and
               lexical_response == :not_applicable and
               news_lookup_response == :not_applicable do
            OpenQuestionBoundary.generate(text_in, features, decision, si)
          else
            :not_applicable
          end

        # Only genuinely unresolved turns fall through to lexical/WM repair.
        attention_response =
          if no_llm? and strategy_response == :not_applicable and
               symbolic_response == :not_applicable and
               lexical_response == :not_applicable and
               open_question_response == :not_applicable and
               news_lookup_response == :not_applicable do
            AttentionFallback.generate(text_in, features, decision, si)
          else
            :not_applicable
          end

        {text0, response_source, response_fallback_reason, response_reflection} =
          cond do
            is_binary(forced_identity) ->
              {forced_identity, :forced_identity, nil, nil}

            is_binary(inline_text) ->
              {inline_text, :inline_skill, nil, nil}

            match?({:ok, _, _, _}, strategy_response) ->
              {:ok, reply, source, realization} = strategy_response
              {reply, source, nil, realization}

            match?({:ok, _, _}, symbolic_response) ->
              {:ok, reply, realization} = symbolic_response
              {reply, :symbolic_generation, nil, realization}

            match?({:ok, _, _}, lexical_response) ->
              {:ok, reply, realization} = lexical_response
              {reply, :symbolic_lexical_definition, nil, realization}

            match?({:ok, _, _}, news_lookup_response) ->
              {:ok, reply, realization} = news_lookup_response
              {reply, :symbolic_news_discovery, nil, realization}

            match?({:ok, _, _}, open_question_response) ->
              {:ok, reply, realization} = open_question_response
              {reply, :symbolic_request_boundary, nil, realization}

            match?({:ok, _, _}, attention_response) ->
              {:ok, reply, realization} = attention_response
              {reply, :wm_attention_fallback, nil, realization}

            true ->
              llm_or_model_unavailable(text_in, features, decision, mood, intent, llm_prompt_type)
          end

        # A verified conclusion or a neutral insufficient-evidence answer
        # is already a complete response. The independent curiosity loop must
        # never append its investigation prompts to user-facing chat.
        {text, curiosity_probe} =
          if response_source in [:symbolic_verified, :symbolic_verified_followup, :symbolic_unresolved,
                                  :symbolic_generation, :symbolic_lexical_definition,
                                  :wm_attention_fallback, :symbolic_request_boundary,
                                  :symbolic_news_discovery] do
            {text0, nil}
          else
            SelfStateFeatures.append_curiosity_probe(text0, features, decision, guard, skill)
          end

        SideEffects.record_turn(session_id, text_in, text, si)

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
            extracted_name: user_name_candidate(extracted_name),
            planner_explanation: planner_explanation,
            comprehension: Map.get(features, :comprehension),
            prefrontal: Map.get(features, :prefrontal),
            control_signals: Map.get(features, :control_signals),
            curiosity_probe: curiosity_probe,
            response_source: response_source,
            response_fallback_reason: response_fallback_reason,
            response_reflection: response_reflection,
            agency_decision: Map.get(features, :agency_decision),
            agency_commands: Map.get(features, :agency_commands),
            agency_command_results: Map.get(features, :agency_command_results),
            agency_memory: agency_memory
          })

        strategy_metadata =
          if is_map(strategy_plan) do
            Map.put(strategy_plan, :outcome,
              if(match?({:ok, _, _, _}, strategy_response), do: :realized, else: :deferred))
          end

        meta = Map.put(meta, :semantic_response_strategy, strategy_metadata)

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
      fact_query_key: fact_query_key,
      semantic_context: Map.take(si, [:sentence, :tokens, :sentence_signatures,
        :sentence_meaning, :provisional_interpretation, :discourse_reference])
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
         projected_expression: projected_expression,
         symbolic_frame: symbolic_frame,
         discourse_reference: discourse_reference,
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
      user_name: user_name_candidate(extracted_name),
      evidence: evidence,
      comprehension: comprehension,
      prefrontal: prefrontal,
      control_signals: control_signals,
      response_policy: response_policy,
      turn_context: turn_context,
      projected_expression: projected_expression,
      symbolic_frame: symbolic_frame,
      discourse_reference: discourse_reference,
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

  defp user_name_candidate(name) when is_binary(name) and name != "",
    do: SideEffects.recalled_user_name(name)

  defp user_name_candidate(_), do: nil

  defp maybe_persist_user_name(true, name, text_in),
    do: SideEffects.persist_user_name(name, text_in)

  defp maybe_persist_user_name(_, _name, _text_in), do: :ok

  defp forced_identity_text(text_in, extracted_name, name_claim?) do
    Memory.forced_identity_text(text_in, extracted_name, name_claim?)
  end

  defp put_chosen_skill(features, %{id: id}) when is_map(features) and is_atom(id) do
    Map.put(features, :skill, id)
  end

  defp put_chosen_skill(features, _skill), do: features

  defp llm_or_model_unavailable(text_in, features, decision, mood, _intent, prompt_type) do
    case LlmSynthesis.generate(text_in, features, decision, mood, prompt_type) do
      {:ok, verified_text, %{authoritative_plan: %{status: :enforced}} = reflection} ->
        {verified_text, :symbolic_verified, nil, reflection}

      {:ok, answer, %{status: :verified_evidence_followup} = reflection} ->
        {answer, :symbolic_verified_followup, nil, reflection}

      {:ok, answer, %{status: :insufficient_verified_evidence} = reflection} ->
        {answer, :symbolic_unresolved, nil, reflection}

      {:ok, llm_text, reflection} ->
        {llm_text, :llm, nil, reflection}

      {:error, reason} ->
        normalized_reason = normalize_generation_reason(reason)

        {model_unavailable_text(normalized_reason), :model_unavailable, normalized_reason, nil}
    end
  end

  defp normalize_generation_reason(reason) when is_atom(reason), do: reason
  defp normalize_generation_reason({reason, _}) when is_atom(reason), do: reason
  defp normalize_generation_reason(%{reason: reason}) when is_atom(reason), do: reason
  defp normalize_generation_reason(%{"reason" => reason}) when is_atom(reason), do: reason
  defp normalize_generation_reason(_), do: :unknown

  # On the symbolic-generation branch, an unsupported turn is a useful
  # coverage signal, not a reason to silently re-enable model inference.
  defp model_unavailable_text(:llm_disabled) do
    "Symbolic response unavailable for this turn (LLM intentionally disabled)."
  end

  defp model_unavailable_text(reason) do
    "Model unavailable (#{reason}). No fallback response was generated."
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
    (Map.get(si, :text) || Map.get(si, :sentence) ||
       Map.get(si, :keyword) || "")
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
