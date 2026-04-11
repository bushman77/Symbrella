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

    # Hard precedence: identity questions never go to skills/LLM.
    if asking_for_user_name?(text_in) do
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

      record_turn(session_id, text_in, text)
      :telemetry.execute([:core, :response, :plan], %{}, meta)

      {:warm, text, meta}
    else
      {vig, inh, exp, pls, tone_hint} = mood_sample(mood)

      intent = Policy.normalize_intent(intent0, text_in)
      guard = Guardrails.detect(text_in)

      benign? = Policy.benign_text?(text_in)
      hostile? = Policy.hostile_text?(text_in)
      command? = Policy.command?(text_in)

      confidence_bucket = bucket_confidence(conf)
      vigilance_bucket = bucket_vigilance(vig)
      risk_bucket = if guard.guardrail?, do: :high, else: :low

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
          extracted_name: extracted_name
        })

      {decision, skill} = decide_and_pick_skill(features, guard, text_in)

      name_claim? = name_claim?(extracted_name, text_in)
      maybe_persist_user_name(name_claim?, extracted_name, text_in)

      forced_identity = forced_identity_text(text_in, extracted_name, name_claim?)

      text =
        forced_identity ||
          inline_skill_text(skill) ||
          llm_or_template(text_in, features, decision, mood, intent)

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
          planner_explanation: planner_explanation
        })

      :telemetry.execute([:core, :response, :plan], %{}, meta)

      {decision.tone, text, meta}
    end
  end

  def plan(_si, _mood), do: {:neutral, "", %{error: :invalid_args}}

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
         extracted_name: extracted_name
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
      user_name: extracted_name
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
         planner_explanation: planner_explanation
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
      explanation: planner_explanation
    }
  end

  # ─────────────────────────────────────────────────────────────────────────────
  # Durable name memory (Hippocampus)
  # ─────────────────────────────────────────────────────────────────────────────

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
  # Smalltalk / utility overrides (fixes “pair_programmer menu on greet/time”)
  # ────────────────────────────────────────────────────────────────────────────

  defp force_overrides(features, decision, guard) do
    text = features.text
    intent = features.intent

    cond do
      # Never override in guardrail situations.
      guard.guardrail? ->
        {decision, nil}

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
            features.intent in [:abuse] or features.hostile? ->
            :firm_guardian

          decision.mode == :explainer ->
            :calm_explainer

          decision.tone == :warm and decision.mode == :pair_programmer ->
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

  defp reason_suffix([]), do: ""
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
