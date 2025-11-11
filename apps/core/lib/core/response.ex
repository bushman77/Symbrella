defmodule Core.Response do
  @moduledoc """
  Response — small policy engine that maps (intent × mood × text) → {tone, text, meta}.

  Public API:
    • plan/2 — given an SI-like map and an optional mood-like map, decide tone/mode/action,
      optionally fire one micro-skill (inline text), and return {tone, text, meta}.

  Notes:
    • This is the successor to `Core.Response` (module renamed).
    • Children live under `Core.Response.*` (Policy, Modes, Guardrails, Skills).
    • Emits telemetry: [:core, :response, :plan].
  """

  @type si_like :: %{
          optional(:intent) => atom,
          optional(:keyword) => any,
          optional(:confidence) => number,
          optional(:text) => String.t()
        }

  @type mood_like :: %{
          optional(:mood) => %{
            optional(:exploration) => number,
            optional(:inhibition) => number,
            optional(:vigilance) => number,
            optional(:plasticity) => number
          },
          optional(:tone_hint) => atom
        }

  alias Core.Response.Policy
  alias Core.Response.Modes
  alias Core.Response.Guardrails
  alias Core.Response.Skills

  # ────────────────────────────────────────────────────────────────────────────
  # Public API
  # ────────────────────────────────────────────────────────────────────────────

  @spec plan(si_like, mood_like) :: {atom, String.t(), map}
  def plan(si, mood \\ %{}) do
    intent0 = Map.get(si, :intent, :unknown)
    conf    = clamp01(Map.get(si, :confidence, 0.0))
    text_in = to_string(Map.get(si, :text) || Map.get(si, :keyword) || "")

    # Mood indices (work with nested or flat)
    vig = getv(mood, :vigilance)
    inh = getv(mood, :inhibition)
    exp = getv(mood, :exploration)
    pls = getv(mood, :plasticity)
    tone_hint = Map.get(mood, :tone_hint)

    # Normalize intent from raw text when classifier is unknown/coarse
    intent = Policy.normalize_intent(intent0, text_in)

    # Guardrails & lexical flags
    guard = Guardrails.detect(text_in)
    benign?  = Policy.benign_text?(text_in)
    hostile? = Policy.hostile_text?(text_in)
    command? = Policy.command?(text_in)

    # Buckets & context (cooldown could be supplied later; start at 0)
    confidence_bucket = bucket_confidence(conf)
    vigilance_bucket  = bucket_vigilance(vig)
    risk_bucket       = if guard.guardrail?, do: :high, else: :low
    cooldown          = 0

    features = %{
      intent_in: intent0,
      intent: intent,
      text: text_in,
      conf: conf,
      confidence_bucket: confidence_bucket,
      vig: vig, inh: inh, exp: exp, plast: pls,
      vigilance_bucket: vigilance_bucket,
      tone_hint: tone_hint,
      benign?: benign?,
      hostile?: hostile?,
      command?: command?,
      cooldown: cooldown,
      episode_bias: 0.0,
      guardrail?: guard.guardrail?,
      approve_token?: guard.approve_token?,
      risk_bucket: risk_bucket,
      guardrail_flags: guard.flags
    }

    # Policy decision (tone/mode/action + scores/overrides)
    decision = Policy.decide(features)

    # Optional micro-skill — at most one inline helper
    skill = Skills.pick(text_in, features, decision)

    # Final text: skill wins if it produces inline text; otherwise a mode template
    text =
      case skill do
        %{inline_text: s} when is_binary(s) and s != "" ->
          s

        _ ->
          Modes.compose(intent, decision.tone, decision.mode)
      end

    meta = %{
      policy_version: decision.policy_version,
      intent_inferred: intent,
      intent_original: intent0,
      confidence: conf,
      confidence_bucket: confidence_bucket,
      risk_bucket: risk_bucket,
      tone: decision.tone,
      mode: decision.mode,
      action: decision.action,
      benign: benign?,
      hostile: hostile?,
      tone_hint: tone_hint,
      mood_sample: %{exploration: exp, inhibition: inh, vigilance: vig, plasticity: pls},
      scores: decision.scores,
      overrides: decision.overrides,
      chosen_skill: (skill && skill.id) || nil,
      skill_reason: (skill && skill.reason) || nil,
      guardrail?: guard.guardrail?,
      approve_token?: guard.approve_token?,
      guardrail_flags: guard.flags
    }

    # Telemetry — safe if :telemetry is in deps (it is in Phoenix projects)
    :telemetry.execute([:core, :response, :plan], %{}, meta)

    {decision.tone, text, meta}
  end

  # ────────────────────────────────────────────────────────────────────────────
  # Utils (local)
  # ────────────────────────────────────────────────────────────────────────────

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
end

