defmodule Core.Response.Meta do
  @moduledoc """
  Metadata, profile classification, and planner explanation builders.
  """

  @spec build(map()) :: map()
  def build(%{
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
        agency_decision: agency_decision,
        agency_commands: agency_commands,
        agency_command_results: agency_command_results,
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
      agency_decision: agency_decision,
      agency_commands: agency_commands,
      agency_command_results: agency_command_results,
      agency_memory: agency_memory
    }
  end

  @spec classify_profile(map(), map(), map()) :: atom()
  def classify_profile(features, decision, guard) do
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

  @spec planner_explanation(
          atom(),
          number(),
          atom(),
          atom(),
          atom() | nil,
          number(),
          number(),
          number(),
          boolean(),
          boolean(),
          atom(),
          term()
        ) ::
          map()
  def planner_explanation(
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

  @spec bucket_confidence(number()) :: :low | :med | :high
  def bucket_confidence(c) when c <= 0.35, do: :low
  def bucket_confidence(c) when c <= 0.70, do: :med
  def bucket_confidence(_), do: :high

  @spec bucket_vigilance(number()) :: :extreme | :high | :normal
  def bucket_vigilance(v) when v >= 0.98, do: :extreme
  def bucket_vigilance(v) when v >= 0.85, do: :high
  def bucket_vigilance(_), do: :normal

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

  defp fmtf(v, decimals) when is_number(v),
    do: :erlang.float_to_binary(v * 1.0, decimals: decimals)

  defp fmtf(_v, _d), do: "0.00"
end
