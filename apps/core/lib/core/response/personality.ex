defmodule Core.Response.Personality do
  @moduledoc """
  Prompt-facing personality policy.

  This module converts compact Brain/Core runtime evidence into a bounded
  behavioral state. It does not claim feelings or consciousness; it selects
  stable response tendencies such as restraint, curiosity, self-checking, and
  explanation depth.
  """

  @type response_profile ::
          :safety_redirect
          | :self_check
          | :semantic_repair
          | :brain_explainer
          | :self_state_boundary
          | :technical_work
          | :social_chat
          | :direct_answer

  @type temperament :: :steady | :curious | :careful | :direct | :supportive
  @type depth :: :brief | :normal | :deep

  @type t :: %{
          temperament: temperament(),
          response_profile: response_profile(),
          assertiveness: float(),
          curiosity: float(),
          restraint: float(),
          warmth: float(),
          self_check: float(),
          abstraction: float(),
          explanation_depth: depth(),
          reasons: [atom()]
        }

  @helpful_intents ~w(
    instruction help command refactor review plan diagram bug optimize benchmark
  )a

  @spec decide(map(), map(), map(), list(), map()) :: t()
  def decide(features, decision, mood, wm_items, context \\ %{}) do
    runtime_state = runtime_state(features, decision, mood, context)
    comprehension = comprehension(features, decision, context)
    profile = response_profile(features, decision, mood, wm_items, runtime_state, comprehension)
    reasons = reasons(profile, features, decision, runtime_state, comprehension)

    %{
      temperament: temperament(profile, runtime_state, features, decision),
      response_profile: profile,
      assertiveness: assertiveness(profile, features, runtime_state),
      curiosity: curiosity(profile, runtime_state),
      restraint: restraint(profile, runtime_state),
      warmth: warmth(profile, decision, features),
      self_check: self_check(profile, runtime_state),
      abstraction: abstraction(profile, runtime_state),
      explanation_depth: explanation_depth(profile, runtime_state),
      reasons: reasons
    }
  end

  @spec profile(t() | map()) :: response_profile()
  def profile(%{response_profile: profile}) when is_atom(profile), do: profile
  def profile(_), do: :direct_answer

  @spec directive(t() | map()) :: String.t()
  def directive(%{response_profile: :safety_redirect}) do
    "Profile instruction: decline unsafe or disallowed help briefly, then redirect to safe information or support."
  end

  def directive(%{response_profile: :self_check}) do
    "Profile instruction: self-check the prior answer first, acknowledge any concrete miss, then give the corrected answer without over-explaining."
  end

  def directive(%{response_profile: :semantic_repair}) do
    "Profile instruction: use the comprehension and LIFG quality summary to separate what is understood from what is uncertain. If the request is still actionable, answer with a brief caveat; ask one targeted question only when needed."
  end

  def directive(%{response_profile: :brain_explainer}) do
    "Profile instruction: explain Symbrella's brain modules as software control signals and evidence sources. Avoid biological equivalence or consciousness claims."
  end

  def directive(%{response_profile: :self_state_boundary}) do
    "Profile instruction: respond warmly to care or concern as Symbrella. Be explicit that Symbrella does not have human feelings or consciousness, then describe the current simulated affect/runtime state as software-derived self-state. Do not call yourself a generic tool, and do not end with a generic service offer."
  end

  def directive(%{response_profile: :technical_work}) do
    "Profile instruction: treat the request as engineering work. Be concrete about files, commands, behavior, tradeoffs, and verification."
  end

  def directive(%{response_profile: :social_chat}) do
    "Profile instruction: keep the reply natural, brief, and conversational. Do not turn casual chat into a technical workflow."
  end

  def directive(_personality) do
    "Profile instruction: answer directly in the smallest useful shape."
  end

  defp response_profile(features, decision, mood, wm_items, runtime_state, comprehension) do
    intent = map_get(features, :intent)
    mode = map_get(decision, :mode)
    action = map_get(decision, :action)
    tone = map_get(decision, :tone)
    tone_hint = map_get(mood, :tone_hint) || map_get(features, :tone_hint)
    profile = decision_profile(decision)
    guardrail? = truthy?(map_get(features, :guardrail?) || map_get(decision, :guardrail?))
    risk_bucket = map_get(features, :risk_bucket)

    cond do
      guardrail? or risk_bucket == :high or intent == :illicit_request or action == :safe_redirect ->
        :safety_redirect

      tone_hint == :deescalate or tone == :deescalate or high_vigilance?(runtime_state) ->
        :self_check

      self_state_care_request?(features) ->
        :self_state_boundary

      degraded_comprehension?(comprehension) or degraded_runtime?(runtime_state) or
          low_confidence?(features) ->
        :semantic_repair

      profile == :calm_explainer or mode == :explainer or brain_facing?(features, wm_items) ->
        :brain_explainer

      technical_work?(intent, mode) ->
        :technical_work

      intent in [:greeting, :gratitude, :smalltalk] or mode in [:chat, :scribe] ->
        :social_chat

      true ->
        :direct_answer
    end
  end

  defp temperament(:safety_redirect, _runtime, _features, _decision), do: :careful
  defp temperament(:self_check, _runtime, _features, _decision), do: :careful
  defp temperament(:semantic_repair, _runtime, _features, _decision), do: :careful
  defp temperament(:brain_explainer, _runtime, _features, _decision), do: :steady
  defp temperament(:self_state_boundary, _runtime, _features, _decision), do: :supportive
  defp temperament(:social_chat, runtime, _features, _decision) do
    if stable_and_curious?(runtime), do: :curious, else: :supportive
  end
  defp temperament(:technical_work, runtime, _features, _decision) do
    if stable_and_curious?(runtime), do: :curious, else: :direct
  end

  defp temperament(:direct_answer, runtime, _features, decision) do
    cond do
      stable_and_curious?(runtime) -> :curious
      map_get(decision, :tone) == :warm -> :supportive
      true -> :direct
    end
  end

  defp assertiveness(:safety_redirect, _features, _runtime), do: 0.75
  defp assertiveness(:self_check, _features, _runtime), do: 0.35
  defp assertiveness(:semantic_repair, _features, _runtime), do: 0.3
  defp assertiveness(:technical_work, features, runtime) do
    confidence = map_get(features, :conf, 0.6)
    clamp01(0.55 + confidence * 0.25 - uncertainty_penalty(runtime))
  end

  defp assertiveness(_profile, features, runtime) do
    confidence = map_get(features, :conf, 0.55)
    clamp01(0.45 + confidence * 0.35 - uncertainty_penalty(runtime))
  end

  defp curiosity(:safety_redirect, _runtime), do: 0.1
  defp curiosity(:self_check, _runtime), do: 0.2
  defp curiosity(:semantic_repair, _runtime), do: 0.25
  defp curiosity(:self_state_boundary, runtime), do: clamp01(0.25 + exploration(runtime) * 0.2)
  defp curiosity(:social_chat, runtime), do: clamp01(0.35 + exploration(runtime) * 0.25)
  defp curiosity(_profile, runtime), do: clamp01(0.25 + exploration(runtime) * 0.55)

  defp restraint(:safety_redirect, _runtime), do: 0.9
  defp restraint(:self_check, _runtime), do: 0.85
  defp restraint(:semantic_repair, _runtime), do: 0.8
  defp restraint(:self_state_boundary, runtime), do: clamp01(0.55 + inhibition(runtime) * 0.25)
  defp restraint(_profile, runtime), do: clamp01(0.35 + inhibition(runtime) * 0.45)

  defp warmth(:safety_redirect, _decision, _features), do: 0.25
  defp warmth(:self_check, _decision, _features), do: 0.35
  defp warmth(:semantic_repair, _decision, _features), do: 0.4
  defp warmth(:self_state_boundary, _decision, _features), do: 0.72
  defp warmth(:social_chat, _decision, _features), do: 0.75
  defp warmth(_profile, decision, _features) do
    case map_get(decision, :tone) do
      :warm -> 0.7
      :deescalate -> 0.35
      :firm -> 0.25
      _ -> 0.5
    end
  end

  defp self_check(:safety_redirect, _runtime), do: 0.55
  defp self_check(:self_check, _runtime), do: 0.9
  defp self_check(:semantic_repair, _runtime), do: 0.8
  defp self_check(:self_state_boundary, _runtime), do: 0.65
  defp self_check(_profile, runtime), do: if(high_vigilance?(runtime), do: 0.75, else: 0.25)

  defp abstraction(:brain_explainer, runtime), do: clamp01(0.45 + plasticity(runtime) * 0.35)
  defp abstraction(:technical_work, runtime), do: clamp01(0.25 + plasticity(runtime) * 0.25)
  defp abstraction(:semantic_repair, _runtime), do: 0.25
  defp abstraction(:self_state_boundary, _runtime), do: 0.25
  defp abstraction(_profile, runtime), do: clamp01(0.2 + plasticity(runtime) * 0.2)

  defp explanation_depth(:safety_redirect, _runtime), do: :brief
  defp explanation_depth(:self_check, _runtime), do: :brief
  defp explanation_depth(:semantic_repair, _runtime), do: :brief
  defp explanation_depth(:self_state_boundary, _runtime), do: :brief
  defp explanation_depth(:brain_explainer, runtime) do
    if stable_and_curious?(runtime), do: :deep, else: :normal
  end

  defp explanation_depth(:technical_work, runtime) do
    if stable_and_curious?(runtime), do: :deep, else: :normal
  end

  defp explanation_depth(_profile, _runtime), do: :brief

  defp reasons(profile, features, decision, runtime_state, comprehension) do
    []
    |> maybe_reason(profile == :safety_redirect, :safety_or_guardrail)
    |> maybe_reason(high_vigilance?(runtime_state), :elevated_vigilance)
    |> maybe_reason(degraded_runtime?(runtime_state), :lifg_degraded)
    |> maybe_reason(degraded_comprehension?(comprehension), :comprehension_degraded)
    |> maybe_reason(low_confidence?(features), :low_confidence)
    |> maybe_reason(self_state_care_request?(features), :self_state_care_request)
    |> maybe_reason(brain_facing?(features, []), :brain_facing_request)
    |> maybe_reason(technical_work?(map_get(features, :intent), map_get(decision, :mode)), :technical_work)
    |> default_reason(profile)
  end

  defp runtime_state(features, decision, mood, context) do
    map_get(context, :runtime_state) ||
      map_get(features, :runtime_state) ||
      map_get(decision, :runtime_state) ||
      map_get(mood, :runtime_state) ||
      %{}
  end

  defp comprehension(features, decision, context) do
    map_get(context, :comprehension) ||
      map_get(features, :comprehension) ||
      map_get(decision, :comprehension) ||
      %{}
  end

  defp decision_profile(decision) when is_map(decision) do
    case map_get(decision, :scores) do
      scores when is_map(scores) -> map_get(scores, :profile)
      _ -> map_get(decision, :profile)
    end
  end

  defp decision_profile(_), do: nil

  defp degraded_comprehension?(summary) when is_map(summary) do
    map_get(summary, :degraded?) == true
  end

  defp degraded_comprehension?(_), do: false

  defp degraded_runtime?(state) when is_map(state) do
    lifg = map_get(state, :lifg, %{})
    map_get(lifg, :degraded?) == true
  end

  defp degraded_runtime?(_), do: false

  defp high_vigilance?(state) when is_map(state) do
    mood = map_get(state, :mood, %{})
    ne = map_get(map_get(state, :neuromodulators, %{}), :norepinephrine)

    high_number?(map_get(mood, :vigilance), 0.8) or high_number?(ne, 0.8)
  end

  defp high_vigilance?(_), do: false

  defp low_confidence?(features) do
    map_get(features, :confidence_bucket) == :low or map_get(features, :conf, 1.0) < 0.35
  end

  defp self_state_care_request?(features) do
    text =
      features
      |> map_get(:text, "")
      |> to_string()
      |> String.downcase()

    Regex.match?(~r/\b(how are you|how do you feel|how are you feeling|are you ok|are you okay)\b/u, text) or
      Regex.match?(~r/\b(concerned|concerend|concern|worried)\b.{0,30}\b(you|symbrella)\b/u, text)
  end

  defp brain_facing?(features, wm_items) do
    text = map_get(features, :text, "")

    concept_text =
      wm_items
      |> Enum.map(&wm_item_term/1)
      |> Enum.join(" ")

    Regex.match?(
      ~r/\b(symbrella|brain|working memory|lifg|pmtg|hippocampus|amygdala|mood|self-state)\b/i,
      text <> " " <> concept_text
    )
  end

  defp technical_work?(intent, mode) do
    intent in @helpful_intents or mode == :collaborator
  end

  defp stable_and_curious?(runtime) do
    exploration(runtime) >= 0.65 and not high_vigilance?(runtime) and not degraded_runtime?(runtime)
  end

  defp uncertainty_penalty(runtime) do
    cond do
      degraded_runtime?(runtime) -> 0.25
      high_vigilance?(runtime) -> 0.2
      true -> 0.0
    end
  end

  defp exploration(runtime), do: runtime_value(runtime, :mood, :exploration, 0.5)
  defp inhibition(runtime), do: runtime_value(runtime, :mood, :inhibition, 0.5)
  defp plasticity(runtime), do: runtime_value(runtime, :mood, :plasticity, 0.5)

  defp runtime_value(runtime, group, key, default) do
    runtime
    |> map_get(group, %{})
    |> map_get(key, default)
    |> number(default)
    |> clamp01()
  end

  defp wm_item_term(item) when is_map(item) do
    payload = map_get(item, :payload)
    payload_lemma = if is_map(payload), do: map_get(payload, :lemma), else: nil
    normalize_term(payload_lemma || map_get(item, :lemma) || map_get(item, :id))
  end

  defp wm_item_term(other), do: normalize_term(other)

  defp normalize_term(nil), do: ""
  defp normalize_term(term) when is_binary(term), do: String.trim(term)
  defp normalize_term(term), do: term |> to_string() |> String.trim()

  defp maybe_reason(reasons, true, reason), do: reasons ++ [reason]
  defp maybe_reason(reasons, false, _reason), do: reasons

  defp default_reason([], profile), do: [profile]
  defp default_reason(reasons, _profile), do: reasons

  defp high_number?(value, threshold) when is_number(value), do: value >= threshold
  defp high_number?(_, _), do: false

  defp number(value, _default) when is_integer(value), do: value * 1.0
  defp number(value, _default) when is_float(value), do: value
  defp number(_value, default), do: default

  defp clamp01(value) when is_number(value), do: value |> max(0.0) |> min(1.0)
  defp clamp01(_), do: 0.0

  defp truthy?(true), do: true
  defp truthy?(_), do: false

  defp map_get(map, key, default \\ nil)

  defp map_get(map, key, default) when is_map(map) and is_atom(key) do
    Map.get(map, key, Map.get(map, Atom.to_string(key), default))
  end

  defp map_get(_, _, default), do: default
end
