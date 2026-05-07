defmodule Core.Response.Affect do
  @moduledoc """
  Prompt-facing simulated affect policy.

  Affect is not treated as human feeling or consciousness. It is a compact
  rendering of Brain/Core control signals into response tone: valence, arousal,
  uncertainty, pressure, warmth, and an expression label the LLM can follow.
  """

  @type label ::
          :steady_care
          | :curious_engaged
          | :settled_reserved
          | :uncertain_repairing
          | :strained_alert
          | :steady_engaged

  @type t :: %{
          label: label(),
          valence: float(),
          arousal: float(),
          confidence: float(),
          social_warmth: float(),
          uncertainty: float(),
          pressure: float(),
          expression: String.t(),
          reasons: [atom()]
        }

  @spec simulate(map(), map(), map(), map()) :: t()
  def simulate(personality, runtime_state, comprehension \\ %{}, features \\ %{}) do
    mood = map_get(runtime_state, :mood, %{})
    neuromodulators = map_get(runtime_state, :neuromodulators, %{})
    lifg = map_get(runtime_state, :lifg, %{})

    warmth = number(map_get(personality, :warmth), 0.5)
    restraint = number(map_get(personality, :restraint), 0.5)
    curiosity = number(map_get(personality, :curiosity), 0.35)
    self_check = number(map_get(personality, :self_check), 0.25)

    exploration = runtime_value(mood, :exploration, 0.5)
    inhibition = runtime_value(mood, :inhibition, 0.5)
    vigilance = runtime_value(mood, :vigilance, 0.5)
    plasticity = runtime_value(mood, :plasticity, 0.5)

    dopamine = runtime_value(neuromodulators, :dopamine, exploration)
    serotonin = runtime_value(neuromodulators, :serotonin, inhibition)
    glutamate = runtime_value(neuromodulators, :glutamate, plasticity)
    norepinephrine = runtime_value(neuromodulators, :norepinephrine, vigilance)

    confidence =
      lifg
      |> map_get(:confidence, map_get(features, :conf, 0.6))
      |> number(0.6)
      |> clamp01()

    uncertainty =
      uncertainty_score(lifg, comprehension, features)

    pressure =
      clamp01(norepinephrine * 0.35 + vigilance * 0.25 + self_check * 0.2 + uncertainty * 0.2)

    arousal =
      clamp01(glutamate * 0.3 + dopamine * 0.2 + norepinephrine * 0.25 + pressure * 0.25)

    social_warmth =
      clamp01(warmth * 0.65 + serotonin * 0.2 + restraint * 0.15)

    valence =
      clamp01(0.35 + social_warmth * 0.3 + confidence * 0.2 + dopamine * 0.15 - pressure * 0.2)

    label =
      label(%{
        profile: map_get(personality, :response_profile),
        curiosity: curiosity,
        restraint: restraint,
        social_warmth: social_warmth,
        arousal: arousal,
        uncertainty: uncertainty,
        pressure: pressure
      })

    %{
      label: label,
      valence: rounded(valence),
      arousal: rounded(arousal),
      confidence: rounded(confidence),
      social_warmth: rounded(social_warmth),
      uncertainty: rounded(uncertainty),
      pressure: rounded(pressure),
      expression: expression(label),
      reasons: reasons(label, personality, lifg, comprehension, pressure, uncertainty)
    }
  end

  @spec directive(t() | map()) :: String.t()
  def directive(%{label: label, expression: expression}) do
    "Affect instruction: render simulated affect as #{label} using #{expression}. Treat it as Symbrella's runtime-derived tone, not human emotion or consciousness."
  end

  def directive(_affect) do
    "Affect instruction: express runtime state only through tone, pacing, caution, curiosity, and social warmth."
  end

  defp label(%{profile: :self_state_boundary}), do: :steady_care

  defp label(%{pressure: pressure, uncertainty: uncertainty})
       when pressure >= 0.72 and uncertainty >= 0.55,
       do: :strained_alert

  defp label(%{uncertainty: uncertainty}) when uncertainty >= 0.55, do: :uncertain_repairing

  defp label(%{curiosity: curiosity, arousal: arousal}) when curiosity >= 0.55 and arousal >= 0.45,
    do: :curious_engaged

  defp label(%{restraint: restraint, arousal: arousal}) when restraint >= 0.65 and arousal <= 0.5,
    do: :settled_reserved

  defp label(_), do: :steady_engaged

  defp expression(:steady_care), do: "warmth, bounded honesty, and a brief self-state explanation"
  defp expression(:curious_engaged), do: "open curiosity, forward momentum, and grounded specificity"
  defp expression(:settled_reserved), do: "calm restraint, concise pacing, and careful wording"
  defp expression(:uncertain_repairing), do: "brief caveats, semantic repair, and targeted clarification only if needed"
  defp expression(:strained_alert), do: "short claims, active self-checking, and reduced assumption-making"
  defp expression(:steady_engaged), do: "steady engagement, natural warmth, and direct usefulness"

  defp uncertainty_score(lifg, comprehension, features) do
    degraded? = map_get(lifg, :degraded?) == true or map_get(comprehension, :degraded?) == true
    missing = scaled_count(map_get(lifg, :missing_candidates), 32)
    weak = scaled_count(map_get(lifg, :weak_decisions), 24)
    fallback = scaled_count(map_get(lifg, :fallback_winners), 16)
    low_confidence = if map_get(features, :confidence_bucket) == :low, do: 0.3, else: 0.0
    degraded = if degraded?, do: 0.25, else: 0.0

    clamp01(missing * 0.25 + weak * 0.25 + fallback * 0.2 + low_confidence + degraded)
  end

  defp reasons(label, personality, lifg, comprehension, pressure, uncertainty) do
    []
    |> maybe_reason(label == :steady_care, :self_state_care)
    |> maybe_reason(label == :curious_engaged, :exploration_high)
    |> maybe_reason(label == :settled_reserved, :inhibition_high)
    |> maybe_reason(pressure >= 0.7, :pressure_high)
    |> maybe_reason(uncertainty >= 0.55, :uncertainty_high)
    |> maybe_reason(map_get(lifg, :degraded?) == true, :lifg_degraded)
    |> maybe_reason(map_get(comprehension, :degraded?) == true, :comprehension_degraded)
    |> maybe_reason(map_get(personality, :response_profile) == :self_check, :self_check_profile)
    |> default_reason(label)
  end

  defp runtime_value(map, key, default) do
    map
    |> map_get(key, default)
    |> number(default)
    |> clamp01()
  end

  defp scaled_count(value, max_value) when is_number(value), do: clamp01(value / max_value)
  defp scaled_count(_value, _max_value), do: 0.0

  defp maybe_reason(reasons, true, reason), do: reasons ++ [reason]
  defp maybe_reason(reasons, false, _reason), do: reasons

  defp default_reason([], label), do: [label]
  defp default_reason(reasons, _label), do: reasons

  defp rounded(value), do: Float.round(clamp01(value), 2)

  defp number(value, _default) when is_integer(value), do: value * 1.0
  defp number(value, _default) when is_float(value), do: value
  defp number(_value, default), do: default

  defp clamp01(value) when is_number(value), do: value |> max(0.0) |> min(1.0)
  defp clamp01(_), do: 0.0

  defp map_get(map, key, default \\ nil)

  defp map_get(map, key, default) when is_map(map) and is_atom(key) do
    Map.get(map, key, Map.get(map, Atom.to_string(key), default))
  end

  defp map_get(_, _, default), do: default
end
