defmodule Core.Response.AffectPolicy do
  @moduledoc """
  Converts raw neuromodulator-inspired controls into prompt-facing behavior.

  The raw `%{da, "5ht", glu, ne}` object is an internal control signal. This
  module makes the LLM-facing boundary deterministic by deriving stable mood
  indices and a compact response policy from that object.
  """

  @type raw_modulators :: %{
          optional(:da) => float(),
          optional(String.t()) => float(),
          optional(:glu) => float(),
          optional(:ne) => float()
        }

  @type t :: %{
          raw_modulators: raw_modulators(),
          interpreted_state: map(),
          response_policy: map(),
          mood: map(),
          tone_hint: atom(),
          pressure_label: atom()
        }

  @spec from_modulators(map()) :: t()
  def from_modulators(modulators) when is_map(modulators) do
    levels = extract_levels(modulators)
    mood = derive_mood(levels)

    %{
      raw_modulators: levels,
      interpreted_state: interpreted_state(mood),
      response_policy: response_policy(mood),
      mood: mood,
      tone_hint: choose_tone(mood),
      pressure_label: pressure_label(mood)
    }
  end

  def from_modulators(_), do: from_modulators(%{})

  @doc """
  Normalizes either a raw modulator object or an existing mood snapshot.

  Existing derived fields are preserved when present; missing fields are filled
  from raw modulators when possible and otherwise default to neutral values.
  """
  @spec normalize(map()) :: map()
  def normalize(mood) when is_map(mood) do
    levels =
      extract_levels(first_map([mood, map_get(mood, :levels), map_get(mood, :raw_modulators)]))

    derived = extract_derived(mood, levels)

    tone_hint = map_get(mood, :tone_hint) || choose_tone(derived)
    pressure = map_get(mood, :pressure_label) || pressure_label(derived)
    raw_modulators = Map.merge(levels, map_get(mood, :raw_modulators, %{}))
    response_policy = Map.merge(response_policy(derived), map_get(mood, :response_policy, %{}))
    interpreted = Map.merge(interpreted_state(derived), map_get(mood, :interpreted_state, %{}))

    mood
    |> Map.put(:mood, derived)
    |> put_derived_top_level(derived)
    |> Map.put(:raw_modulators, raw_modulators)
    |> Map.put(:interpreted_state, interpreted)
    |> Map.put(:response_policy, response_policy)
    |> Map.put(:tone_hint, tone_hint)
    |> Map.put(:pressure_label, pressure)
    |> put_runtime_state(derived, raw_modulators, tone_hint, pressure)
  end

  def normalize(_), do: normalize(%{})

  defp extract_derived(mood, levels) do
    nested = map_get(mood, :mood, %{})

    derived =
      %{
        exploration: first_number([map_get(nested, :exploration), map_get(mood, :exploration)]),
        inhibition: first_number([map_get(nested, :inhibition), map_get(mood, :inhibition)]),
        vigilance: first_number([map_get(nested, :vigilance), map_get(mood, :vigilance)]),
        plasticity: first_number([map_get(nested, :plasticity), map_get(mood, :plasticity)])
      }

    if Enum.all?(Map.values(derived), &is_number/1) do
      Map.new(derived, fn {key, value} -> {key, clamp01(value)} end)
    else
      explicit =
        derived
        |> Enum.reject(fn {_key, value} -> is_nil(value) end)
        |> Enum.into(%{})

      Map.merge(derive_mood(levels), explicit)
    end
  end

  defp derive_mood(levels) do
    da = map_get(levels, :da, 0.5)
    serotonin = map_get(levels, :"5ht", 0.5)
    glu = map_get(levels, :glu, 0.5)
    ne = map_get(levels, :ne, 0.5)

    %{
      exploration: clamp01(0.6 * da + 0.4 * ne),
      inhibition: clamp01(serotonin),
      vigilance: clamp01(ne),
      plasticity: clamp01(0.5 * da + 0.5 * glu)
    }
  end

  defp extract_levels(map) when is_map(map) do
    %{
      :da => first_number([map_get(map, :da), map_get(map, :dopamine)], 0.5),
      "5ht" => first_number([map_get(map, :"5ht"), map_get(map, :serotonin)], 0.5),
      :glu => first_number([map_get(map, :glu), map_get(map, :glutamate)], 0.5),
      :ne => first_number([map_get(map, :ne), map_get(map, :norepinephrine)], 0.5)
    }
    |> Map.new(fn {key, value} -> {key, clamp01(value)} end)
  end

  defp extract_levels(_), do: extract_levels(%{})

  defp interpreted_state(mood) do
    %{
      exploration: band(map_get(mood, :exploration)),
      inhibition: band(map_get(mood, :inhibition)),
      plasticity: band(map_get(mood, :plasticity)),
      vigilance: band(map_get(mood, :vigilance))
    }
  end

  defp response_policy(mood) do
    exploration = map_get(mood, :exploration, 0.5)
    inhibition = map_get(mood, :inhibition, 0.5)
    plasticity = map_get(mood, :plasticity, 0.5)
    vigilance = map_get(mood, :vigilance, 0.5)

    %{
      social_state: social_state(inhibition, vigilance),
      tone: policy_tone(exploration, inhibition, vigilance),
      verbosity: verbosity(plasticity, vigilance),
      curiosity: curiosity(exploration, plasticity, vigilance),
      caution: caution(inhibition, vigilance),
      emotional_pressure: pressure(vigilance, inhibition),
      defensiveness: defensiveness(vigilance),
      next_action: next_action(inhibition, vigilance),
      avoid: avoid(inhibition, vigilance),
      self_check: self_check(vigilance),
      explanation_depth: explanation_depth(plasticity, exploration, vigilance),
      instruction: instruction(exploration, inhibition, plasticity, vigilance)
    }
  end

  defp policy_tone(exploration, inhibition, vigilance) do
    cond do
      vigilance >= 0.65 and inhibition < 0.6 -> :calm_accountable
      vigilance >= 0.72 and inhibition < 0.62 -> :careful_deescalating
      inhibition >= 0.62 and vigilance <= 0.55 and exploration >= 0.48 -> :warm_grounded
      inhibition >= 0.7 and exploration < 0.45 -> :calm_reserved
      exploration >= 0.68 and vigilance < 0.65 -> :curious_engaged
      true -> :steady_direct
    end
  end

  defp social_state(inhibition, vigilance) do
    if vigilance >= 0.65 and inhibition < 0.6 do
      :trust_rupture
    else
      :ordinary
    end
  end

  defp defensiveness(vigilance) when vigilance >= 0.65, do: :low
  defp defensiveness(_), do: :normal

  defp next_action(inhibition, vigilance) do
    if vigilance >= 0.65 and inhibition < 0.6 do
      :invite_correction
    else
      :answer_request
    end
  end

  defp avoid(inhibition, vigilance) do
    if vigilance >= 0.65 and inhibition < 0.6 do
      [:engineering_template, :dismissive_redirect, :argumentative_reply]
    else
      []
    end
  end

  defp verbosity(plasticity, vigilance) do
    cond do
      vigilance >= 0.72 -> :concise
      plasticity >= 0.66 -> :expanded_when_useful
      true -> :concise
    end
  end

  defp curiosity(exploration, plasticity, vigilance) do
    cond do
      vigilance >= 0.7 -> :low
      exploration >= 0.68 and plasticity >= 0.55 -> :active
      exploration >= 0.48 -> :light
      true -> :minimal
    end
  end

  defp caution(inhibition, vigilance) do
    cond do
      vigilance >= 0.72 -> :high
      vigilance >= 0.58 or inhibition >= 0.72 -> :elevated
      true -> :normal
    end
  end

  defp pressure(vigilance, inhibition) do
    cond do
      vigilance >= 0.72 and inhibition < 0.58 -> :high
      vigilance >= 0.58 -> :moderate
      true -> :low
    end
  end

  defp self_check(vigilance) when vigilance >= 0.72, do: :active
  defp self_check(vigilance) when vigilance >= 0.58, do: :light
  defp self_check(_), do: :normal

  defp explanation_depth(plasticity, exploration, vigilance) do
    cond do
      vigilance >= 0.72 -> :brief
      plasticity >= 0.65 and exploration >= 0.6 -> :deep_when_requested
      plasticity >= 0.52 -> :normal
      true -> :brief
    end
  end

  defp instruction(exploration, inhibition, plasticity, vigilance) do
    cond do
      vigilance >= 0.65 and inhibition < 0.6 ->
        "Respond with calm accountability; avoid defensiveness, engineering templates, and dismissive redirects; invite the user to identify the concrete miss."

      vigilance >= 0.72 ->
        "Respond carefully and briefly; self-check assumptions before adding detail."

      inhibition >= 0.62 and vigilance <= 0.55 and exploration >= 0.48 ->
        "Respond warmly, calmly, and briefly; keep urgency low and do not over-explain."

      exploration >= 0.68 and plasticity >= 0.55 ->
        "Respond with grounded curiosity and add a useful connection only when it helps."

      plasticity >= 0.65 ->
        "Respond clearly and adapt explanations to the user's frame without overreaching."

      true ->
        "Respond steadily and directly."
    end
  end

  defp choose_tone(mood) do
    exploration = map_get(mood, :exploration, 0.5)
    inhibition = map_get(mood, :inhibition, 0.5)
    vigilance = map_get(mood, :vigilance, 0.5)

    cond do
      vigilance >= 0.72 and inhibition < 0.62 -> :deescalate
      exploration >= 0.65 and inhibition >= 0.45 and vigilance < 0.65 -> :warm
      inhibition >= 0.7 and exploration < 0.45 and vigilance < 0.65 -> :cool
      true -> :neutral
    end
  end

  defp pressure_label(mood) do
    vigilance = map_get(mood, :vigilance, 0.5)
    inhibition = map_get(mood, :inhibition, 0.5)

    cond do
      vigilance >= 0.72 and inhibition < 0.58 -> :cautious_emergency_attention
      vigilance >= 0.58 -> :heightened_attention
      inhibition >= 0.68 -> :steady_restraint
      true -> :steady
    end
  end

  defp band(value) when is_number(value) do
    cond do
      value < 0.35 -> :low
      value < 0.5 -> :mild
      value < 0.68 -> :moderate
      true -> :high
    end
  end

  defp band(_), do: :moderate

  defp put_derived_top_level(map, derived) do
    Enum.reduce(derived, map, fn {key, value}, acc ->
      if Map.has_key?(acc, key) or Map.has_key?(acc, Atom.to_string(key)) do
        acc
      else
        Map.put(acc, key, value)
      end
    end)
  end

  defp put_runtime_state(map, derived, raw, tone_hint, pressure) do
    neuromodulators = %{
      dopamine: map_get(raw, :da),
      serotonin: map_get(raw, :"5ht"),
      glutamate: map_get(raw, :glu),
      norepinephrine: map_get(raw, :ne)
    }

    runtime_state =
      map_get(map, :runtime_state, %{})
      |> ensure_map()
      |> Map.put_new(:mood, derived)
      |> Map.put_new(:neuromodulators, neuromodulators)
      |> Map.put_new(:tone_hint, tone_hint)
      |> Map.put_new(:pressure_label, pressure)

    Map.put(map, :runtime_state, runtime_state)
  end

  defp first_map(values) do
    Enum.find(values, %{}, fn
      %{} = map -> map_size(map) > 0
      _ -> false
    end)
  end

  defp first_number(values, default \\ nil) do
    Enum.find_value(values, default, fn
      value when is_number(value) -> value * 1.0
      value when is_binary(value) ->
        case Float.parse(String.trim(value)) do
          {number, ""} -> number
          _ -> nil
        end

      _ ->
        nil
    end)
  end

  defp clamp01(value) when is_number(value), do: value |> max(0.0) |> min(1.0)
  defp clamp01(_), do: 0.5

  defp ensure_map(map) when is_map(map), do: map
  defp ensure_map(_), do: %{}

  defp map_get(map, key, default \\ nil)

  defp map_get(map, key, default) when is_map(map) and is_atom(key) do
    Map.get(map, key, Map.get(map, Atom.to_string(key), default))
  end

  defp map_get(map, key, default) when is_map(map) and is_binary(key) do
    Map.get(map, key, default)
  end

  defp map_get(_, _, default), do: default
end
