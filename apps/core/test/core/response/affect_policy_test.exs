defmodule Core.Response.AffectPolicyTest do
  use ExUnit.Case, async: true

  alias Core.Response.AffectPolicy

  test "raw modulator object becomes derived mood indices and response policy" do
    policy =
      AffectPolicy.from_modulators(%{
        :da => 0.52,
        "5ht" => 0.68,
        :glu => 0.46,
        :ne => 0.44
      })

    assert policy.raw_modulators == %{:da => 0.52, "5ht" => 0.68, :glu => 0.46, :ne => 0.44}

    assert policy.mood == %{
             exploration: 0.488,
             inhibition: 0.68,
             vigilance: 0.44,
             plasticity: 0.49
           }

    assert policy.interpreted_state == %{
             exploration: :mild,
             inhibition: :high,
             plasticity: :mild,
             vigilance: :mild
           }

    assert policy.response_policy.tone == :warm_grounded
    assert policy.response_policy.verbosity == :concise
    assert policy.response_policy.curiosity == :light
    assert policy.response_policy.caution == :normal
    assert policy.response_policy.emotional_pressure == :low

    assert policy.response_policy.instruction ==
             "Respond warmly, calmly, and briefly; keep urgency low and do not over-explain."
  end

  test "normalizes raw object into prompt-safe runtime state" do
    normalized = AffectPolicy.normalize(%{:da => 0.4, "5ht" => 0.5, :glu => 0.55, :ne => 0.72})

    assert normalized.mood.vigilance == 0.72
    assert_in_delta normalized.mood.plasticity, 0.475, 0.001
    assert normalized.tone_hint == :deescalate
    assert normalized.response_policy.tone == :calm_accountable
    assert normalized.runtime_state.mood == normalized.mood

    assert normalized.runtime_state.neuromodulators == %{
             dopamine: 0.4,
             serotonin: 0.5,
             glutamate: 0.55,
             norepinephrine: 0.72
           }
  end

  test "normalizes existing derived mood without replacing it with neutral raw defaults" do
    normalized =
      AffectPolicy.normalize(%{
        mood: %{exploration: 0.82, inhibition: 0.5, vigilance: 0.3, plasticity: 0.72}
      })

    assert normalized.mood.exploration == 0.82
    assert normalized.mood.plasticity == 0.72
    assert normalized.response_policy.tone == :curious_engaged
    assert normalized.response_policy.curiosity == :active
    assert normalized.response_policy.explanation_depth == :deep_when_requested
  end

  test "high norepinephrine with reduced serotonin maps to trust repair policy" do
    policy =
      AffectPolicy.from_modulators(%{
        :da => 0.48,
        "5ht" => 0.56,
        :glu => 0.54,
        :ne => 0.69
      })

    assert policy.response_policy.social_state == :trust_rupture
    assert policy.response_policy.tone == :calm_accountable
    assert policy.response_policy.defensiveness == :low
    assert policy.response_policy.next_action == :invite_correction
    assert :engineering_template in policy.response_policy.avoid
    assert policy.response_policy.instruction =~ "calm accountability"
  end
end
