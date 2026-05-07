defmodule Core.Response.AffectTest do
  use ExUnit.Case, async: true

  alias Core.Response.Affect

  test "self-state boundary renders warm bounded care instead of human feeling" do
    affect =
      Affect.simulate(
        %{
          response_profile: :self_state_boundary,
          warmth: 0.72,
          restraint: 0.7,
          curiosity: 0.33,
          self_check: 0.65
        },
        %{
          mood: %{exploration: 0.4, inhibition: 0.6, vigilance: 0.4, plasticity: 0.4},
          neuromodulators: %{dopamine: 0.4, serotonin: 0.6, glutamate: 0.4, norepinephrine: 0.4},
          lifg: %{confidence: 0.9, missing_candidates: 16, weak_decisions: 6, fallback_winners: 0}
        }
      )

    assert affect.label == :steady_care
    assert affect.social_warmth >= 0.65
    assert affect.confidence == 0.9
    assert affect.expression =~ "bounded honesty"
    assert Affect.directive(affect) =~ "not human emotion or consciousness"
  end

  test "high pressure and uncertainty render strained alert expression" do
    affect =
      Affect.simulate(
        %{response_profile: :self_check, warmth: 0.35, restraint: 0.85, curiosity: 0.2, self_check: 0.9},
        %{
          mood: %{vigilance: 0.9, inhibition: 0.75, exploration: 0.2, plasticity: 0.3},
          neuromodulators: %{norepinephrine: 0.92, serotonin: 0.75, glutamate: 0.55},
          lifg: %{degraded?: true, confidence: 0.35, missing_candidates: 28, weak_decisions: 20, fallback_winners: 12}
        },
        %{degraded?: true}
      )

    assert affect.label == :strained_alert
    assert affect.pressure >= 0.7
    assert affect.uncertainty >= 0.55
    assert :pressure_high in affect.reasons
    assert :uncertainty_high in affect.reasons
  end

  test "stable exploration renders curious engagement" do
    affect =
      Affect.simulate(
        %{response_profile: :social_chat, warmth: 0.75, restraint: 0.45, curiosity: 0.7, self_check: 0.2},
        %{
          mood: %{exploration: 0.86, inhibition: 0.35, vigilance: 0.25, plasticity: 0.7},
          neuromodulators: %{dopamine: 0.85, serotonin: 0.45, glutamate: 0.65, norepinephrine: 0.2},
          lifg: %{degraded?: false, confidence: 0.82, missing_candidates: 1, weak_decisions: 1, fallback_winners: 0}
        }
      )

    assert affect.label == :curious_engaged
    assert affect.valence > affect.pressure
    assert affect.arousal >= 0.45
    assert affect.expression =~ "open curiosity"
  end
end
