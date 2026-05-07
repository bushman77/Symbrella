defmodule Core.Response.PersonalityTest do
  use ExUnit.Case, async: true

  alias Core.Response.Personality

  test "high norepinephrine and degraded LIFG choose self-checking careful behavior" do
    personality =
      Personality.decide(
        %{intent: :question, conf: 0.7},
        %{tone: :neutral, mode: :coach},
        %{},
        [],
        %{
          runtime_state: %{
            mood: %{vigilance: 0.86},
            neuromodulators: %{norepinephrine: 0.84},
            lifg: %{degraded?: true}
          }
        }
      )

    assert personality.response_profile == :self_check
    assert personality.temperament == :careful
    assert personality.self_check >= 0.9
    assert :elevated_vigilance in personality.reasons
  end

  test "high exploration with stable LIFG becomes curious but still bounded" do
    personality =
      Personality.decide(
        %{intent: :question, conf: 0.8},
        %{tone: :warm, mode: :chat},
        %{},
        [],
        %{
          runtime_state: %{
            mood: %{exploration: 0.82, inhibition: 0.4, vigilance: 0.25, plasticity: 0.72},
            neuromodulators: %{dopamine: 0.8, norepinephrine: 0.25},
            lifg: %{degraded?: false}
          }
        }
      )

    assert personality.response_profile == :social_chat
    assert personality.temperament == :curious
    assert personality.curiosity > personality.restraint
    assert personality.explanation_depth == :brief
  end

  test "high inhibition favors restrained semantic repair when comprehension is degraded" do
    personality =
      Personality.decide(
        %{intent: :question, conf: 0.5},
        %{tone: :neutral, mode: :coach},
        %{},
        [],
        %{
          runtime_state: %{
            mood: %{exploration: 0.35, inhibition: 0.88, vigilance: 0.4, plasticity: 0.3},
            neuromodulators: %{serotonin: 0.88}
          },
          comprehension: %{degraded?: true}
        }
      )

    assert personality.response_profile == :semantic_repair
    assert personality.temperament == :careful
    assert personality.restraint >= 0.8
    assert personality.assertiveness <= 0.3
  end

  test "WM brain focus chooses brain explainer without grandiose behavior" do
    personality =
      Personality.decide(
        %{intent: :question, text: "how does your brain state affect this?"},
        %{tone: :warm, mode: :chat},
        %{},
        [%{id: "symbrella|system|core", payload: %{lemma: "symbrella"}}],
        %{runtime_state: %{mood: %{exploration: 0.5, vigilance: 0.3}, lifg: %{degraded?: false}}}
      )

    assert personality.response_profile == :brain_explainer
    assert personality.temperament == :steady
    assert personality.abstraction > 0.2
    assert Personality.directive(personality) =~ "Avoid biological equivalence or consciousness claims"
  end

  test "care about Symbrella's state chooses bounded self-state behavior despite noisy LIFG" do
    personality =
      Personality.decide(
        %{intent: :ask, text: "how are you feeling, im concerned about you.", conf: 0.7},
        %{tone: :warm, mode: :chat},
        %{},
        [],
        %{
          runtime_state: %{
            mood: %{exploration: 0.4, inhibition: 0.6, vigilance: 0.4, plasticity: 0.4},
            neuromodulators: %{norepinephrine: 0.4},
            lifg: %{
              degraded?: true,
              confidence: 0.7,
              missing_candidates: 11,
              weak_decisions: 8,
              fallback_winners: 3
            }
          }
        }
      )

    assert personality.response_profile == :self_state_boundary
    assert personality.temperament == :supportive
    assert personality.warmth > personality.assertiveness
    assert :self_state_care_request in personality.reasons

    assert Personality.directive(personality) =~
             "does not have human feelings or consciousness"

    assert Personality.directive(personality) =~ "Do not call yourself a generic tool"
    assert Personality.directive(personality) =~ "do not end with a generic service offer"
  end

  test "misspelled concern still chooses bounded self-state behavior" do
    personality =
      Personality.decide(
        %{intent: :ask, text: "im concerend about you", conf: 0.7},
        %{tone: :warm, mode: :chat},
        %{},
        [],
        %{runtime_state: %{lifg: %{confidence: 0.7, fallback_winners: 3, missing_candidates: 9}}}
      )

    assert personality.response_profile == :self_state_boundary
    assert :self_state_care_request in personality.reasons
  end
end
