defmodule Core.Response.SelfStateSummaryTest do
  use ExUnit.Case, async: true

  alias Core.Response.SelfStateSummary

  test "summarize/1 formats mood, self portrait, working memory, and lifg state" do
    text =
      SelfStateSummary.summarize(%{
        mood: %{
          mood: %{exploration: 0.7, inhibition: 0.6, vigilance: 0.3, plasticity: 0.8},
          levels: %{da: 0.65, "5ht": 0.6, glu: 0.95, ne: 0.3},
          tone_hint: :warm
        },
        self_portrait: %{
          traits: %{
            curiosity_bias: 0.62,
            confidence_baseline: 0.55,
            stability: 0.7,
            novelty_seeking: 0.66,
            risk_aversion: 0.42
          },
          patterns: %{wm_updates: 3, lifg_payload_gaps: 1, chargram_violations: 0},
          last_events: [%{event: [:brain, :wm, :update]}]
        },
        wm: %{
          wm: [
            %{id: "self portrait|phrase|core", payload: %{lemma: "self portrait"}},
            %{id: "working memory|phrase|core", payload: %{lemma: "working memory"}}
          ],
          cfg: %{capacity: 8}
        },
        lifg: %{
          running?: true,
          state: %{
            last: %{
              intent: :question,
              confidence: 0.81,
              choices: [%{chosen_id: "self|noun|0"}],
              audit: %{weak_decisions: 1, fallback_winners: 0}
            }
          }
        }
      })

    assert text =~ "Current live summary:"
    assert text =~ "Mood indices are exploration=0.7"
    assert text =~ "raw modulators are da=0.65"
    assert text =~ "SelfPortrait traits are curiosity_bias=0.62"
    assert text =~ "active patterns are wm_updates=3, lifg_payload_gaps=1"
    assert text =~ "Working memory: size=2, capacity=8"
    assert text =~ "focus=self portrait, working memory"
    assert text =~ "LIFG snapshot: running=true"
  end

  test "self_portrait_answer/0 includes access paths and bounded self-state wording" do
    text = SelfStateSummary.self_portrait_answer()

    assert text =~ "/brain"
    assert text =~ "Brain.SelfPortrait.snapshot()"
    assert text =~ "Brain.Introspect.snapshot(:self_portrait)"
    assert text =~ "software self-state snapshot"
    assert text =~ "not a claim of human consciousness or feelings"
  end
end
