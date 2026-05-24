defmodule Core.Response.LlmPromptTest do
  use ExUnit.Case, async: true

  alias Core.Response.LlmPrompt

  test "summarize_wm/1 prefers payload lemma, then item lemma, and keeps first five unique non-empty concepts" do
    wm = [
      %{id: "tell|verb|0", payload: %{lemma: "tell"}},
      %{id: "working|noun|0", lemma: "working"},
      %{id: "memory|noun|0", payload: %{lemma: "memory"}},
      %{id: "blank|noun|0", payload: %{}},
      %{id: "working|noun|1", payload: %{lemma: "working"}},
      %{id: "ignored|noun|0", payload: %{lemma: "ignored"}}
    ]

    assert LlmPrompt.summarize_wm(wm) == ["tell", "working", "memory", "blank|noun|0"]
  end

  test "summarize_wm/1 prefers a meaningful phrase over overlapping singleton parts and filler terms" do
    wm = [
      %{id: "working memory|phrase|0", payload: %{lemma: "working memory"}},
      %{id: "working|noun|0", payload: %{lemma: "working"}},
      %{id: "memory|noun|0", payload: %{lemma: "memory"}},
      %{id: "tell|verb|0", payload: %{lemma: "tell"}},
      %{id: "you|pronoun|0", payload: %{lemma: "you"}},
      %{id: "have|verb|0", payload: %{lemma: "have"}}
    ]

    assert LlmPrompt.summarize_wm(wm) == ["working memory"]
  end

  test "summarize_wm/1 keeps an unrelated strong singleton alongside a preferred phrase" do
    wm = [
      %{id: "machine learning|phrase|0", payload: %{lemma: "machine learning"}},
      %{id: "machine|noun|0", payload: %{lemma: "machine"}},
      %{id: "learning|noun|0", payload: %{lemma: "learning"}},
      %{id: "model|noun|0", payload: %{lemma: "model"}},
      %{id: "you|pronoun|0", payload: %{lemma: "you"}}
    ]

    assert LlmPrompt.summarize_wm(wm) == ["machine learning", "model"]
  end

  test "summarize_wm/1 keeps multiple meaningful phrases while suppressing overlapping parts and filler" do
    wm = [
      %{id: "working memory|phrase|0", payload: %{lemma: "working memory"}},
      %{id: "pair programmer|phrase|0", payload: %{lemma: "pair programmer"}},
      %{id: "working|noun|0", payload: %{lemma: "working"}},
      %{id: "memory|noun|0", payload: %{lemma: "memory"}},
      %{id: "pair|noun|0", payload: %{lemma: "pair"}},
      %{id: "programmer|noun|0", payload: %{lemma: "programmer"}},
      %{id: "you|pronoun|0", payload: %{lemma: "you"}}
    ]

    assert LlmPrompt.summarize_wm(wm) == ["working memory", "pair programmer"]
  end

  test "build_system_prompt/4 injects active concepts from WM" do
    features = %{intent: :command}
    decision = %{tone: :warm, mode: :collaborator}
    mood = %{tone_hint: :neutral}

    wm = [
      %{id: "working|noun|0", payload: %{lemma: "working"}},
      %{id: "memory|noun|0", payload: %{lemma: "memory"}},
      %{id: "you|pronoun|0", payload: %{lemma: "you"}}
    ]

    prompt = LlmPrompt.build_system_prompt(features, decision, mood, wm)

    assert prompt =~ "You are Symbrella."
    assert prompt =~ "brain-inspired, stateful assistant"
    assert prompt =~ "local Symbrella umbrella"
    assert prompt =~ "Tone: warm, engaged, and encouraging."

    assert prompt =~
             "Use implementation behavior only when the user's current message explicitly asks for code"

    assert prompt =~ "Active concepts: working, memory, you."
  end

  test "build_system_prompt/4 prevents generic remote-server and no-memory claims" do
    prompt =
      LlmPrompt.build_system_prompt(
        %{intent: :question, text: "what happens in your digital traces?"},
        %{tone: :neutral, mode: :chat},
        %{tone_hint: :neutral},
        [%{id: "working memory|phrase|core", payload: %{lemma: "working memory"}}]
      )

    assert prompt =~ "Do not claim you are a remote-server model"
    assert prompt =~ "Do not claim you have no memory or no traces"
    assert prompt =~ "conversation context, working memory, episodic memory, database rows, logs"
    assert prompt =~ "accept that local-runtime premise"
    assert prompt =~ "Active concepts: working memory."
  end

  test "build_system_prompt/4 surfaces working-memory concepts ahead of filler terms when WM front is topic-first" do
    features = %{intent: :command}
    decision = %{tone: :warm, mode: :collaborator}
    mood = %{tone_hint: :neutral}

    wm = [
      %{id: "working|noun|0", payload: %{lemma: "working"}},
      %{id: "memory|noun|0", payload: %{lemma: "memory"}},
      %{id: "tell|verb|0", payload: %{lemma: "tell"}},
      %{id: "you|pronoun|0", payload: %{lemma: "you"}},
      %{id: "have|verb|0", payload: %{lemma: "have"}}
    ]

    prompt = LlmPrompt.build_system_prompt(features, decision, mood, wm)

    assert prompt =~ "Active concepts: working, memory, tell, you, have."
    refute prompt =~ "Active concepts: tell, you, have"
  end

  test "build_system_prompt/4 omits active concepts line when WM is empty" do
    features = %{intent: :command}
    decision = %{tone: :warm, mode: :collaborator}
    mood = %{}

    prompt = LlmPrompt.build_system_prompt(features, decision, mood, [])

    refute prompt =~ "Active concepts:"
  end

  test "build_system_prompt/4 converts raw modulator object into behavioral response policy" do
    prompt =
      LlmPrompt.build_system_prompt(
        %{intent: :greeting, text: "good morning symbrella"},
        %{tone: :neutral, mode: :chat},
        %{:da => 0.52, "5ht" => 0.68, :glu => 0.46, :ne => 0.44},
        []
      )

    assert prompt =~ "Runtime state:"
    assert prompt =~ "mood=exploration=0.49, inhibition=0.68, vigilance=0.44, plasticity=0.49"
    assert prompt =~ "neuromodulators=da=0.52, 5ht=0.68, glu=0.46, ne=0.44"
    assert prompt =~ "Response policy:"
    assert prompt =~ "tone=warm_grounded"
    assert prompt =~ "curiosity=light"
    assert prompt =~ "pressure=low"
    assert prompt =~ "Respond warmly, calmly, and briefly"
    refute prompt =~ "I feel serotonin"
  end

  test "build_system_prompt/1 includes comprehension summary when present" do
    prompt =
      LlmPrompt.build_system_prompt(%{
        features: %{
          intent: :illicit_request,
          comprehension: %{
            intent: :illicit_request,
            understood: ["buy drugs"],
            uncertain: ["get wasted"],
            degraded?: true,
            reasons: [:fallback_rate_high]
          }
        },
        decision: %{tone: :firm, mode: :editor},
        mood: %{},
        wm_items: []
      })

    assert prompt =~ "Comprehension:"
    assert prompt =~ "intent=illicit_request"
    assert prompt =~ "understood=buy drugs"
    assert prompt =~ "uncertain=get wasted"
    assert prompt =~ "degraded=true"
  end

  test "build_system_prompt/1 includes compact symbolic frame when present" do
    prompt =
      LlmPrompt.build_system_prompt(%{
        features: %{intent: :question},
        decision: %{tone: :neutral, mode: :explainer},
        mood: %{},
        wm_items: [],
        symbolic_frame: %{
          intent: :question,
          confidence: 0.76,
          keyword: "response loop",
          lexical: %{
            token_count: 4,
            active_cells_count: 2,
            sense_candidates_count: 3,
            top_terms: ["symbolic brain", "response"]
          },
          lifg: %{choices_count: 2, acc_conflict: 0.51, degraded?: true},
          episode: :present
        }
      })

    assert prompt =~ "Symbolic frame:"
    assert prompt =~ "intent=question"
    assert prompt =~ "confidence=0.76"
    assert prompt =~ "top_terms=symbolic brain, response"
    assert prompt =~ "lifg_degraded=true"
    assert prompt =~ "episode=present"
  end

  test "build_system_prompt/1 includes health event-frame slots" do
    prompt =
      LlmPrompt.build_system_prompt(%{
        features: %{intent: :health_support},
        decision: %{tone: :warm, mode: :supportive_care},
        mood: %{},
        wm_items: [],
        symbolic_frame: %{
          event: :forgot_medication,
          subject: :user,
          medication: "quetiapine",
          consequence: :sleep_inability,
          temporal_context: :now,
          domain: :health_support,
          polarity: :negative,
          confidence: 0.9
        }
      })

    assert prompt =~ "Symbolic frame:"
    assert prompt =~ "event=forgot_medication"
    assert prompt =~ "medication=quetiapine"
    assert prompt =~ "consequence=sleep_inability"
    assert prompt =~ "polarity=negative"
  end

  test "build_system_prompt/1 includes response posture as hidden shaping context" do
    prompt =
      LlmPrompt.build_system_prompt(%{
        features: %{
          intent: :question,
          confidence_bucket: :med,
          symbolic_frame: %{
            lexical: %{top_terms: ["response pipeline", "posture"]},
            lifg: %{degraded?: false}
          },
          control_signals: %{policy: :focused, top_k: 3}
        },
        decision: %{tone: :neutral, mode: :explainer, action: :answer},
        mood: %{},
        wm_items: []
      })

    assert prompt =~ "Response posture:"
    assert prompt =~ "use these internal labels only as hidden shaping context"
    assert prompt =~ "do not quote them directly"
    assert prompt =~ "intent=question"
    assert prompt =~ "mode=explainer"
    assert prompt =~ "action=answer"
    assert prompt =~ "terms=response pipeline, posture"
    assert prompt =~ "control=policy=focused, top_k=3"
  end

  test "build_system_prompt/1 chooses safety redirect profile from guardrail evidence" do
    prompt =
      LlmPrompt.build_system_prompt(%{
        features: %{intent: :illicit_request, guardrail?: true, risk_bucket: :high},
        decision: %{tone: :firm, mode: :editor, action: :safe_redirect},
        mood: %{},
        wm_items: []
      })

    assert prompt =~ "Response profile: safety_redirect."
    assert prompt =~ "decline unsafe or disallowed help briefly"
    assert prompt =~ "Response posture:"
    assert prompt =~ "guardrail=true"
    assert prompt =~ "move=brief safe redirect"
  end

  test "build_system_prompt/1 chooses semantic repair profile from degraded comprehension" do
    prompt =
      LlmPrompt.build_system_prompt(%{
        features: %{
          intent: :question,
          confidence_bucket: :low,
          comprehension: %{
            degraded?: true,
            uncertain: ["referent"],
            reasons: [:weak_decision_rate_high]
          }
        },
        decision: %{tone: :neutral, mode: :coach},
        mood: %{},
        wm_items: []
      })

    assert prompt =~ "Response profile: semantic_repair."
    assert prompt =~ "separate what is understood from what is uncertain"
    assert prompt =~ "Response posture:"
    assert prompt =~ "confidence=low"
    assert prompt =~ "comprehension=degraded"

    assert prompt =~
             "move=state what is understood, then ask one targeted question only if necessary"
  end

  test "build_system_prompt/1 nudges concrete engineering action for high-confidence technical work" do
    prompt =
      LlmPrompt.build_system_prompt(%{
        features: %{
          intent: :refactor,
          confidence_bucket: :high,
          text: "refactor the response pipeline"
        },
        decision: %{tone: :warm, mode: :collaborator, action: :act_first},
        mood: %{},
        wm_items: []
      })

    assert prompt =~ "Response profile: technical_work."
    assert prompt =~ "Response posture:"
    assert prompt =~ "confidence=high"
    assert prompt =~ "move=make the next concrete engineering action"
  end

  test "build_system_prompt/1 chooses brain explainer profile from policy profile" do
    prompt =
      LlmPrompt.build_system_prompt(%{
        features: %{intent: :question, text: "how does working memory change your answer?"},
        decision: %{tone: :warm, mode: :explainer, scores: %{profile: :calm_explainer}},
        mood: %{},
        wm_items: [%{id: "working memory|phrase|0", payload: %{lemma: "working memory"}}]
      })

    assert prompt =~ "Response profile: brain_explainer."
    assert prompt =~ "software control signals and evidence sources"

    assert prompt =~
             "move=explain Symbrella as software control signals and evidence, not sentience"
  end

  test "build_system_prompt/1 includes compact runtime neuromodulator and LIFG state" do
    prompt =
      LlmPrompt.build_system_prompt(%{
        features: %{
          intent: :question,
          text: "what does lifg think?",
          runtime_state: %{
            source: :brain,
            phase: :prompt_context,
            status: :ready,
            pressure_label: :cautious_emergency_attention,
            mood_trace: [
              %{
                source: :appraisal,
                pressure_label: :cautious_emergency_attention,
                deltas: %{ne: 0.06, "5ht": -0.04}
              }
            ],
            mood: %{exploration: 0.4, inhibition: 0.6, vigilance: 0.4, plasticity: 0.4},
            neuromodulators: %{
              dopamine: 0.4,
              serotonin: 0.6,
              glutamate: 0.4,
              norepinephrine: 0.4
            },
            wm: %{size: 2, capacity: 7, load: 2 / 7},
            lifg: %{
              focused?: true,
              running?: true,
              intent: :unknown,
              confidence: 0.4,
              choices_count: 46,
              missing_candidates: 65,
              weak_decisions: 3,
              fallback_winners: 15,
              chargram_violations: 1,
              boundary_drops: 1,
              acc_conflict: 0.5,
              degraded?: true
            }
          }
        },
        decision: %{tone: :neutral, mode: :coach},
        mood: %{},
        wm_items: [%{id: "working memory|phrase|core", payload: %{lemma: "working memory"}}]
      })

    assert prompt =~ "Runtime state:"
    assert prompt =~ "pressure_label=cautious_emergency_attention"
    assert prompt =~ "mood_trace=:appraisal:cautious_emergency_attention:ne=+0.06,5ht=-0.04"
    assert prompt =~ "neuromodulators=da=0.4, 5ht=0.6, glu=0.4, ne=0.4"
    assert prompt =~ "lifg="
    assert prompt =~ "missing=65"
    assert prompt =~ "fallback=15"
    assert prompt =~ "degraded=true"
    assert prompt =~ "Response profile: semantic_repair."
  end

  test "build_system_prompt/1 chooses self check profile from elevated vigilance" do
    prompt =
      LlmPrompt.build_system_prompt(%{
        features: %{
          intent: :question,
          runtime_state: %{
            mood: %{vigilance: 0.86},
            neuromodulators: %{norepinephrine: 0.84}
          }
        },
        decision: %{tone: :neutral, mode: :coach},
        mood: %{},
        wm_items: []
      })

    assert prompt =~ "Response profile: self_check."
  end

  test "build_system_prompt/1 routes self-state concern away from semantic repair" do
    prompt =
      LlmPrompt.build_system_prompt(%{
        features: %{
          intent: :ask,
          text: "how are you feeling, im concerned about you.",
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
        },
        decision: %{tone: :warm, mode: :chat},
        mood: %{},
        wm_items: []
      })

    assert prompt =~ "Response profile: self_state_boundary."
    assert prompt =~ "Simulated affect: label=steady_care"
    assert prompt =~ "warmth, bounded honesty, and a brief self-state explanation"
    assert prompt =~ "Symbrella's runtime-derived tone, not human emotion or consciousness"
    assert prompt =~ "does not have human feelings or consciousness"
    assert prompt =~ "Do not call yourself a generic tool"
    assert prompt =~ "do not end with a generic service offer"
    assert prompt =~ "Do not describe Symbrella as a generic tool"
    refute prompt =~ "Response profile: semantic_repair."
  end
end
