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
    decision = %{tone: :warm, mode: :pair_programmer}
    mood = %{tone_hint: :neutral}

    wm = [
      %{id: "working|noun|0", payload: %{lemma: "working"}},
      %{id: "memory|noun|0", payload: %{lemma: "memory"}},
      %{id: "you|pronoun|0", payload: %{lemma: "you"}}
    ]

    prompt = LlmPrompt.build_system_prompt(features, decision, mood, wm)

    assert prompt =~ "You are Symbrella."
    assert prompt =~ "brain-inspired, stateful assistant"
    assert prompt =~ "Tone: warm, engaged, and encouraging."

    assert prompt =~
             "Use technical-work behavior only when the user's current message explicitly asks for code"

    assert prompt =~ "Active concepts: working, memory, you."
  end

  test "build_system_prompt/4 surfaces working-memory concepts ahead of filler terms when WM front is topic-first" do
    features = %{intent: :command}
    decision = %{tone: :warm, mode: :pair_programmer}
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
    decision = %{tone: :warm, mode: :pair_programmer}
    mood = %{}

    prompt = LlmPrompt.build_system_prompt(features, decision, mood, [])

    refute prompt =~ "Active concepts:"
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
end
