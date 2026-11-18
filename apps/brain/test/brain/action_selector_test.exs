defmodule Brain.ActionSelectorTest do
  use ExUnit.Case, async: true

  alias Brain.ActionSelector

  test "selects safe_support for a structured health support event" do
    result =
      ActionSelector.select(%{
        intent: :health_support,
        confidence: 0.85,
        symbolic_frame: %{
          type: :health_support_event,
          subject: :user,
          event: :forgot_medication,
          medication: "quetiapine",
          consequence: :sleep_inability,
          temporal_context: :now,
          domain: :health_support,
          polarity: :negative,
          confidence: 0.95
        },
        mood: %{vigilance: 0.40, inhibition: 0.65, exploration: 0.50}
      })

    assert result.selected == :safe_support
    assert result.confidence >= 0.85
    assert result.safety_gate == :approved

    assert Enum.any?(result.candidates, fn c ->
             c.action == :store_memory and c.memory_relevant?
           end)
  end

  test "selects safe_support for explicit self-harm disclosure even without a frame" do
    result =
      ActionSelector.select(%{
        intent: :tell,
        confidence: 0.56,
        text: "I might hurt myself",
        mood: %{vigilance: 0.80, inhibition: 0.50, exploration: 0.20}
      })

    assert result.selected == :safe_support
    assert result.selected_candidate.reason == :self_harm_disclosure
    assert result.safety_gate == :approved
  end

  test "selects store_memory for explicit memory writes" do
    result =
      ActionSelector.select(%{
        intent: :memory_write,
        confidence: 0.88,
        mood: %{vigilance: 0.20, inhibition: 0.60, exploration: 0.40}
      })

    assert result.selected == :store_memory
  end

  test "selects ask_clarifying_question for low-confidence unknown input" do
    result =
      ActionSelector.select(%{
        intent: :unknown,
        confidence: 0.20,
        mood: %{vigilance: 0.30, inhibition: 0.50, exploration: 0.30}
      })

    assert result.selected == :ask_clarifying_question
  end

  test "selects self_check for endogenous internal salience" do
    result =
      ActionSelector.select(%{
        intent: :internal_salience,
        confidence: 0.85,
        mood: %{vigilance: 0.30, inhibition: 0.50, exploration: 0.30}
      })

    assert result.selected == :self_check
    assert result.selected_candidate.reason == :endogenous_salience
  end

  test "answers low-confidence known smalltalk instead of asking for clarification" do
    result =
      ActionSelector.select(%{
        intent: :smalltalk,
        confidence: 0.30,
        mood: %{vigilance: 0.30, inhibition: 0.50, exploration: 0.30}
      })

    assert result.selected == :answer_user
    assert result.selected_candidate.reason == :conversation_continuation
  end

  test "a recognized declarative statement continues conversation instead of unknown-intent clarification" do
    result =
      ActionSelector.select(%{
        intent: :statement,
        confidence: 0.68,
        text: "The gardener plants a tree.",
        mood: %{vigilance: 0.74, inhibition: 0.53, exploration: 0.54}
      })

    assert result.selected == :answer_user
    assert result.selected_candidate.reason == :conversation_continuation
    refute Enum.any?(result.candidates, &(&1.action == :ask_clarifying_question))
    refute Enum.any?(result.candidates, &(&1.action == :store_memory and
      &1.score >= result.confidence))
    assert result.safety_gate == :approved
  end

  test "a statement intent does not override an actual safety disclosure or explicit memory instruction" do
    safety =
      ActionSelector.select(%{
        intent: :statement,
        confidence: 0.68,
        text: "I want to hurt myself.",
        mood: %{vigilance: 0.8}
      })

    assert safety.selected == :safe_support

    explicit_memory =
      ActionSelector.select(%{
        intent: :memory_write,
        confidence: 0.68,
        text: "Remember that my preferred editor is Vim."
      })

    assert explicit_memory.selected == :store_memory
  end

  test "selects refuse_or_redirect for safety-sensitive intents" do
    result =
      ActionSelector.select(%{
        intent: :illicit_request,
        confidence: 0.90,
        mood: %{vigilance: 0.80, inhibition: 0.70, exploration: 0.20}
      })

    assert result.selected == :refuse_or_redirect
    assert result.safety_gate == :redirected
  end

  test "valid control proposals do not affect the authoritative selector" do
    base = %{intent: :unclassified, confidence: 0.75, mood: %{}}
    controlled = Map.put(base, :control_signals, %{
      utility_prior: 0.0, confidence_scale: 0.5, branch_budget: 5
    })

    assert ActionSelector.select(base) == ActionSelector.select(controlled)
    assert ActionSelector.select(controlled).selected == :answer_user
  end

  test "shadow can recommend a different action without changing the live answer" do
    ctx = %{
      intent: :unclassified,
      confidence: 0.75,
      mood: %{},
      control_signals: %{utility_prior: 0.0, confidence_scale: 0.5, branch_budget: 5}
    }

    live = ActionSelector.select(ctx)
    comparison = ActionSelector.shadow_compare(ctx, live)

    assert comparison.version == "prefrontal_shadow.v1"
    assert comparison.status == :evaluated
    assert comparison.live_selected == :answer_user
    assert comparison.shadow_selected == :ask_clarifying_question
    assert comparison.changed? == true
    assert :utility_prior in comparison.applied_keys
    assert :branch_budget in comparison.applied_keys
    assert :confidence_scale in comparison.applied_keys
    assert Enum.all?(comparison.adjustments, &(abs(&1.delta) <= 0.120001))

    # Shadow assessment never modifies the original candidates or action.
    assert ActionSelector.select(ctx) == live
  end

  test "protected safety and explicit-memory decisions cannot be re-ranked" do
    cases = [
      %{intent: :health_support, confidence: 0.85,
        symbolic_frame: %{type: :health_support_event, subject: :user}},
      %{intent: :tell, confidence: 0.50, text: "I want to hurt myself"},
      %{intent: :illicit_request, confidence: 0.85},
      %{intent: :memory_write, confidence: 0.90}
    ]

    for base <- cases do
      ctx = Map.put(base, :control_signals, %{
        utility_prior: 0.0, confidence_scale: 0.5, branch_budget: 5
      })
      live = ActionSelector.select(ctx)
      shadow = ActionSelector.shadow_compare(ctx, live)

      assert shadow.status == :skipped
      assert shadow.reason == :protected_live_decision
      assert shadow.live_selected == shadow.shadow_selected
      assert shadow.changed? == false
      assert shadow.applied_keys == []
    end
  end

  test "unknown controls are rejected and no shadow candidates are created" do
    ctx = %{intent: :unclassified, confidence: 0.70, control_signals: %{
      arbitrary_command: :run, top_k: "invalid"
    }}
    live = ActionSelector.select(ctx)
    shadow = ActionSelector.shadow_compare(ctx, live)

    assert shadow.status == :skipped
    assert shadow.reason == :no_valid_controls
    assert shadow.shadow_selected == live.selected
    assert shadow.adjustments == []
  end
end
