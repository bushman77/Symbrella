defmodule Core.Response.PlanModelUnavailableTest do
  use ExUnit.Case, async: true

  alias Core.Response

  test "plan/2 does not synthesize template text when the model is unavailable" do
    si = %{
      intent: :question,
      confidence: 0.2,
      text: "what should we do with this unclear response pipeline state?",
      comprehension: %{
        degraded?: true,
        understood: ["response pipeline"],
        uncertain: ["target module"]
      }
    }

    {_tone, text, meta} = Response.plan(si, %{})

    assert meta.confidence_bucket == :low
    assert meta.response_source == :model_unavailable
    assert meta.response_fallback_reason in [:llm_not_available, :lazy_start_disabled]
    assert text =~ "No fallback response was generated."
    refute text =~ "Suggested next step:"
  end

  test "greeting does not use canned social text when LLM synthesis is unavailable" do
    si = %{intent: :unknown, confidence: 0.8, text: "good morning symbrella"}

    {_tone, text, meta} = Response.plan(si, %{})

    assert meta.intent_inferred == :greeting
    assert meta.response_source == :model_unavailable
    assert text =~ "No fallback response was generated."
    refute text =~ "Good morning"
    refute text =~ "code"
    refute text =~ "error"
    refute text =~ "Suggested next step:"
  end

  test "personal finance text does not fall back to canned financial advice" do
    si = %{
      intent: :help,
      confidence: 0.8,
      text: "so far i checked it on credit karma, and setup an appoinment with a debt consolidating place"
    }

    {_tone, text, meta} = Response.plan(si, %{})

    assert meta.intent_inferred == :help
    assert meta.response_source == :model_unavailable
    assert text =~ "No fallback response was generated."
    refute text =~ "Before committing to consolidation"
    refute text =~ "debts, balances, interest rates"
    refute text =~ "engineering"
    refute text =~ "module"
    refute text =~ "file"
    refute text =~ "code"
  end
end
