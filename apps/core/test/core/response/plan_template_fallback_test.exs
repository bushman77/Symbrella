defmodule Core.Response.PlanTemplateFallbackTest do
  use ExUnit.Case, async: true

  alias Core.Response

  test "plan/2 passes contextual opts into deterministic template fallback" do
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
    assert text =~ "Suggested next step:"
    assert text =~ "State what is understood, then ask one targeted question only if necessary."
  end

  test "greeting fallback stays conversational when LLM synthesis is unavailable" do
    si = %{intent: :unknown, confidence: 0.8, text: "good morning symbrella"}

    {_tone, text, meta} = Response.plan(si, %{})

    assert meta.intent_inferred == :greeting
    assert text =~ "Good morning"
    refute text =~ "code"
    refute text =~ "error"
    refute text =~ "Suggested next step:"
  end

  test "personal finance fallback does not use engineering language" do
    si = %{
      intent: :help,
      confidence: 0.8,
      text: "so far i checked it on credit karma, and setup an appoinment with a debt consolidating place"
    }

    {_tone, text, meta} = Response.plan(si, %{})

    assert meta.intent_inferred == :help
    assert text =~ "Before committing to consolidation"
    assert text =~ "Suggested next step:"
    assert text =~ "debts, balances, interest rates"
    refute text =~ "engineering"
    refute text =~ "module"
    refute text =~ "file"
    refute text =~ "code"
  end
end
