defmodule Core.Response.AgencyReflectionTest do
  use ExUnit.Case, async: true

  alias Core.Response.AgencyReflection

  test "from_response/4 records model unavailability as a scope-reduction adjustment" do
    reflection =
      AgencyReflection.from_response(
        "what should we do?",
        "Model unavailable (llm_not_available). No fallback response was generated.",
        %{},
        %{
          action: :offer_options,
          mode: :coach,
          response_source: :model_unavailable,
          response_fallback_reason: :llm_not_available,
          confidence: 0.4,
          self_state_effects: []
        }
      )

    assert reflection.v == 1
    assert :model_unavailable in reflection.what_failed
    assert reflection.next_time_adjustment == :reduce_scope
    assert reflection.confidence_delta < 0.0
    assert :model_unavailable in reflection.signals
    assert :reduce_scope in reflection.signals
  end

  test "from_response/4 records uncertainty as clarification pressure" do
    reflection =
      AgencyReflection.from_response(
        "help",
        "What detail matters most?",
        %{},
        %{
          action: :offer_options,
          mode: :coach,
          response_source: :inline_skill,
          confidence: 0.2,
          self_state_effects: [:ask_clarifying_question, :hedge_under_uncertainty]
        }
      )

    assert :needed_clarification in reflection.what_failed
    assert reflection.next_time_adjustment == :ask_clearer_question
    assert :clarify in reflection.signals
    assert :uncertainty in reflection.signals
  end
end
