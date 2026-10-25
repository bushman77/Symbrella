defmodule Core.Response.LlmPromptEventsTest do
  use ExUnit.Case, async: true

  alias Core.Response.LlmPromptEvents

  test "prompt builds telemetry measurements and metadata" do
    system = """
    Response profile: warm_collaborator.
    Simulated affect: steady.
    Personality state: focused.
    """

    context = %{
      session_id: "s1",
      features: %{intent: :chat},
      decision: %{mode: :companion, tone: :warm}
    }

    {measurements, metadata} = LlmPromptEvents.prompt(system, "hello", context)

    assert measurements.system_chars == String.length(system)
    assert measurements.user_chars == 5
    assert metadata.session_id == "s1"
    assert metadata.intent == :chat
    assert metadata.mode == :companion
    assert metadata.tone == :warm
    assert metadata.response_profile == "warm_collaborator"
    assert metadata.simulated_affect == "steady"
    assert metadata.personality_state == "focused"
    assert metadata.system_sha256 == LlmPromptEvents.sha256_hex(system)
  end

  test "complete prefers prompt response profile over context profile" do
    system = "Response profile: direct.\n"
    context = %{decision: %{response_profile: :fallback_profile}}
    reflection = %{status: :accepted, confidence: 0.9, ignored: true}

    {_measurements, metadata} =
      LlmPromptEvents.complete("user", "assistant", context, system, reflection)

    assert metadata.response_profile == :direct
    assert metadata.prompt_response_profile == "direct"
    assert metadata.reflection == %{status: :accepted, confidence: 0.9}
  end

  test "completion telemetry retains bounded polling provenance without calling it verified" do
    polling = %{
      kind: :public_polling,
      evidence_level: :source_reported,
      verified?: false,
      limitations: [:causation_not_established],
      observations: [%{
        subject: "Donald Trump", metric: :approval, value_pct: 31.0,
        origin: :headline_reported, population: nil, fieldwork_start: nil,
        provenance: %{publisher: "Example Wire",
          published_at: "2026-10-24T12:00:00Z", url: "https://news.google.com/rss/articles/poll"}
      }],
      comparisons: []
    }

    context = %{web_evidence: %{
      status: :retrieved,
      resolution: :headlines_available,
      routing_trigger: :public_polling_question,
      request_type: :current_events,
      sources: [],
      polling: polling
    }}

    {_measurements, meta} =
      LlmPromptEvents.complete("question", "answer", context, "system", nil)

    assert meta.web_retrieval.routing_trigger == :public_polling_question
    assert meta.web_retrieval.polling.verified? == false
    assert [%{value_pct: 31.0, fieldwork_start: nil, provenance: provenance}] =
             meta.web_retrieval.polling.observations
    assert provenance.publisher == "Example Wire"
    assert meta.web_retrieval.polling.comparisons == []
  end

  test "extract_system_user returns first system and last user message" do
    messages = [
      %{"role" => "user", "content" => "old"},
      %{"role" => "system", "content" => "rules"},
      %{"role" => "assistant", "content" => "reply"},
      %{"role" => "user", "content" => "new"}
    ]

    assert LlmPromptEvents.extract_system_user(messages) == {"rules", "new"}
  end
end
