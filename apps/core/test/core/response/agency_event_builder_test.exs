defmodule Core.Response.AgencyEventBuilderTest do
  use ExUnit.Case, async: true

  alias Core.Response.AgencyEventBuilder

  test "builds response event attrs without persistence" do
    attrs =
      AgencyEventBuilder.response_attrs(
        "  hello  ",
        String.duplicate("a", 1_700),
        %{self_model: %{name: :symbrella}, agency_memory: %{recent: [:ask]}},
        %{
          session_id: :demo,
          action: :respond,
          intent_inferred: :chat,
          confidence: 0.8,
          mode: :companion,
          agency_commands: [%{type: :observe}]
        }
      )

    assert attrs.session_id == "demo"
    assert attrs.source == "core_response"
    assert attrs.input.text == "hello"
    assert attrs.input.intent == "chat"
    assert attrs.decision.mode == "companion"
    assert attrs.decision.agency_commands == [%{"type" => "observe"}]
    assert attrs.self_model == %{"name" => "symbrella"}
    assert attrs.reasons.agency_memory == %{"recent" => ["ask"]}
    assert String.length(attrs.outcome.assistant_text) == 1_603
    assert is_map(attrs.reflection)
  end

  test "builds command event attrs without persistence" do
    attrs =
      AgencyEventBuilder.command_attrs(
        %{id: "cmd-1", type: :search, reason: :need_context, risk: :low},
        %{status: :deferred, reason: :permission_required},
        session_id: "s1"
      )

    assert attrs.session_id == "s1"
    assert attrs.source == "agency_executor"
    assert attrs.status == "deferred"
    assert attrs.action == "search"

    assert attrs.input.command == %{
             "id" => "cmd-1",
             "reason" => "need_context",
             "risk" => "low",
             "type" => "search"
           }

    assert attrs.reflection.signals == [:defer_action]
    assert attrs.reflection.next_time_adjustment == :request_permission
  end
end
