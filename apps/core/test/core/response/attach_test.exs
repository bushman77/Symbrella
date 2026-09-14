defmodule Core.Response.AttachTest do
  use ExUnit.Case, async: false

  alias Core.Response.Attach

  setup do
    old_ledger = Application.get_env(:core, :agency_ledger_enabled?)
    Application.put_env(:core, :agency_ledger_enabled?, false)

    on_exit(fn ->
      case old_ledger do
        nil -> Application.delete_env(:core, :agency_ledger_enabled?)
        value -> Application.put_env(:core, :agency_ledger_enabled?, value)
      end
    end)

    :ok
  end

  test "reconciles stale low-confidence action metadata after social intent normalization" do
    stale_action_meta = %{
      version: "action_selector.v1",
      selected: :ask_clarifying_question,
      selected_candidate: %{
        action: :ask_clarifying_question,
        score: 0.75,
        reason: :low_confidence,
        speech_required?: true,
        memory_relevant?: false
      },
      candidates: [
        %{
          action: :ask_clarifying_question,
          score: 0.75,
          reason: :low_confidence,
          speech_required?: true,
          memory_relevant?: false
        }
      ],
      confidence: 0.75,
      safety_gate: :approved
    }

    si =
      Attach.maybe_build_response_plan(
        %{
          sentence: "how are you on this fine saturday afternoon Symbrella?",
          text: "how are you on this fine saturday afternoon Symbrella?",
          intent: :unknown,
          confidence: 0.40,
          selected_action: :ask_clarifying_question,
          action_candidates: stale_action_meta.candidates,
          action_meta: stale_action_meta,
          trace: []
        },
        response: :auto
      )

    assert si.response_meta.intent_inferred == :smalltalk
    assert si.selected_action == :answer_user
    assert si.action_meta.selected == :answer_user
    assert si.action_meta.selected_candidate.reason == :conversation_continuation
    assert si.response_meta.agent_selected_action == :answer_user
    assert si.response_meta.agent_action_meta.selected == :answer_user
    assert %Core.Agency.Decision{selected_action: :answer_user} = si.response_meta.agency_decision
  end
end
