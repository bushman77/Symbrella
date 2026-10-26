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

  test "carries the canonical lexical greeting winner from SI into symbolic realization" do
    previous = Application.get_env(:core, :llm_inference_enabled?, :unset)
    Application.put_env(:core, :llm_inference_enabled?, false)

    on_exit(fn ->
      if previous == :unset do
        Application.delete_env(:core, :llm_inference_enabled?)
      else
        Application.put_env(:core, :llm_inference_enabled?, previous)
      end
    end)

    winner = %{
      token_index: 0, id: "hello|interjection|1", lemma: "hello",
      pos: "interjection", score: 0.16190361976623535,
      source: :active_cells
    }

    si =
      Attach.maybe_build_response_plan(
        %{
          sentence: "Hello Symbrella.",
          intent: :greet,
          confidence: 0.76,
          semantic_choices: [winner],
          lifg_choices: [winner],
          selected_action: :answer_user,
          trace: []
        },
        response: :auto
      )

    assert si.response_text == "Hello."
    assert si.response_meta.response_source == :symbolic_generation
    assert si.response_meta.response_reflection.lexical_witness.id == winner.id
    assert si.response_meta.response_reflection.provenance == :canonical_lifg_choice
    assert si.response_meta.response_reflection.semantic_verified? == false
  end

end
