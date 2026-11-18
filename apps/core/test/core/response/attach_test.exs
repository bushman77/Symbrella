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
      safety_gate: :approved,
      prefrontal_shadow: %{
        status: :evaluated,
        shadow_selected: :observe_silently,
        applied_keys: [:utility_prior]
      }
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

    lineage = si.decision_lineage
    assert lineage.v == 1
    assert lineage.initial_action == :ask_clarifying_question
    assert lineage.final_action == :answer_user
    assert lineage.reconciliation == :reconciled
    assert lineage.reason == :social_intent_reconciliation
    assert lineage.initial_safety_gate == :approved
    assert lineage.final_safety_gate == :approved
    assert lineage.shadow_action == :observe_silently
    assert lineage.shadow_status == :evaluated
    assert lineage.shadow_applied_keys == [:utility_prior]
    assert lineage.delivery_status == :not_observed
    assert lineage.initial_command_statuses == []
    assert lineage.final_command_statuses == []
  end


  test "carries the post-LIFG speech-act frame through response planning" do
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
      %{
        sentence: "Hello Symbrella.",
        intent: :greet,
        confidence: 0.76,
        semantic_choices: [winner],
        lifg_choices: [winner],
        selected_action: :answer_user,
        trace: []
      }
      |> Core.Semantic.SpeechActs.attach()
      |> then(&Attach.maybe_build_response_plan(&1, response: :auto))

    assert si.response_text == "Hello."
    assert si.response_meta.response_source == :symbolic_generation
    assert si.response_meta.response_reflection.lexical_witness.id == winner.id
    assert si.response_meta.response_reflection.addressee == :self
    assert si.response_meta.response_reflection.vocative.resolution == :assistant_identity
    assert si.response_meta.response_reflection.semantic_verified? == false
    assert si.decision_lineage.initial_action == :answer_user
    assert si.decision_lineage.final_action == :answer_user
    assert si.decision_lineage.reconciliation == :unchanged
    assert si.decision_lineage.reason == :action_preserved
    assert si.decision_lineage.delivery_status == :not_observed
  end

  test "a protected live action survives enabled social response planning" do
    winner = %{
      token_index: 0, id: "hello|interjection|1", lemma: "hello",
      pos: "interjection", score: 0.16, source: :active_cells
    }

    action_meta = %{
      selected: :safe_support,
      selected_candidate: %{reason: :support_requested},
      safety_gate: :approved
    }
    original_decision = %Core.Agency.Decision{
      trace_id: "agency|protected|1",
      selected_action: :safe_support
    }

    si = Attach.maybe_build_response_plan(%{
      sentence: "Hello Symbrella.", intent: :greet, confidence: 0.80,
      semantic_choices: [winner], lifg_choices: [winner],
      selected_action: :safe_support,
      action_meta: action_meta, agency_decision: original_decision,
      agency_commands: [], agency_command_results: [], trace: []
    }, response: :auto)

    assert is_map(si.response_meta)
    assert si.selected_action == :safe_support
    assert si.action_meta == action_meta
    assert si.agency_decision == original_decision
    assert si.decision_lineage.initial_action == :safe_support
    assert si.decision_lineage.final_action == :safe_support
    assert si.decision_lineage.initial_decision_trace_id == "agency|protected|1"
    assert si.decision_lineage.final_decision_trace_id == "agency|protected|1"
    assert si.decision_lineage.reconciliation == :unchanged
    assert si.decision_lineage.delivery_status == :not_observed
  end

  test "a missing initial action is not misreported as reconciled" do
    winner = %{
      token_index: 0, id: "hello|interjection|1", lemma: "hello",
      pos: "interjection", score: 0.16, source: :active_cells
    }

    si = Attach.maybe_build_response_plan(%{
      sentence: "Hello Symbrella.", intent: :greet, confidence: 0.80,
      semantic_choices: [winner], lifg_choices: [winner],
      selected_action: nil, trace: []
    }, response: :auto)

    assert si.selected_action == :answer_user
    assert si.decision_lineage.initial_action == nil
    assert si.decision_lineage.final_action == :answer_user
    assert si.decision_lineage.reconciliation == :not_recorded
    assert si.decision_lineage.reason == :initial_action_unavailable
  end

  test "disabled response planning preserves a protected action and existing receipts" do
    initial_meta = %{
      selected: :safe_support,
      selected_candidate: %{reason: :safety_state_check},
      safety_gate: :approved,
      prefrontal_shadow: %{status: :skipped, shadow_selected: :safe_support,
        applied_keys: []}
    }
    receipt = %{status: :deferred, reason: :permission_required}
    original = %{
      intent: :health_support, selected_action: :safe_support,
      action_meta: initial_meta, agency_command_results: [receipt],
      response_meta: nil, trace: []
    }

    final = Attach.maybe_build_response_plan(original, response: :off)

    assert final.selected_action == original.selected_action
    assert final.action_meta == original.action_meta
    assert final.agency_command_results == original.agency_command_results
    assert final.response_meta == nil
    assert final.decision_lineage.initial_action == :safe_support
    assert final.decision_lineage.final_action == :safe_support
    assert final.decision_lineage.reconciliation == :not_planned
    assert final.decision_lineage.reason == :response_disabled
    assert final.decision_lineage.initial_command_statuses == [:deferred]
    assert final.decision_lineage.final_command_statuses == [:deferred]
    assert final.decision_lineage.delivery_status == :not_observed
  end

end
