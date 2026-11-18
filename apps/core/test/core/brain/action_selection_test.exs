defmodule Core.Brain.ActionSelectionTest do
  use ExUnit.Case, async: true

  alias Core.Brain.ActionSelection

  test "attaches selected action, candidates, meta, and trace" do
    si = %{
      sentence: "I forgot my quetiapine and now I can not sleep.",
      source: :test,
      tokens: [],
      trace: [],
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
    }

    out = ActionSelection.attach(si, [])

    assert out.selected_action == :safe_support
    assert is_list(out.action_candidates)
    assert %{selected: :safe_support} = out.action_meta

    assert %Core.Agency.Decision{selected_action: :safe_support, trace_id: trace_id} =
             out.agency_decision

    assert is_binary(trace_id)
    assert out.agency_commands == []
    assert out.agency_command_results == []

    assert Enum.any?(out.trace, fn
             %{
               stage: :action_selection,
               decision: :safe_support,
               meta: %{trace_id: ^trace_id, commands: []}
             } ->
               true

             _ ->
               false
           end)
  end

  test "proposes a permission-gated memory command for store_memory actions" do
    si = %{
      sentence: "Remember that my preferred editor is Vim.",
      source: :test,
      tokens: [],
      trace: [],
      intent: :memory_write,
      confidence: 0.88,
      mood: %{vigilance: 0.20, inhibition: 0.60, exploration: 0.40}
    }

    out = ActionSelection.attach(si, [])

    assert out.selected_action == :store_memory
    assert %Core.Agency.Decision{selected_action: :store_memory} = out.agency_decision

    assert [%Core.Agency.Command{type: :write_memory, requires_permission?: true}] =
             out.agency_commands

    assert [
             %{
               type: :write_memory,
               status: :deferred,
               reason: :permission_required
             }
           ] = out.agency_command_results
  end

  test "Core action selection routes recognized SVO statements as conversation" do
    si = %{
      sentence: "The gardener plants a tree.",
      source: :test,
      tokens: [],
      trace: [],
      intent: :statement,
      confidence: 0.68,
      conversation_act: :statement,
      mood: %{vigilance: 0.74, inhibition: 0.53, exploration: 0.54}
    }

    out = ActionSelection.attach(si, [])
    assert out.intent == :statement
    assert out.selected_action == :answer_user
    assert out.action_meta.selected_candidate.reason == :conversation_continuation
    assert out.agency_commands == []
    assert Enum.any?(out.trace, fn
      %{stage: :action_selection, decision: :answer_user} -> true
      _ -> false
    end)
  end

  test "keeps low-confidence smalltalk on the answer path" do
    si = %{
      sentence: "How are you doing on this fine saturday afternoon Symbrella?",
      source: :test,
      tokens: [],
      trace: [],
      intent: :smalltalk,
      confidence: 0.30,
      mood: %{vigilance: 0.30, inhibition: 0.50, exploration: 0.30}
    }

    out = ActionSelection.attach(si, [])

    assert out.selected_action == :answer_user
    assert %{selected: :answer_user} = out.action_meta
    assert %Core.Agency.Decision{selected_action: :answer_user} = out.agency_decision
  end

  test "shadow comparison is recorded while authoritative action and commands stay unchanged" do
    si = %{
      sentence: "An unclassified observation",
      source: :test,
      tokens: [],
      trace: [],
      intent: :unclassified,
      confidence: 0.75,
      mood: %{},
      control_signals: %{
        utility_prior: 0.0,
        confidence_scale: 0.5,
        branch_budget: 5
      },
      prefrontal: %{
        provenance: %{
          utility_prior: Brain.VmPFC,
          confidence_scale: Brain.DmPFC,
          branch_budget: Brain.FPC
        }
      }
    }

    baseline = ActionSelection.attach(si, prefrontal_shadow?: false)
    observed = ActionSelection.attach(si, prefrontal_shadow?: true)

    assert baseline.selected_action == :answer_user
    assert observed.selected_action == baseline.selected_action
    assert observed.action_candidates == baseline.action_candidates
    assert observed.agency_decision.selected_action == baseline.agency_decision.selected_action
    # Decisions and command IDs are generated independently on each call.
    # Compare stable action/permission semantics rather than unique identifiers.
    stable_command = fn command ->
      Map.take(command, [:type, :reason, :risk, :requires_permission?, :payload])
    end

    assert Enum.map(observed.agency_commands, stable_command) ==
             Enum.map(baseline.agency_commands, stable_command)

    stable_result = fn result -> Map.take(result, [:type, :status, :reason]) end

    assert Enum.map(observed.agency_command_results, stable_result) ==
             Enum.map(baseline.agency_command_results, stable_result)

    shadow = observed.action_meta.prefrontal_shadow
    assert shadow.status == :evaluated
    assert shadow.live_selected == :answer_user
    assert shadow.shadow_selected == :ask_clarifying_question
    assert shadow.changed? == true
    assert Enum.any?(shadow.applied_sources, &(
      &1.key == :branch_budget and &1.planner == Brain.FPC
    ))

    assert Enum.any?(observed.trace, fn
      %{stage: :action_selection, meta: %{prefrontal_shadow: %{changed?: true}}} -> true
      _ -> false
    end)
  end

  test "explicitly disabled shadow reports its status without altering memory permission" do
    si = %{
      sentence: "Remember my preference.",
      source: :test,
      tokens: [],
      trace: [],
      intent: :memory_write,
      confidence: 0.88,
      mood: %{},
      control_signals: %{utility_prior: 0.0}
    }

    disabled = ActionSelection.attach(si, prefrontal_shadow?: false)
    enabled = ActionSelection.attach(si, prefrontal_shadow?: true)

    assert disabled.action_meta.prefrontal_shadow.status == :disabled
    assert enabled.action_meta.prefrontal_shadow.reason == :protected_live_decision
    assert disabled.selected_action == enabled.selected_action
    assert [%Core.Agency.Command{requires_permission?: true}] = enabled.agency_commands
    assert [%{status: :deferred, reason: :permission_required}] = enabled.agency_command_results
  end
end
