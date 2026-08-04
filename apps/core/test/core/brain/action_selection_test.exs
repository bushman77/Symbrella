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
end
