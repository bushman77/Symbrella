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

    assert Enum.any?(out.trace, fn
             %{stage: :action_selection, decision: :safe_support} -> true
             _ -> false
           end)
  end
end
