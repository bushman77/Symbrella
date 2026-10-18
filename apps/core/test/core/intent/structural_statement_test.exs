defmodule Core.Intent.StructuralStatementTest do
  use ExUnit.Case, async: false

  alias Core.Intent.StructuralStatement

  @opts [
    mode: :prod,
    experience_origin: :user_turn,
    fuzzy: false,
    response: :off,
    episodes: false,
    persist_episodes: false,
    hippocampus_encode?: false,
    self_model: :off,
    comprehension_wm?: false,
    notify_brain?: false,
    synapse_shadow?: false,
    semantic_sense_shadow?: false,
    lifg_opts: [grammar_weight: 1.0]
  ]

  test "live structurally interpreted declarative becomes statement, not a memory command" do
    si = Core.resolve_input("The baker cooks a meal.", @opts)
    assert si.intent == :statement
    assert si.conversation_act == :statement
    assert si.confidence >= 0.6
    assert %{kind: :participant_assertion, speech_act: :assertion,
             source_frame: :finite_present_svo} = si.sentence_meaning

    event = Enum.find(si.trace,
      &match?(%{stage: :structural_statement}, &1))
    assert event.decision == :statement
    assert event.reason == :revalidated_participant_assertion
    assert event.meta.writes_memory? == false
    assert event.meta.selects_verb_sense? == false
    assert is_list(si.lifg_choices)
  end

  test "original non-unknown intent is not overwritten by assertion reconciliation" do
    si = Core.resolve_input("The baker cooks a meal.", @opts)
    assert si.intent == :statement
    command = %{si | intent: :command, conversation_act: :instruction}
    assert StructuralStatement.attach(command) == command
  end

  test "unvalidated, non-user and interrogative inputs cannot become statements" do
    si = Core.resolve_input("The baker cooks a meal.", @opts)
    assert si.intent == :statement

    for changed <- [
          %{si | intent: :unknown, sentence_meaning: nil},
          %{si | intent: :unknown, source: :assistant},
          %{si | intent: :unknown, experience_origin: :assistant_turn},
          %{si | intent: :unknown, atl_slate: %{winners: []}},
          %{si | intent: :unknown,
            sentence_meaning: %{kind: :participant_assertion,
              speech_act: :assertion, status: :structurally_interpreted,
              source_frame: :finite_present_svo}}
        ] do
      assert StructuralStatement.attach(changed) == changed
    end
  end

  test "questions retain their question-shaped intent and are not promoted" do
    si = Core.resolve_input("What did the artist draw?", @opts)
    assert StructuralStatement.attach(si) == si
  end
end
