defmodule Core.Curiosity.ClarificationLoopTest do
  use ExUnit.Case, async: false

  alias Core.Brain.WM.Comprehension
  alias Core.Curiosity.ClarificationLoop

  @meta %{response_source: :llm, agency_decision: %{risk: :low,
    selected_action: :answer_user}, response_reflection: %{status: :accept}}

  defp sid, do: "clarification-#{System.unique_integer([:positive])}"

  defp si(sid, term \\ "glimmerflux") do
    %{
      session_id: sid, source: :user, intent: :statement,
      sentence: "This #{term} circuit is unpredictable.",
      sentence_meaning: %{status: :not_yet_interpreted},
      comprehension: %{
        degraded?: true, uncertain: [term], understood: [],
        unresolved: [term], recognized_topics: []
      },
      trace: []
    }
  end

  defp server do
    name = :"clarification_loop_test_#{System.unique_integer([:positive])}"
    start_supervised!({ClarificationLoop, name: name, enabled?: true, auto_ask?: true})
    name
  end

  defp admit(si) do
    result = Comprehension.store(si, comprehension_wm?: true)
    assert Enum.any?(result.trace, &(&1.stage == :wm_comprehension and &1.decision == :admitted))
    on_exit(fn -> Brain.defocus(Comprehension.frame_id(si.session_id)) end)
  end

  test "a real WM comprehension gap offers a separate, non-authoritative prompt" do
    server = server()
    session = sid()
    input = si(session)
    admit(input)

    assert {:ask, prompt, flags} =
      ClarificationLoop.observe(session, input, "Interesting design.", @meta, server)
    assert prompt =~ "glimmerflux"
    assert flags.kind == :clarification_prompt
    assert flags.factual_authority? == false
    assert flags.learning_credit? == false

    state = ClarificationLoop.snapshot(session, server)
    assert state.attempts == 1
    assert state.session.pending.term == "glimmerflux"
    assert state.learning.bias == 0.0
    assert state.learning.success_count == 0
    assert state.learning.cooldown_ms == 600_000
    assert ClarificationLoop.snapshot("other-session", server).session.pending == nil

    # The very next turn is the sole opportunity: no duplicate prompt.
    second = si(session)
    assert :none == ClarificationLoop.observe(session, second, "Got it.", @meta, server)
    assert ClarificationLoop.snapshot(session, server).session.pending == nil
  end

  test "user explanations are recorded but cannot independently verify a meaning" do
    server = server()
    session = sid()
    input = si(session)
    admit(input)

    assert {:ask, _, _} =
      ClarificationLoop.observe(session, input, "Tell me more.", @meta, server)

    reply = %{sentence: "By glimmerflux I mean a made up device",
      lexical_constructions: [], intent: :statement}
    assert :none == ClarificationLoop.observe(session, reply, "Thanks.", @meta, server)

    snap = ClarificationLoop.snapshot(session, server)
    assert snap.session.last_outcome == :clarification_received_unverified
    assert snap.learning.unverified_count == 1
    assert snap.learning.success_count == 0
    assert snap.learning.bias == 0.0
  end

  test "only a new matching dictionary-verified construction earns bounded learning credit" do
    server = server()
    session = sid()
    input = si(session, "power")
    admit(input)

    assert {:ask, _, _} =
      ClarificationLoop.observe(session, input, "Interesting idea.", @meta, server)

    # An unrelated verified sense cannot complete a pending power question.
    unrelated = %{
      sentence: "By power I meant energy",
      lexical_constructions: [%{id: "energy|noun|1", phrase: "energy",
        definition: "Capacity to work.", dictionary_verified?: true,
        context_score: 0.34}]
    }
    assert :clarification_received_unverified ==
      ClarificationLoop.evaluate(%{term: "power", verified_before: []}, unrelated)

    verified = %{
      sentence: "By power I meant electrical power",
      lexical_constructions: [%{id: "power|noun|1", phrase: "power",
        definition: "The rate at which energy is transferred.",
        dictionary_verified?: true, context_score: 0.22}]
    }

    assert :verified_improvement ==
      ClarificationLoop.evaluate(%{term: "power", verified_before: []}, verified)

    # A term with a sense already verified before asking is not "new" progress.
    assert :clarification_received_unverified ==
      ClarificationLoop.evaluate(%{term: "power",
        verified_before: ["power|noun|1"]}, verified)

    assert :none == ClarificationLoop.observe(session, verified, "Noted.", @meta, server)
    result = ClarificationLoop.snapshot(session, server)
    assert result.learning.success_count == 1
    assert result.learning.sample_count == 1
    assert result.learning.bias == 0.03
    assert result.learning.cooldown_ms == 540_000
    assert ClarificationLoop.cooldown_for(25) == 360_000
    assert result.session.last_outcome == :verified_improvement
  end

  test "real-world factual questions, repaired answers, guardrail cases and unknown unrelated terms never prompt" do
    good_gap = {:ok, %{status: :review_required,
      next_action: :consider_user_clarification, age_ms: 1}}
    input = si(sid())

    assert {:ok, "glimmerflux"} ==
      ClarificationLoop.proposal(input, good_gap, "I understand.", @meta)

    refute match?({:ok, _},
      ClarificationLoop.proposal(%{input | intent: :ask},
        good_gap, "I understand.", @meta))

    refute match?({:ok, _},
      ClarificationLoop.proposal(input, good_gap, "Is that correct?", @meta))

    refute match?({:ok, _},
      ClarificationLoop.proposal(input, good_gap, "I understand.",
        put_in(@meta, [:response_reflection, :status], :clarify)))

    refute match?({:ok, _},
      ClarificationLoop.proposal(input, good_gap, "I understand.",
        put_in(@meta, [:agency_decision, :risk], :high)))

    assert :none == ClarificationLoop.proposal(input,
      {:ok, %{status: :review_required, next_action: :inspect_comprehension,
        age_ms: 1}}, "I understand.", @meta)

    assert :none == ClarificationLoop.proposal(
      put_in(input, [:comprehension, :unresolved], []),
      good_gap, "I understand.", @meta)
  end
end
