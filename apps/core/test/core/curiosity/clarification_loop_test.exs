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
    assert state.session.last_decision == :clarification_proposed
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

  test "generic model follow-up becomes a single grounded clarification" do
    server = server()
    session = sid()
    input = si(session)
    admit(input)
    generic = "Congratulations! What have you been learning about in your module?"

    assert ClarificationLoop.replaceable_generic_reply?(generic)
    assert {:replace_generic, question, flags} =
      ClarificationLoop.observe(session, input, generic, @meta, server)
    assert question =~ "glimmerflux"
    assert String.ends_with?(question, "?")
    assert flags.delivery == :replace_generic
    assert ClarificationLoop.display_text(generic, {:replace_generic, question, flags}) ==
             question
    refute String.contains?(question, "Congratulations")
    assert ClarificationLoop.snapshot(session, server).session.pending.term == "glimmerflux"

    # A specific question is not a vacuous module follow-up.
    assert {:skip, :reply_already_asks_question} =
      ClarificationLoop.diagnose(input,
        {:ok, %{status: :review_required,
          next_action: :consider_user_clarification, age_ms: 1}},
        "Do you use glimmerflux for cryptographic signatures?", @meta)
  end

  test "generic follow-up variants are replaceable, but concrete questions and real answers survive" do
    for generic <- [
      "Congratulations! What have you been learning about in your module?",
      "That sounds interesting! What are you working on?",
      "That's great. Can you tell me more about your module?",
      "Awesome! What does your module do?",
      "Nice! What have you learned so far?",
      "That sounds cool! What is your project about?",
      "What are your goals with it?",
      "How is your module coming along?"
    ] do
      assert ClarificationLoop.replaceable_generic_reply?(generic), generic
    end

    for substantive <- [
      "What does glimmerflux mean?",
      "What Elixir libraries are you using for the module?",
      "Do you use glimmerflux for cryptographic signatures?",
      "I found your module documentation useful. What else does it support?",
      "Congratulations! Your module parses JSON. What does your module do?",
      "What are you doing? Are you testing it?",
      "What have you been learning about in your module? What about your project?"
    ] do
      refute ClarificationLoop.replaceable_generic_reply?(substantive), substantive
    end
  end

  test "smalltalk intent on a real user statement may clarify a generic LLM question" do
    server = server()
    session = sid()
    input = %{si(session) | source: :prod, intent: :smalltalk}
      |> Map.put(:experience_origin, :user_turn)
    admit(input)

    question = "That sounds interesting! What are you working on?"
    assert {:replace_generic, replacement, _meta} =
      ClarificationLoop.observe(session, input, question, @meta, server)
    assert replacement =~ "glimmerflux"
    assert ClarificationLoop.snapshot(session, server).session.pending.term == "glimmerflux"
  end

  test "specific model question about the unknown term is tracked, not replaced" do
    server = server()
    session = sid()
    input = si(session)
    admit(input)
    question = ~s(What does "glimmerflux" mean?)

    assert ClarificationLoop.already_clarifies?(question, "glimmerflux")
    assert ClarificationLoop.already_clarifies?(
      "Could you explain what you mean by glimmerflux?", "glimmerflux")
    refute ClarificationLoop.already_clarifies?(
      "How does glimmerflux work?", "glimmerflux")
    refute ClarificationLoop.already_clarifies?(
      "What is the purpose of glimmerflux?", "glimmerflux")

    assert {:tracked_existing, ^question, flags} =
      ClarificationLoop.observe(session, input, question, @meta, server)

    assert flags.delivery == :tracked_existing
    assert ClarificationLoop.display_text(question,
      {:tracked_existing, question, flags}) == question

    state = ClarificationLoop.snapshot(session, server)
    assert state.session.last_decision == :existing_clarification_tracked
    assert state.session.pending.term == "glimmerflux"
    assert state.attempts == 1

    assert :none == ClarificationLoop.observe(session,
      %{sentence: "By glimmerflux I mean a fictional module",
        lexical_constructions: []}, "Thanks.", @meta, server)
    assert ClarificationLoop.snapshot(session, server).session.last_outcome ==
      :clarification_received_unverified
  end

  test "live glimmerflux user report is tracked despite noisy :code intent" do
    server = server()
    session = sid()
    input =
      %{si(session) | sentence: "I've been working on a glimmerflux module.",
        source: :prod, intent: :code}
      |> Map.put(:experience_origin, :user_turn)
    admit(input)

    reply =
      "That sounds like an interesting project you're working on! Could you tell me more about what a glimmerflux module is and how it works?"

    assert ClarificationLoop.already_clarifies?(reply, "glimmerflux")
    assert {:tracked_existing, ^reply, flags} =
      ClarificationLoop.observe(session, input, reply, @meta, server)
    assert flags.delivery == :tracked_existing
    snap = ClarificationLoop.snapshot(session, server)
    assert snap.attempts == 1
    assert snap.session.pending.term == "glimmerflux"
    assert snap.session.last_decision == :existing_clarification_tracked
  end

  test "declarative override rejects actual requests, commands and safety-sensitive reports" do
    input =
      %{si(sid()) | sentence: "I've been working on a glimmerflux module.",
        source: :prod, intent: :ask}
      |> Map.put(:experience_origin, :user_turn)
    gap = {:ok, %{status: :review_required,
      next_action: :consider_user_clarification, age_ms: 1}}

    assert {:eligible, "glimmerflux"} =
      ClarificationLoop.diagnose(input, gap, "Interesting.", @meta)
    assert {:skip, :intent_not_statement} =
      ClarificationLoop.diagnose(%{input | sentence: "Build a glimmerflux module."},
        gap, "Interesting.", @meta)
    assert {:skip, :intent_not_statement} =
      ClarificationLoop.diagnose(%{input | intent: :command},
        gap, "Interesting.", @meta)
    assert {:skip, :factual_or_question_turn} =
      ClarificationLoop.diagnose(%{input | sentence: "I've been working on what?"},
        gap, "Interesting.", @meta)
    assert {:skip, :safety_sensitive} =
      ClarificationLoop.diagnose(%{input | sentence:
        "I've been working on a dangerous glimmerflux module."},
        gap, "Interesting.", @meta)
  end

  test "real pipeline user source is :prod with :user_turn provenance" do
    input = %{si(sid()) | source: :prod}
    |> Map.put(:experience_origin, :user_turn)

    gap = {:ok, %{status: :review_required,
      next_action: :consider_user_clarification, age_ms: 10}}

    assert {:eligible, "glimmerflux"} =
      ClarificationLoop.diagnose(input, gap, "Interesting design.", @meta)

    assert {:ok, "glimmerflux"} =
      ClarificationLoop.proposal(input, gap, "Interesting design.", @meta)

    assert {:skip, :not_user_originated} =
      ClarificationLoop.diagnose(%{input | experience_origin: :synthetic},
        gap, "Interesting design.", @meta)

    assert {:skip, :not_user_originated} =
      ClarificationLoop.diagnose(%{input | source: :test},
        gap, "Interesting design.", @meta)
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
