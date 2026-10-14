defmodule Core.Response.GeographicContinuityTest do
  use ExUnit.Case, async: false

  alias Core.Discourse.References
  alias Core.MWE.Decisions
  alias Core.Response.ActionAlignment
  alias Core.Response.AnswerPolish
  alias Core.Response.LlmChatHistory
  alias Core.Response.LlmPrompt
  alias Core.Response.ReflectionLoop
  alias Core.Response.SelfStateFeatures
  alias Core.Semantic.EventFrames

  setup do
    {:ok, session_id: "geo-#{System.unique_integer([:positive])}"}
  end

  test "binds its and it to the explicitly named place in earlier user turns", %{
    session_id: session_id
  } do
    LlmChatHistory.record_turn(
      session_id,
      "Where is Maple Ridge, BC?",
      "Maple Ridge is in British Columbia."
    )

    population_reference = References.resolve("What is its population?", session_id)
    assert population_reference.antecedent == "Maple Ridge, BC"
    assert population_reference.pronoun == "its"
    assert population_reference.source == :recent_user_turn

    LlmChatHistory.record_turn(
      session_id,
      "What is its population?",
      "I don't have a verified figure."
    )

    distance_reference = References.resolve("Is it close to Vancouver?", session_id)
    assert distance_reference.antecedent == "Maple Ridge, BC"
    assert distance_reference.pronoun == "it"
    assert distance_reference.confidence == 0.9
  end

  test "never guesses an antecedent without an explicit location turn", %{
    session_id: session_id
  } do
    assert References.resolve("Is it close to Vancouver?", session_id) == nil
    LlmChatHistory.record_turn(session_id, "Hello there", "Hello")
    assert References.resolve("What is its population?", session_id) == nil
  end

  test "references are session-scoped", %{session_id: session_id} do
    LlmChatHistory.record_turn(session_id, "Where is Maple Ridge, BC?", "BC")
    assert References.resolve("Is it close to Vancouver?", session_id <> "-other") == nil
  end

  test "spatial and population frames keep referents separate from verified facts", %{
    session_id: session_id
  } do
    LlmChatHistory.record_turn(session_id, "Where is Maple Ridge, BC?", "Canada")

    spatial =
      %{sentence: "Is it close to Vancouver?", session_id: session_id}
      |> References.attach()
      |> EventFrames.attach()

    assert spatial.symbolic_frame.type == :location_question
    assert spatial.symbolic_frame.subject == "Maple Ridge, BC"
    assert spatial.symbolic_frame.target == "Vancouver"
    assert spatial.symbolic_frame.relation == :proximity
    assert spatial.symbolic_frame.grounding_status == :unverified

    population =
      %{sentence: "What is its population?", session_id: session_id}
      |> References.attach()
      |> EventFrames.attach()

    assert population.symbolic_frame.subject == "Maple Ridge, BC"
    assert population.symbolic_frame.relation == :population
    assert population.symbolic_frame.grounding_status == :unverified
  end

  test "does not promote conflicting MWE senses over an interrogative construction" do
    si = %{
      sentence: "Is it close to Vancouver?",
      tokens: [
        %{index: 0, phrase: "Is"},
        %{index: 1, phrase: "it"},
        %{index: 2, phrase: "close"},
        %{index: 3, phrase: "to"},
        %{index: 4, phrase: "Vancouver"}
      ],
      lifg_choices: [
        %{token_index: 0, id: "is|verb|0"},
        %{token_index: 1, id: "it|pronoun|0"},
        %{token_index: 2, id: "close|adjective|0"},
        %{token_index: 3, id: "to|preposition|0"},
        %{token_index: 4, id: "vancouver|proper_noun|1"}
      ],
      mwe_choices: [
        %{
          id: "is it|interjection|1",
          phrase: "is it",
          token_span: {0, 1},
          source: :mwe_lexicon,
          pos: "interjection"
        },
        %{
          id: "close to|adverb|1",
          phrase: "close to",
          token_span: {2, 3},
          source: :mwe_lexicon,
          pos: "adverb"
        }
      ]
    }

    resolved = Decisions.attach(si)
    assert length(resolved.semantic_choices) == 5
    refute Enum.any?(resolved.semantic_choices, &(Map.get(&1, :kind) == :mwe))
    assert length(si.mwe_choices) == 2
  end

  test "degraded comprehension causes an unsupported population claim to be repaired" do
    context = %{
      comprehension: %{degraded?: true},
      symbolic_frame: %{
        type: :location_question,
        subject: "Maple Ridge, BC",
        relation: :population,
        grounding_status: :unverified
      },
      features: %{intent: :ask, conf: 0.9},
      decision: %{}
    }

    {:ok, final, review} =
      ReflectionLoop.review(
        "What is its population?",
        "The population is approximately 10,000.",
        context
      )

    assert review.status == :repair
    assert :geographic_claim_unverified in review.issues
    assert final =~ "don't have a verified population figure"
    refute final =~ "10,000"
  end

  test "degraded comprehension prevents unverified proximity assertions" do
    context = %{
      comprehension: %{degraded?: true},
      symbolic_frame: %{
        type: :location_question,
        subject: "Maple Ridge, BC",
        target: "Vancouver",
        relation: :proximity,
        grounding_status: :unverified
      },
      features: %{intent: :ask, conf: 0.9},
      decision: %{}
    }

    {:ok, final, review} =
      ReflectionLoop.review("Is it close to Vancouver?", "It is not close.", context)

    assert review.status == :repair
    assert final =~ "Maple Ridge, BC"
    assert final =~ "Vancouver"
    assert final =~ "don't have verified distance information"
  end

  test "review leaves unrelated, non-geographic facts alone" do
    {:ok, text, review} =
      ReflectionLoop.review(
        "What is 1 plus 1?",
        "The answer is 2.",
        %{comprehension: %{degraded?: true}, features: %{intent: :ask, conf: 0.9}}
      )

    assert text == "The answer is 2."
    refute :geographic_claim_unverified in review.issues
  end

  test "completed factual answers do not receive curiosity probes" do
    features = %{
      text: "Where is Maple Ridge, BC?",
      selected_action: :answer_user,
      self_state: %{
        uncertainty: 0.9,
        focus: :clarify,
        effects: [:ask_clarifying_question]
      }
    }

    assert {"It is in British Columbia.", nil} =
             SelfStateFeatures.append_curiosity_probe(
               "It is in British Columbia.",
               features,
               %{action: :offer_options},
               %{guardrail?: false},
               nil
             )
  end

  test "generic LLM closing questions are removed only on direct factual answers" do
    context = %{features: %{selected_action: :answer_user}}

    assert AnswerPolish.polish(
             "Where is Maple Ridge?",
             "Maple Ridge is in BC. Is there anything else you would like to know?",
             context
           ) == "Maple Ridge is in BC."

    assert AnswerPolish.polish(
             "Tell me what is troubling you",
             "You sound uncertain. Is there anything else you would like to know?",
             context
           ) =~ "Is there anything else"
  end

  test "prompt exposes explicit antecedent without claiming location verification" do
    prompt =
      LlmPrompt.build_system_prompt(
        %{
          intent: :ask,
          discourse_reference: %{
            pronoun: "it",
            antecedent: "Maple Ridge, BC",
            source: :recent_user_turn
          }
        },
        %{mode: :coach, action: :answer},
        %{},
        []
      )

    assert prompt =~ "it refers to Maple Ridge, BC"
    assert prompt =~ "not the requested geographic fact"
    assert prompt =~ "Do not invent population figures"
  end

  test "selected answer action overrides optional planner offers, not guardrails" do
    decision = %{action: :offer_options, mode: :coach}

    assert %{action: :answer} =
             ActionAlignment.align(decision, :answer_user, "Is it close to Vancouver?", false)

    assert %{action: :offer_options} =
             ActionAlignment.align(decision, :answer_user, "Is it close to Vancouver?", true)

    assert %{action: :offer_options} =
             ActionAlignment.align(decision, :ask_clarifying_question, "Is it close?", false)
  end
end
