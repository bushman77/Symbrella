defmodule Core.Response.GeographicContinuityTest do
  use ExUnit.Case, async: false

  alias Core.Discourse.References
  alias Core.MWE.Decisions
  alias Core.Response.ActionAlignment
  alias Core.Response.AnswerPolish
  alias Core.Response.Context
  alias Core.Response.LlmChatHistory
  alias Core.Response.LlmPrompt
  alias Core.Response.ReflectionLoop
  alias Core.Response.SelfStateFeatures
  alias Core.Semantic.EventFrames

  setup do
    {:ok, session_id: "geo-#{System.unique_integer([:positive])}"}
  end

  test "distance follow-up preserves the place referent and requests coordinates", %{
    session_id: session_id
  } do
    LlmChatHistory.record_turn(session_id, "where is maple ridge bc", "Maple Ridge is in BC.")

    si =
      %{sentence: "how far is it from kamloops bc", session_id: session_id}
      |> References.attach()
      |> EventFrames.attach()

    assert si.discourse_reference.antecedent == "maple ridge bc"
    assert si.symbolic_frame.type == :location_question
    assert si.symbolic_frame.relation == :proximity
    assert si.symbolic_frame.subject == "maple ridge bc"
    assert si.symbolic_frame.target == "kamloops bc"
    assert si.symbolic_frame.grounding_status == :unverified

    assert {query, :proximity} =
      Core.Tools.WebRetrieval.query_for(%{
        user_text: si.sentence,
        features: %{symbolic_frame: si.symbolic_frame}
      })

    assert query =~ "kamloops bc"
    refute Map.has_key?(si.symbolic_frame, :distance_km)
  end

  test "directly named place distance questions always demand coordinate grounding" do
    for sentence <- [
          "how far is maple ridge from kamloops",
          "How far is Maple Ridge, BC from Kamloops?",
          "How far is Prince George from Victoria?"
        ] do
      si =
        %{sentence: sentence}
        |> References.attach()
        |> EventFrames.attach()

      assert %{type: :location_question, relation: :proximity,
               grounding_status: :unverified, unresolved_roles: []} =
               si.symbolic_frame

      assert is_binary(si.symbolic_frame.subject)
      assert is_binary(si.symbolic_frame.target)
      assert si.symbolic_frame.antecedent_source == :explicit_user_question

      assert %{relation: :proximity, request_type: :geographic_proximity} =
               Core.Tools.WebRetrieval.route_for(%{
                 user_text: sentence,
                 features: %{symbolic_frame: si.symbolic_frame}
               })
    end

    si = EventFrames.attach(%{sentence: "how far is maple ridge from kamloops"})
    assert si.symbolic_frame.subject == "maple ridge"
    assert si.symbolic_frame.target == "kamloops"

    context = %{
      features: %{intent: :ask, conf: 0.9, symbolic_frame: si.symbolic_frame}
    }

    {:ok, answer, reflection} =
      ReflectionLoop.review(si.sentence, "Maple Ridge is 10 miles from Kamloops.", context)

    assert reflection.status == :repair
    assert :geographic_claim_unverified in reflection.issues
    assert answer =~ "don't have verified distance information"
    refute answer =~ "10 miles"
  end

  test "quoted direct-distance questions retain both named roles and route coordinates" do
    for question <- [
          "How far is Maple Ridge, BC from Kamloops, BC?",
          "How far is Maple Ridge, BC from Kamloops, BC?”",
          "“How far is Maple Ridge, BC from Kamloops, BC?”"
        ] do
      assert %{type: :location_question, relation: :proximity,
               subject: "Maple Ridge, BC", target: "Kamloops, BC",
               grounding_status: :unverified} =
               EventFrames.location_question(question)

      si = EventFrames.attach(%{sentence: question})

      assert si.symbolic_frame.subject == "Maple Ridge, BC"
      assert si.symbolic_frame.target == "Kamloops, BC"

      assert %{relation: :proximity, request_type: :geographic_proximity,
               trigger: :symbolic_frame} =
               Core.Tools.WebRetrieval.route_for(%{
                 user_text: question,
                 features: %{symbolic_frame: si.symbolic_frame}
               })
    end
  end

  test "response review independently rejects hallucinated mileage if frame is lost" do
    question = "How far is Maple Ridge, BC from Kamloops, BC?”"
    guessed = "Maple Ridge, BC is approximately 120 miles away from Kamloops, BC."

    # Deliberately omit the geographic frame: this was the live failure mode.
    context = %{features: %{intent: :ask, conf: 0.9}}

    {:ok, response, reflection} = ReflectionLoop.review(question, guessed, context)

    assert reflection.status == :repair
    assert :geographic_claim_unverified in reflection.issues
    assert response =~ "Maple Ridge, BC"
    assert response =~ "Kamloops, BC"
    assert response =~ "don't have verified distance information"
    refute response =~ "120 miles"

    assert {:ok, ^guessed, %{status: :accept}} =
             ReflectionLoop.review(
               "Please repeat the following quotation",
               guessed,
               context
             )
  end

  test "short it followup binds to immediately preceding distance question", %{
    session_id: session_id
  } do
    question = "How far is Maple Ridge, BC from Kamloops, BC?"
    grounded =
      "Based on Wikidata coordinates, Maple Ridge, BC and Kamloops, BC " <>
        "are approximately 229.0 km apart in a straight line (not driving distance). " <>
        "Sources: https://www.wikidata.org/wiki/Q16740 and " <>
        "https://www.wikidata.org/wiki/Q473209"

    LlmChatHistory.record_turn(session_id, question, grounded)
    binding = References.resolve("what is it", session_id)

    assert %{pronoun: "it", source: :recent_distance_turn,
             evidence: :explicit_distance_question, confidence: 0.75,
             subject: "Maple Ridge, BC", target: "Kamloops, BC"} = binding

    si = References.attach(%{session_id: session_id, sentence: "what is it"})
    assert si.discourse_reference == binding
    assert binding.antecedent == "the distance between Maple Ridge, BC and Kamloops, BC"

    # This reference gives the topic, not another coordinate claim.
    refute Map.has_key?(binding, :distance_km)

    prompt = LlmPrompt.build_system_prompt(
      %{intent: :ask, discourse_reference: binding},
      %{mode: :collaborator, action: :answer},
      %{},
      []
    )

    assert prompt =~ "immediately preceding question about distance"
    assert prompt =~ "Maple Ridge, BC"
    assert prompt =~ "Kamloops, BC"
    assert prompt =~ "Do not introduce yourself"

    context = %{features: %{intent: :ask, conf: 0.9, discourse_reference: binding}}
    draft =
      "It's a question and answer system. I'm here to provide information " <>
        "and answer your questions. How can I assist you today?"

    {:ok, corrected, reflection} =
      ReflectionLoop.review("what is it", draft, context)

    assert reflection.status == :repair
    assert :distance_followup_subject_loss in reflection.issues
    assert corrected =~ "straight-line distance between Maple Ridge, BC and Kamloops, BC"
    assert corrected =~ "Wikidata coordinates"
    refute corrected =~ "question and answer system"

    {:ok, correct_reply, accepted} = ReflectionLoop.review(
      "what is it",
      "It's the straight-line distance between Maple Ridge and Kamloops, not a driving distance.",
      context
    )

    assert accepted.status == :accept
    assert correct_reply =~ "straight-line distance"
  end

  test "what is it does not borrow a distant topic or invent an assistant identity", %{
    session_id: session_id
  } do
    assert References.resolve("what is it", session_id) == nil
    assert References.resolve("what is it", session_id <> "-other") == nil

    LlmChatHistory.record_turn(session_id,
      "How far is Maple Ridge, BC from Kamloops, BC?",
      "229 km straight line."
    )
    LlmChatHistory.record_turn(session_id, "Good morning", "Good morning!")

    assert References.resolve("what is it", session_id) == nil
    assert References.resolve("What is its population?", session_id) == nil

    # An explicit assistant-identity question must not be overwritten.
    assert References.resolve("What is Symbrella?", session_id) == nil
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

  test "real response context preserves the geographic frame for reflection", %{
    session_id: session_id
  } do
    LlmChatHistory.record_turn(session_id, "Where is Maple Ridge, BC?", "British Columbia")

    si =
      %{session_id: session_id, sentence: "Is it close to Vancouver?", intent: :ask}
      |> References.attach()
      |> EventFrames.attach()

    compact = Context.from_si(si)

    assert compact.symbolic_frame.event.type == :location_question
    assert compact.symbolic_frame.event.subject == "Maple Ridge, BC"
    assert compact.symbolic_frame.event.target == "Vancouver"
    assert compact.symbolic_frame.event.grounding_status == :unverified

    context =
      Context.from_response_parts(
        si.sentence,
        %{
          session_id: session_id,
          intent: :ask,
          conf: 0.9,
          comprehension: %{degraded?: true},
          symbolic_frame: si.symbolic_frame,
          turn_context: compact
        },
        %{action: :answer},
        %{},
        %{}
      )

    {:ok, final, review} =
      ReflectionLoop.review(
        si.sentence,
        "Maple Ridge is not close to Vancouver.",
        context
      )

    assert review.status == :repair
    assert :geographic_claim_unverified in review.issues
    assert final =~ "Maple Ridge, BC"
    assert final =~ "verified distance information"
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
    assert prompt =~ "disclose uncertainty instead of inventing numerical figures or geographical facts"
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
