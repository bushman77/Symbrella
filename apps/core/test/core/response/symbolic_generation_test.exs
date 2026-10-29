defmodule Core.Response.SymbolicGenerationTest do
  use ExUnit.Case, async: false

  alias Core.Semantic.SpeechActs
  alias Core.Response
  alias Core.Response.SymbolicGeneration

  @hello_choice %{
    token_index: 0, id: "hello|interjection|1", pos: "interjection",
    lemma: "hello", source: :active_cells, score: 0.16190361976623535
  }

  setup do
    original = Application.get_env(:core, :llm_inference_enabled?, :unset)
    Application.put_env(:core, :llm_inference_enabled?, false)

    on_exit(fn ->
      if original == :unset do
        Application.delete_env(:core, :llm_inference_enabled?)
      else
        Application.put_env(:core, :llm_inference_enabled?, original)
      end
    end)

    :ok
  end

  test "a canonical lexical greeting and identity vocative generate a supported reply" do
    si =
      %{
        sentence: "Hello Symbrella.",
        intent: :greet,
        confidence: 0.76,
        text: "Hello Symbrella.",
        semantic_choices: [@hello_choice],
        lifg_choices: [@hello_choice],
        comprehension: %{intent: :greet, degraded?: true, unresolved: ["symbrella"]},
        session_id: "symbolic-greeting-#{System.unique_integer([:positive])}"
      }
      |> SpeechActs.attach()

    assert si.speech_act_frame.addressee == :self
    assert si.speech_act_frame.vocative.resolution == :assistant_identity
    assert si.speech_act_frame.lexical_evidence.level == :canonical_lifg_selection

    {_tone, reply, meta} = Response.plan(si, %{})
    assert reply == "Hello."
    assert meta.response_source == :symbolic_generation
    assert meta.response_fallback_reason == nil
    assert meta.curiosity_probe == nil
    assert meta.response_reflection.status == :symbolic_realized
    assert meta.response_reflection.speaker == :user
    assert meta.response_reflection.addressee == :self
    assert meta.response_reflection.vocative.resolution == :assistant_identity
    assert meta.response_reflection.lexical_witness.id == "hello|interjection|1"
    assert meta.response_reflection.provenance == :canonical_lifg_choice
    assert meta.response_reflection.semantic_verified? == false
    assert meta.response_reflection.llm_called? == false
  end

  test "wrong or missing canonical witness never licenses Hello" do
    for choices <- [
          nil,
          [],
          [%{@hello_choice | id: "hello|noun|1"}],
          [%{@hello_choice | token_index: 1}],
          [%{@hello_choice | pos: "noun"}],
          [%{@hello_choice | score: 0.0}]
        ] do
      si =
        SpeechActs.attach(%{sentence: "Hello Symbrella.", semantic_choices: choices})

      assert si.speech_act_frame.lexical_evidence.level == :insufficient

      assert :not_applicable =
               SymbolicGeneration.generate(
                 "Hello Symbrella.",
                 %{risk_bucket: :low, intent: :greet},
                 %{action: :answer},
                 si
               )
    end
  end

  test "stale lexical evidence alone is insufficient without a bound speech-act frame" do
    assert :not_applicable =
             SymbolicGeneration.generate(
               "Hello Symbrella.",
               %{risk_bucket: :low, intent: :greet},
               %{action: :answer},
               %{semantic_choices: [@hello_choice]}
             )
  end

  test "unsupported Hello yields the explicit no-LLM result" do
    si =
      %{intent: :unknown, confidence: 0.8, sentence: "Hello Symbrella.",
        text: "Hello Symbrella.",
        session_id: "symbolic-no-evidence-#{System.unique_integer([:positive])}"}
      |> SpeechActs.attach()

    {_tone, reply, meta} = Response.plan(si, %{})
    assert reply =~ "Symbolic response unavailable"
    assert meta.response_source == :model_unavailable
    assert meta.response_fallback_reason == :llm_disabled
  end

  test "surface-only greetings remain available with their weaker provenance" do
    for {input, expected} <- [
          {"hi", "Hello."},
          {"Good morning, Symbrella!", "Good morning."},
          {"Good afternoon Symbrella", "Good afternoon."},
          {"good eveing symbrella", "Good evening."}
        ] do
      si =
        %{intent: :unknown, confidence: 0.85, sentence: input, text: input,
          session_id: "symbolic-other-#{System.unique_integer([:positive])}"}
        |> SpeechActs.attach()

      {_tone, reply, meta} = Response.plan(si, %{})
      assert reply == expected, input
      assert meta.response_source == :symbolic_generation
      assert meta.response_reflection.provenance == :surface_greeting
      assert meta.response_reflection.lexical_witness == nil
    end
  end

  test "other addressees, reported speech and mixed turns do not become self-greetings" do
    features = %{risk_bucket: :low, intent: :greet}
    decision = %{action: :answer}

    other =
      SpeechActs.attach(%{
        sentence: "Hello Bradley.",
        semantic_choices: [@hello_choice]
      })

    assert other.speech_act_frame.speech_act == :greeting
    assert other.speech_act_frame.addressee == :other
    assert other.speech_act_frame.vocative.resolution == :unresolved_other
    assert :not_applicable =
             SymbolicGeneration.generate("Hello Bradley.", features, decision, other)

    for sentence <- [
          "Symbrella said hello.",
          "Hello Symbrella, how are you?",
          "Hello Symbrella and Bradley.",
          "The word hello is an interjection."
        ] do
      si = SpeechActs.attach(%{sentence: sentence, semantic_choices: [@hello_choice]})
      assert is_nil(si.speech_act_frame), sentence
      assert :not_applicable =
               SymbolicGeneration.generate(sentence, features, decision, si)
    end
  end

  test "input text, selected action and guardrail must match the bound frame" do
    si =
      SpeechActs.attach(%{
        sentence: "Hello Symbrella.",
        semantic_choices: [@hello_choice]
      })

    assert :not_applicable =
             SymbolicGeneration.generate(
               "Hello Bradley.", %{risk_bucket: :low, intent: :greet},
               %{action: :answer}, si)

    assert :not_applicable =
             SymbolicGeneration.generate(
               "Hello Symbrella.", %{risk_bucket: :high, intent: :greet},
               %{action: :answer}, si)

    assert :not_applicable =
             SymbolicGeneration.generate(
               "Hello Symbrella.", %{risk_bucket: :low, intent: :greet},
               %{action: :suppress}, si)
  end

  test "native responses cover simple gratitude, farewell and wellbeing turns" do
    for {input, expected, act} <- [
      {"Thank you, Symbrella!", "You're welcome.", :gratitude},
      {"Thanks a lot.", "You're welcome.", :gratitude},
      {"Good night, Symbrella.", "Good night.", :farewell},
      {"See you later!", "Take care.", :farewell},
      {"How are you, Symbrella?", "I'm here and ready to talk.", :wellbeing_query}
    ] do
      si =
        %{sentence: input, text: input, intent: :unknown, confidence: 0.7,
          session_id: "native-social-#{System.unique_integer([:positive])}"}
        |> SpeechActs.attach()

      {_tone, reply, meta} = Response.plan(si, %{})
      assert reply == expected, input
      assert meta.response_source == :symbolic_generation
      assert meta.response_reflection.interpreted_speech_act == act
      assert meta.response_reflection.provenance == :surface_social_act
      assert meta.response_reflection.semantic_verified? == false
      assert meta.response_reflection.llm_called? == false
    end
  end

  test "live smalltalk how-are-you MWE plus uncertain today generates wellbeing response" do
    for question <- [
      "how are you today",
      "How are you today?",
      "How are you doing today?",
      "How are you this evening?",
      "How are you today, Symbrella?"
    ] do
      mwe = %{
        id: "how are you|phrase|1", kind: :mwe, token_index: 0,
        token_span: {0, 2}, source: :mwe_lexicon, score: 0.5
      }

      si =
        %{
          source: :user, sentence: question, text: question,
          intent: :smalltalk, confidence: 0.655,
          session_id: "wellbeing-live-#{System.unique_integer([:positive])}",
          semantic_choices: [mwe, %{
            id: "today|noun|1", token_index: 3,
            source: :active_cells, score: 0.1583
          }],
          comprehension: %{
            intent: :smalltalk, recognized_topics: ["today"],
            understood: [], uncertain: ["today"], unresolved: [],
            degraded?: true, reasons: [:weak_decision_rate_high]
          }
        }
        |> SpeechActs.attach()

      assert si.speech_act_frame.speech_act == :wellbeing_query
      assert si.speech_act_frame.lexical_evidence.level == :surface_only

      {_tone, reply, meta} = Response.plan(si, %{})
      assert reply == "I'm here and ready to talk.", question
      assert meta.response_source == :symbolic_generation
      assert meta.response_reflection.interpreted_speech_act == :wellbeing_query
      assert meta.response_reflection.llm_called? == false
      assert meta.response_reflection.factual_claims? == false
      assert meta.curiosity_probe == nil
    end
  end

  test "response attachment carries today's wellbeing frame to the LiveView reply" do
    sentence = "how are you today"
    si =
      %{
        source: :user, sentence: sentence, text: sentence,
        intent: :smalltalk, confidence: 0.655,
        session_id: "wellbeing-response-attach-#{System.unique_integer([:positive])}",
        semantic_choices: [
          %{id: "how are you|phrase|1", kind: :mwe, token_index: 0,
            token_span: {0, 2}, score: 0.5, source: :mwe_lexicon},
          %{id: "today|noun|1", token_index: 3,
            score: 0.1583, source: :active_cells}
        ],
        comprehension: %{
          intent: :smalltalk, recognized_topics: ["today"], understood: [],
          uncertain: ["today"], unresolved: [], degraded?: true,
          reasons: [:weak_decision_rate_high]
        }
      }
      |> SpeechActs.attach()

    attached = Core.Response.Attach.maybe_build_response_plan(si, [])
    assert attached.speech_act_frame.temporal_anchor == :today
    assert attached.response_text == "I'm here and ready to talk."
    assert attached.response_meta.response_source == :symbolic_generation
    assert attached.response_meta.response_reflection.interpreted_speech_act ==
             :wellbeing_query
    assert attached.response_meta.response_reflection.llm_called? == false
  end

  test "wellbeing questions to other people never produce Symbrella's check-in" do
    sentence = "How are you today, Bradley?"

    si =
      %{source: :user, sentence: sentence, text: sentence,
        intent: :smalltalk, confidence: 0.7,
        session_id: "wellbeing-other-#{System.unique_integer([:positive])}"}
      |> SpeechActs.attach()

    assert si.speech_act_frame.addressee == :other
    assert :not_applicable =
      SymbolicGeneration.generate(sentence,
        %{risk_bucket: :low, intent: :smalltalk},
        %{action: :answer}, si)
  end

  test "foreign addressees and unsafe decisions do not generate native social replies" do
    for text <- [
      "Thanks Bradley.", "Good night, Bradley.",
      "How are you, Bradley?", "Thanks Symbrella, now explain the code."
    ] do
      si = SpeechActs.attach(%{sentence: text})
      assert :not_applicable =
               SymbolicGeneration.generate(text, %{risk_bucket: :low, intent: :unknown},
                 %{action: :offer_options}, si)
    end

    text = "Thanks Symbrella."
    si = SpeechActs.attach(%{sentence: text})

    forged = %{si | sentence: "Thanks for helping with my code."}
    assert :not_applicable =
             SymbolicGeneration.generate(text,
               %{risk_bucket: :low, intent: :gratitude},
               %{action: :offer_options}, forged)
    assert :not_applicable =
             SymbolicGeneration.generate(text,
               %{risk_bucket: :high, intent: :gratitude},
               %{action: :offer_options}, si)
  end

  test "revalidated action receives attributed response without verifying its truth" do
    choices = [
      %{token_index: 1, id: "artist|noun|1"},
      %{token_index: 2, id: "draws|verb|1"},
      %{token_index: 4, id: "picture|noun|1"}
    ]

    si =
      %{
        sentence: "The artist draws a picture.", text: "The artist draws a picture.",
        source: :user, intent: :tell, confidence: 0.8, trace: [],
        session_id: "native-svo-#{System.unique_integer([:positive])}",
        tokens:
          ["The", "artist", "draws", "a", "picture."]
          |> Enum.with_index()
          |> Enum.map(fn {word, index} -> %{index: index, phrase: word, n: 1} end),
        active_cells: [
          %{id: "draws|verb|1", norm: "draws", pos: "verb",
            definition: "An inflected form of draw.",
            semantic_atoms: ~w(tag:form-of tag:indicative tag:present tag:singular tag:third-person)}
        ],
        sentence_signatures: [
          %{id: :finite_present_svo_candidate, source: :core_syntax,
            span: {0, 4},
            evidence: [:contiguous_surface, :attested_nominal_subject,
                       :attested_nominal_object, :attested_finite_present_verb],
            roles: %{subject_determiner: 0, subject_head: 1, predicate: 2,
                     object_determiner: 3, object_head: 4}}
        ],
        lifg_choices: choices,
        atl_slate: %{winners: choices}
      }
      |> Core.Semantic.EpisodicProjection.attach()
      |> Core.Semantic.SentenceMeaning.attach()
      |> SpeechActs.attach()

    assert si.sentence_meaning.kind == :participant_assertion
    {_tone, reply, meta} = Response.plan(si, %{})
    assert reply == "You're saying that the artist draws a picture."
    assert meta.response_source == :symbolic_generation
    assert meta.response_reflection.speech_act == :acknowledge_user_report
    assert meta.response_reflection.provenance == :revalidated_episodic_projection
    assert meta.response_reflection.event_occurrence_verified? == false
    assert meta.response_reflection.response_is_memory_acknowledgment? == false
    assert meta.response_reflection.llm_called? == false

    # Exercise the real Response.Attach SI projection, not just plan/2:
    # dropping sentence_meaning at this boundary would disable the generator.
    attached = Core.Response.Attach.maybe_build_response_plan(si, [])
    assert attached.response_text == reply
    assert attached.response_meta.response_source == :symbolic_generation

    # Core's ordinary helpful :tell path uses :act_first.
    assert {:ok, "You're saying that the artist draws a picture.", _} =
             SymbolicGeneration.generate(si.text,
               %{risk_bucket: :low, intent: :tell},
               %{action: :act_first}, si)

    forged = %{si | source: :assistant}
    assert :not_applicable =
             SymbolicGeneration.generate(forged.text,
               %{risk_bucket: :low, intent: :statement},
               %{action: :answer}, forged)

    invented = put_in(si.sentence_meaning.roles.patient.surface, "a castle")
    assert :not_applicable =
             SymbolicGeneration.generate(invented.text,
               %{risk_bucket: :low, intent: :statement},
               %{action: :answer}, invented)

    assert :not_applicable =
             SymbolicGeneration.generate(si.text,
               %{risk_bucket: :low, intent: :statement},
               %{action: :suppress}, si)
  end


  test "native generation composes validated progressive spatial reports" do
    words = ~w(A bird is sitting on the fence.)
    ids = [
      "a|determiner|1", "bird|noun|1", "is|verb|1",
      "sitting|adjective|1", "on|preposition|1",
      "the|determiner|1", "fence|noun|1"
    ]
    choices = ids |> Enum.with_index() |>
      Enum.map(fn {id, index} -> %{id: id, token_index: index} end)

    si =
      %{
        sentence: "A bird is sitting on the fence.",
        text: "A bird is sitting on the fence.",
        source: :user, intent: :tell, trace: [], confidence: 0.75,
        session_id: "native-spatial-#{System.unique_integer([:positive])}",
        tokens: words |> Enum.with_index() |>
          Enum.map(fn {word, index} -> %{index: index, phrase: word, n: 1} end),
        active_cells: [
          %{id: "a|determiner|1", norm: "a", pos: "determiner"},
          %{id: "bird|noun|1", norm: "bird", pos: "noun"},
          %{id: "is|verb|1", norm: "is", pos: "verb"},
          %{id: "sitting|adjective|1", norm: "sitting", pos: "adjective"},
          %{id: "sitting|verb|1", norm: "sitting", pos: "verb",
            semantic_atoms: ~w(tag:form-of tag:present tag:participle)},
          %{id: "on|preposition|1", norm: "on", pos: "preposition"},
          %{id: "the|determiner|1", norm: "the", pos: "determiner"},
          %{id: "fence|noun|1", norm: "fence", pos: "noun"}
        ],
        lifg_choices: choices,
        atl_slate: %{winners: choices}
      }
      |> Core.Syntax.Signatures.attach()
      |> Core.Semantic.EpisodicProjection.attach()
      |> Core.Semantic.SentenceMeaning.attach()
      |> SpeechActs.attach()

    assert si.sentence_meaning.event_type == :spatial
    {_tone, reply, meta} = Response.plan(si, %{})
    assert reply == "You're describing a bird as sitting on the fence."
    assert meta.response_source == :symbolic_generation
    assert meta.response_reflection.event_kind == :spatial
    assert meta.response_reflection.event_occurrence_verified? == false
    assert meta.response_reflection.llm_called? == false
  end


  test "live classified feedback and bye intents route through native social realization" do
    for {input, observed_intent, expected, act} <- [
      {"Thank you, Symbrella!", :feedback, "You're welcome.", :gratitude},
      {"see you later!", :bye, "Take care.", :farewell}
    ] do
      # Live Core intent selection supplies these labels, not :unknown.
      assert Core.Response.Policy.normalize_intent(observed_intent, input) == observed_intent

      si =
        %{
          sentence: input, text: input, intent: observed_intent, confidence: 0.84,
          session_id: "live-social-intent-#{System.unique_integer([:positive])}"
        }
        |> SpeechActs.attach()

      assert si.speech_act_frame.speech_act == act
      {_tone, response, meta} = Response.plan(si, %{})

      assert response == expected, input
      assert meta.response_source == :symbolic_generation
      assert meta.response_reflection.interpreted_speech_act == act
      assert meta.response_reflection.llm_called? == false

      attached = Core.Response.Attach.maybe_build_response_plan(si, [])
      assert attached.response_text == expected
      assert attached.response_meta.response_source == :symbolic_generation
    end
  end

  test "feedback and bye labels alone never license a reply without a real social act" do
    for {text, intent} <- [
      {"The reviewer left feedback on the code.", :feedback},
      {"I need to buy some groceries.", :bye},
      {"See you later, Bradley.", :bye},
      {"Thank you Symbrella, but could you debug this?", :feedback}
    ] do
      si = %{sentence: text, text: text, intent: intent} |> SpeechActs.attach()
      assert :not_applicable ==
               SymbolicGeneration.generate(text,
                 %{risk_bucket: :low, intent: intent},
                 %{action: :offer_options}, si)
    end
  end

end
