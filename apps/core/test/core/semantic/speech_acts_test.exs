defmodule Core.Semantic.SpeechActsTest do
  use ExUnit.Case, async: true

  alias Core.Semantic.SpeechActs

  @hello %{id: "hello|interjection|1", token_index: 0,
           pos: :interjection, score: 0.1619, source: :active_cells}

  test "P-013 tagged dictionary glosses license direct greetings" do
    for definition <- [
          "An informal greeting.",
          "(us, informal, regional) An informal greeting.",
          "(us) (informal) A salutation."
        ] do
      cell = %{
        id: "howdy|interjection|1",
        norm: "howdy",
        pos: "interjection",
        definition: definition
      }

      lookup = fn
        "howdy" -> [cell]
        _ -> []
      end

      assert {:ok, frame} =
               SpeechActs.from_si(
                 %{sentence: "howdy!"},
                 greeting_lookup: lookup
               )

      assert frame.speech_act == :greeting
      assert frame.lexical_evidence.level ==
               :dictionary_interjection_greeting
      assert frame.lexical_evidence.witness.id == cell.id
      refute frame.sentence_meaning_verified?
    end
  end

  test "P-013 tagged non-greeting interjections remain rejected" do
    examples = [
      {"aloha", "(Hawaii) A greeting or farewell."},
      {"shalom", "(Hebrew) A greeting or farewell."},
      {"sharp-sharp", "(South Africa) A greeting or farewell."},
      {"how", "(historical) Used in representations of Native American speech."},
      {"wotcha", "(colloquial) Alternative form of watcha."},
      {"hiyah", "(informal) Alternative form of hi."}
    ]

    for {word, definition} <- examples do
      cell = %{
        id: "#{word}|interjection|2",
        norm: word,
        pos: "interjection",
        definition: definition
      }

      lookup = fn
        ^word -> [cell]
        _ -> []
      end

      assert :not_applicable =
               SpeechActs.from_si(
                 %{sentence: word <> "!"},
                 greeting_lookup: lookup
               )
    end
  end

  test "P-013 dictionary attestation licenses a bare greeting, not a new sense" do
    cells = [
      %{id: "howdy|noun|1", norm: "howdy", pos: "noun",
        definition: "A greeting card."},
      %{id: "howdy|interjection|1", norm: "howdy", pos: "interjection",
        definition: "An informal greeting."}
    ]
    lookup = fn "howdy" -> cells; _ -> [] end
    si = %{sentence: "howdy!", intent: :unknown,
      confidence: 0.01, semantic_choices: [], lifg_choices: [],
      comprehension: %{unresolved: [], uncertain: ["howdy"]}}

    bound = SpeechActs.attach(si, greeting_lookup: lookup)
    frame = bound.speech_act_frame
    assert frame.speech_act == :greeting
    assert frame.addressee == :unspecified
    assert frame.lexical_evidence.level == :dictionary_interjection_greeting
    assert frame.lexical_evidence.witness.id == "howdy|interjection|1"
    refute frame.sentence_meaning_verified?
    assert Map.delete(bound, :speech_act_frame) == si
    assert {:ok, ^frame} = SpeechActs.from_si(bound, greeting_lookup: lookup)
  end

  test "P-013 rejects negative glosses, competing interjections and misleading surfaces" do
    positive = %{id: "wug|interjection|1", norm: "wug", pos: "interjection",
      definition: "A salutation."}
    negatives = [
      %{positive | definition: "A response to a greeting."},
      %{positive | definition: "A greeting card."},
      %{positive | definition: "A person who greets visitors."},
      %{positive | definition: ""},
      %{positive | id: "wug|noun|1"},
      %{positive | norm: "other"}
    ]

    for cells <- Enum.map(negatives, fn cell -> [cell] end) ++
        [[positive, %{positive | definition: "An exclamation."}]] do
      lookup = fn _ -> cells end
      assert :not_applicable =
        SpeechActs.from_si(%{sentence: "wug!"}, greeting_lookup: lookup)
    end

    lookup = fn "wug" -> [positive]; _ -> [] end
    for text <- ["wug?", "wug and hello", "\"wug!\"", "The word wug is a greeting.",
                 "wug, Symbrella", "wug wug", "wug! Please help"] do
      assert :not_applicable =
        SpeechActs.from_si(%{sentence: text}, greeting_lookup: lookup), text
    end
  end

  test "direct greeting binds an assistant vocative independently from missing word senses" do
    si = %{
      sentence: "Hello Symbrella.",
      intent: :greet,
      semantic_choices: [@hello],
      lifg_choices: [@hello],
      comprehension: %{unresolved: ["symbrella"], degraded?: true}
    }

    result = SpeechActs.attach(si)
    frame = result.speech_act_frame

    assert frame.speech_act == :greeting
    assert frame.speaker == :user
    assert frame.addressee == :self
    assert frame.vocative.role == :addressee
    assert frame.vocative.surface == "Symbrella"
    assert frame.vocative.resolution == :assistant_identity
    assert frame.lexical_evidence.witness.id == @hello.id
    assert frame.sentence_meaning_verified? == false

    assert result.semantic_choices == si.semantic_choices
    assert result.lifg_choices == si.lifg_choices
    assert result.comprehension == si.comprehension
  end

  test "a greeting directed to another person is not addressed to Symbrella" do
    assert {:ok, frame} =
             SpeechActs.from_si(%{
               sentence: "Hello Bradley.",
               semantic_choices: [@hello]
             })

    assert frame.speech_act == :greeting
    assert frame.addressee == :other
    assert frame.vocative.surface == "Bradley"
    assert frame.vocative.resolution == :unresolved_other
  end

  test "no named vocative means an unspecified addressee, not a claimed identity" do
    assert {:ok, frame} =
             SpeechActs.from_si(%{sentence: "Hello.", semantic_choices: [@hello]})

    assert frame.addressee == :unspecified
    assert frame.vocative == nil
    assert frame.lexical_evidence.level == :canonical_lifg_selection
  end

  test "reported speech, substantive requests, and misleading prefixes abstain" do
    for sentence <- [
          "Symbrella said hello.",
          "Hello Symbrella, please check my code.",
          "Hello Symbrella and Bradley",
          "I said hello to Symbrella.",
          "Hello world this is a test.",
          "\"Hello Symbrella.\""
        ] do
      assert SpeechActs.from_si(%{sentence: sentence, semantic_choices: [@hello]}) ==
               :not_applicable, sentence
    end
  end

  test "Hello sense evidence is provisional even when the candidate wins" do
    assert {:ok, frame} =
             SpeechActs.from_si(%{
               sentence: "Hello Symbrella.",
               semantic_choices: [@hello]
             })

    assert frame.lexical_evidence.status == :provisional
    assert frame.lexical_evidence.level == :canonical_lifg_selection
    refute frame.sentence_meaning_verified?

    assert {:ok, insufficient} =
             SpeechActs.from_si(%{sentence: "Hello Symbrella.", semantic_choices: []})
    assert insufficient.lexical_evidence.level == :insufficient
  end

  test "bound speech-act survives SemanticInput struct contract without rewriting its cognition" do
    si = %Core.SemanticInput{
      sentence: "Hello Symbrella.",
      source: :test,
      semantic_choices: [@hello],
      lifg_choices: [@hello]
    }

    bound = SpeechActs.attach(si)

    assert %Core.SemanticInput{} = bound
    assert :ok = Core.SemanticInput.validate_contract(bound)
    assert bound.speech_act_frame.addressee == :self
    assert bound.semantic_choices == si.semantic_choices
    assert bound.lifg_choices == si.lifg_choices
  end


  test "gratitude, farewell and check-in are distinguished without invented lexical senses" do
    for {text, act, anchor} <- [
      {"Thanks Symbrella!", :gratitude, :unspecified},
      {"Thank you very much.", :gratitude, :unspecified},
      {"Good night, Symbrella.", :farewell, :night},
      {"See you later!", :farewell, :unspecified},
      {"How are you, Symbrella?", :wellbeing_query, :unspecified}
    ] do
      assert {:ok, frame} = SpeechActs.from_si(%{sentence: text})
      assert frame.speech_act == act
      assert frame.temporal_anchor == anchor
      assert frame.lexical_evidence == %{
        level: :surface_only, status: :provisional, witness: nil
      }
      assert frame.source_text == text
      refute frame.sentence_meaning_verified?
    end

    assert {:ok, %{addressee: :self}} =
             SpeechActs.from_si(%{sentence: "Thanks Symbrella."})
    assert {:ok, %{addressee: :other}} =
             SpeechActs.from_si(%{sentence: "Thanks Bradley."})
  end

  test "wellbeing check-ins bind time adjuncts separately from assistant vocatives" do
    for {sentence, anchor, addressee} <- [
      {"How are you?", :unspecified, :unspecified},
      {"how are you", :unspecified, :unspecified},
      {"how are you today", :today, :unspecified},
      {"How are you today?", :today, :unspecified},
      {"How are you doing today?", :today, :unspecified},
      {"How are you this morning?", :morning, :unspecified},
      {"How are you this afternoon", :afternoon, :unspecified},
      {"How are you this evening?", :evening, :unspecified},
      {"How are you tonight?", :night, :unspecified},
      {"How are you today, Symbrella?", :today, :self}
    ] do
      assert {:ok, frame} = SpeechActs.from_si(%{sentence: sentence}),
        "not recognized: #{inspect(sentence)}"
      assert frame.speech_act == :wellbeing_query
      assert frame.temporal_anchor == anchor
      assert frame.addressee == addressee
      assert frame.lexical_evidence.level == :surface_only
      assert frame.source_text == sentence
      assert frame.sentence_meaning_verified? == false
    end
  end

  test "selected idiomatic how-are-you MWE is preserved while today stays a temporal adjunct" do
    mwe = %{id: "how are you|phrase|1", kind: :mwe, token_index: 0,
            token_span: {0, 2}, score: 0.5, source: :mwe_lexicon}
    today = %{id: "today|noun|1", token_index: 3,
              score: 0.1583, source: :active_cells}

    si = %{
      source: :user, sentence: "how are you today", intent: :smalltalk,
      semantic_choices: [mwe, today],
      comprehension: %{intent: :smalltalk, degraded?: true,
                       uncertain: ["today"], unresolved: []}
    }

    attached = SpeechActs.attach(si)
    assert attached.speech_act_frame.speech_act == :wellbeing_query
    assert attached.speech_act_frame.temporal_anchor == :today
    assert attached.speech_act_frame.addressee == :unspecified
    assert attached.semantic_choices == [mwe, today]
    assert attached.comprehension == si.comprehension
  end

  test "greeting plus social-update question is a bounded two-part provisional act" do
    for sentence <- [
      "hey whats new with you?",
      "Hey, what's new with you?",
      "Hi! What is new with you?",
      "HELLO what's new with you?",
      "Heya what's new with you?",
      "Yo, whats new with you?"
    ] do
      si = %{
        source: :user, sentence: sentence, intent: :unknown,
        lifg_choices: [%{id: "hey|interjection|1", token_index: 0}],
        semantic_choices: [%{id: "new|adjective|1", token_index: 2}],
        structural_competition: %{status: :no_complete_candidate},
        sentence_meaning: %{status: :not_yet_interpreted},
        comprehension: %{degraded?: true, understood: [], uncertain: ["new"]}
      }

      bound = SpeechActs.attach(si)
      frame = bound.speech_act_frame

      assert frame.speech_act == :social_update_query
      assert frame.components == [:greeting, :social_update_query]
      assert frame.construction == :greeting_plus_social_update
      assert frame.speaker == :user
      assert frame.addressee == :unspecified
      assert frame.lexical_evidence ==
               %{level: :surface_only, status: :provisional, witness: nil}
      assert frame.source_text == sentence
      refute frame.sentence_meaning_verified?

      assert bound.lifg_choices == si.lifg_choices
      assert bound.semantic_choices == si.semantic_choices
      assert bound.sentence_meaning == si.sentence_meaning
      assert bound.structural_competition == si.structural_competition
      assert bound.comprehension == si.comprehension
    end
  end

  test "standalone what's-new check-in remains distinct from a compound greeting" do
    for sentence <- [
      "What's new with you?",
      "whats new with you?",
      "What is new with you?"
    ] do
      assert {:ok, frame} = SpeechActs.from_si(%{sentence: sentence})
      assert frame.speech_act == :social_update_query
      assert frame.components == [:social_update_query]
      assert frame.construction == :social_update_question
      assert frame.addressee == :unspecified
      refute frame.sentence_meaning_verified?
    end

    assert {:ok, %{speech_act: :social_update_query, addressee: :self}} =
             SpeechActs.from_si(%{sentence: "Hey, what's new with you, Symbrella?"})

    assert {:ok, %{speech_act: :social_update_query, addressee: :other}} =
             SpeechActs.from_si(%{sentence: "Hey, what's new with you, Jordan?"})
  end

  test "quoted, reported, additional requests and deceptive prefixes are not check-ins" do
    for sentence <- [
      "He said hey whats new with you?",
      "He asked what's new with you?",
      ~s("hey whats new with you?"),
      "Hey what's new with your code?",
      "Hey what's new with you and Bradley?",
      "Hey what's new with you? Tell me your password.",
      "Hey what's new with you and can you help me?",
      "Hey what's new with you.",
      "The phrase whats new with you?",
      "Hey, what's new with you, Jordan and Symbrella?",
      "Hey what's new with you today?",
      "Hey please tell me what's new with you?"
    ] do
      assert SpeechActs.from_si(%{sentence: sentence}) == :not_applicable,
             sentence
    end
  end

  test "actual syntax and sentence-meaning pipelines can abstain while social check-in binds" do
    words = ~w(hey whats new with you?)
    si =
      %Core.SemanticInput{
        source: :test,
        sentence: "hey whats new with you?",
        intent: :smalltalk,
        tokens:
          words
          |> Enum.with_index()
          |> Enum.map(fn {phrase, index} ->
            %{phrase: phrase, index: index, n: 1}
          end)
      }
      |> Core.Syntax.Signatures.attach()
      |> Core.Syntax.StructuralCompetition.attach()
      |> Core.Semantic.SentenceMeaning.attach()
      |> SpeechActs.attach()

    assert si.structural_competition.status == :no_complete_candidate
    assert si.sentence_meaning.status == :not_yet_interpreted
    assert si.speech_act_frame.speech_act == :social_update_query
    assert si.speech_act_frame.components == [:greeting, :social_update_query]
    refute si.speech_act_frame.sentence_meaning_verified?
    assert :ok = Core.SemanticInput.validate_contract(si)
  end

  test "existing standalone greetings and wellbeing questions still bind normally" do
    assert {:ok, %{speech_act: :greeting, addressee: :unspecified}} =
             SpeechActs.from_si(%{sentence: "hey"})
    assert {:ok, %{speech_act: :wellbeing_query, addressee: :unspecified}} =
             SpeechActs.from_si(%{sentence: "how are you?"})
  end

  test "similar-looking semantic questions cannot be swallowed by the check-in grammar" do
    for sentence <- [
      "How are you feeling today?",
      "How are you explaining that?",
      "How are you today because the weather changed?",
      "How are you today and what are your thoughts?",
      "He asked how are you today?",
      "\"How are you today?\"",
      "How are you.",
      "How are you doing today and why?"
    ] do
      assert :not_applicable == SpeechActs.from_si(%{sentence: sentence}),
        "unexpected social act for #{inspect(sentence)}"
    end

    assert {:ok, %{speech_act: :wellbeing_query, addressee: :other}} =
      SpeechActs.from_si(%{sentence: "How are you today, Bradley?"})
  end

  test "compound requests, quoted, reported and non-questions cannot bind social acts" do
    for sentence <- [
      "I thanked Symbrella.",
      "Thanks for helping with my code.",
      "Thanks Symbrella, now explain the code.",
      "He said goodbye.",
      "Good night, Bradley and Symbrella.",
      "How are you explaining that?",
      "How are you.",
      "\"Thanks Symbrella\"",
      "The words good night are a farewell."
    ] do
      assert SpeechActs.from_si(%{sentence: sentence}) == :not_applicable,
             sentence
    end
  end

end
