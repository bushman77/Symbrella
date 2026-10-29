defmodule Core.Semantic.SpeechActsTest do
  use ExUnit.Case, async: true

  alias Core.Semantic.SpeechActs

  @hello %{id: "hello|interjection|1", token_index: 0,
           pos: :interjection, score: 0.1619, source: :active_cells}

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
