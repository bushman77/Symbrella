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
