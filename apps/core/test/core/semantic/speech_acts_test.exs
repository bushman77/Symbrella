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
end
