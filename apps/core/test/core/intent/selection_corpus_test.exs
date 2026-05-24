defmodule Core.Intent.SelectionCorpusTest do
  use ExUnit.Case, async: true

  alias Core.Intent.Selection

  @cases [
    {"can you explain why recall failed?", :debug},
    {"what did I tell you about my sister?", :ask_info},
    {"remember that I live in Richmond", :memory_write},
    {"fix the pmtg ambiguity issue", :debug},
    {"what does hippocampus mean?", :define},
    {"how does Symbrella figure out intent?", :brain_introspect},
    {"please run mix compile", :command},
    {"translate hola into English", :translate},
    {"thanks, that helped", :feedback},
    {"hello there", :greet}
  ]

  describe "semantic intent corpus" do
    for {text, expected} <- @cases do
      @text text
      @expected expected

      test "#{text} -> #{expected}" do
        si = Selection.select(%{sentence: @text, tokens: [], trace: []})

        assert si.intent == @expected
        assert is_number(si.confidence)
        assert si.confidence > 0.0
        assert [%{role: :winner, intent: @expected} | _] = si.intent_evidence
      end
    end
  end

  test "unknown stays low confidence instead of pretending to understand" do
    si = Selection.select(%{sentence: "blorf nindle zup", tokens: [], trace: []})

    assert si.intent == :unknown
    assert si.confidence <= 0.4
  end

  test "uses fuzzy cue repair before symbolic scoring" do
    si = Selection.select(%{sentence: "wahts my naem?", tokens: [], trace: []})

    assert si.intent == :ask_info
    assert si.keyword == "whats my name"
    assert %{text: "whats my name?", aliases: [:asking_for_user_name]} = si.fuzzy_text
    assert [%{role: :winner, intent: :ask_info} | _] = si.intent_evidence
    assert Enum.any?(si.intent_evidence, &(&1.role == :fuzzy_correction))
    assert Enum.any?(si.intent_evidence, &(&1.role == :fuzzy_alias))
  end

  test "social opener does not dominate medication and sleep disclosure" do
    si =
      Selection.select(%{
        sentence:
          "Good morning, I've been having trouble sleeping because I forgot my quitiapine",
        tokens: [],
        trace: []
      })

    assert si.intent == :health_support
    assert si.opener_intent == :greet
    assert si.opener_text == "good morning"
    assert si.primary_text == "i've been having trouble sleeping because i forgot my quetiapine"
    assert si.conversation_act == :personal_disclosure
    assert si.topic_domain == :health_sleep_medication

    assert [%{role: :winner, intent: :health_support} | _] = si.intent_evidence
    assert Enum.any?(si.intent_evidence, &(&1.role == :social_opener))
  end

  test "social opener keeps primary question as main intent" do
    si = Selection.select(%{sentence: "Yo, do you remember my name?", tokens: [], trace: []})

    assert si.intent == :ask_info
    assert si.opener_intent == :greet
    assert si.primary_text == "do you remember my name"
  end
end
