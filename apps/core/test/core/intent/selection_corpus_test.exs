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
    {"hey symbrella how are you on this fine saturday morning", :smalltalk},
    {"I feel pretty tired today", :tell},
    {"I prefer concise answers", :tell},
    {"Actually I meant the room is cold", :correction},
    {"yes that is right", :affirm},
    {"no thanks", :deny},
    {"bye for now", :bye},
    {"this system is learning context", :statement},
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

  test "social opener preserves environmental temperature observation" do
    si =
      Selection.select(%{
        sentence: "good morning Symbrella. It feel pretty cold in here today",
        tokens: [],
        trace: []
      })

    assert si.intent == :environment_observation
    assert si.opener_intent == :greet
    assert si.opener_text == "good morning symbrella"
    assert si.primary_text == "it feel pretty cold in here today"
    assert si.keyword == "it feel pretty cold in here today"
    assert si.conversation_act == :environment_observation
    assert si.topic_domain == :environment_temperature
    refute Map.has_key?(si, :fuzzy_text)
    assert [%{role: :winner, intent: :environment_observation} | _] = si.intent_evidence
  end

  test "social opener plus ambient chilly morning is environmental context" do
    si =
      Selection.select(%{
        sentence: "good morning Symbrella, it is pretty chilly this morning",
        tokens: [],
        trace: []
      })

    assert si.intent == :environment_observation
    assert si.opener_intent == :greet
    assert si.primary_text == "it is pretty chilly this morning"
    assert si.conversation_act == :environment_observation
    assert si.topic_domain == :environment_temperature
    assert si.context_frame.subject == :environment
    assert si.context_frame.attribute == :temperature
    assert si.context_frame.state == :cold
    assert si.context_frame.temporal_reference == :morning
  end

  test "symbolic affect disclosure carries context frame" do
    si = Selection.select(%{sentence: "I feel pretty tired today", tokens: [], trace: []})

    assert si.intent == :tell
    assert si.conversation_act == :affect_disclosure
    assert si.topic_domain == :personal_state
    assert si.context_frame.subject == :user
    assert si.context_frame.attribute == :affect
    assert si.context_frame.state == :tired
    assert si.context_frame.temporal_reference == :today
    assert si.context_frame.certainty == :high
  end

  test "symbolic correction beats environmental observation but keeps repair context" do
    si = Selection.select(%{sentence: "Actually I meant the room is cold", tokens: [], trace: []})

    assert si.intent == :correction
    assert si.conversation_act == :correction
    assert si.topic_domain == :conversation_repair
    assert si.context_frame.subject == :prior_context
    assert si.context_frame.state == :needs_repair
    assert [%{role: :winner, intent: :correction} | _] = si.intent_evidence
  end

  test "symbolic preference statement carries user preference context" do
    si = Selection.select(%{sentence: "I prefer concise answers", tokens: [], trace: []})

    assert si.intent == :tell
    assert si.conversation_act == :preference_statement
    assert si.topic_domain == :user_preference
    assert si.context_frame.subject == :user
    assert si.context_frame.attribute == :preference
    assert si.context_frame.polarity == :positive
  end
end
