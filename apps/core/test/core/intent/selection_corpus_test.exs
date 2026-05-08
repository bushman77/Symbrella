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
end
