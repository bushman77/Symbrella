defmodule Core.Intent.SelectionTest do
  use ExUnit.Case, async: true
  alias Core.Intent.Selection

  defp intent(s), do: Selection.select(%{sentence: s}).intent
  defp conf(s), do: Selection.select(%{sentence: s}).confidence

  test "elongated greeting (hello?! → greet, strong conf)" do
    assert intent("Heeellooo?!") == :greet
    assert conf("Heeellooo?!") >= 0.70
  end

  test "abuse phrase is decisive (fuck you!! → abuse, ~0.98)" do
    assert intent("fuck you!!") == :abuse
    assert conf("fuck you!!") >= 0.95
  end

  test "insult pattern (you're an idiot → insult)" do
    assert intent("you're an idiot") == :insult
    assert conf("you're an idiot") >= 0.70
  end

  test "translate intent (translate hola to english)" do
    assert intent("translate hola to english") == :translate
    assert conf("translate hola to english") >= 0.80
  end

  test "question shape (what time is it? → ask, high conf)" do
    assert intent("what time is it?") == :ask
    assert conf("what time is it?") >= 0.85
  end

  test "expressive greeting does not leak into ask" do
    assert intent("hey??!") == :greet
    assert conf("hey??!") >= 0.70
  end

  test "unknown fallback (no clear cues)" do
    i = intent("hmm")
    # allow greet if you later expand greetings
    assert i in [:unknown, :greet]
  end

  test "command: please build the project" do
    assert intent("please build the project") == :command
    assert conf("please build the project") >= 0.80
  end

  test "command: send me the logs" do
    assert intent("send me the logs") == :command
    assert conf("send me the logs") >= 0.80
  end

  test "feedback: thanks for the help" do
    assert intent("thanks for the help") == :feedback
    assert conf("thanks for the help") >= 0.80
  end

  test "feedback: this is not working" do
    assert intent("this is not working") == :feedback
    assert conf("this is not working") >= 0.70
  end


  test "greet" do
    si = %{sentence: "hey there", tokens: ["hey", "there"]}
    {si2, res} = Selection.select(si)
    assert si2.intent == :greet
    assert res.intent == :greet
    assert si2.confidence > 0.3
  end

  test "define" do
    si = %{sentence: "what is hippocampus", tokens: ["what", "is", "hippocampus"]}
    {si2, res} = Selection.select(si)
    assert si2.intent == :define
    assert res.keyword == "hippocampus"
    assert si2.confidence > 0.3
  end

  test "translate" do
    si = %{sentence: "how do you say hello in french", tokens: ~w(how do you say hello in french)}
    {si2, res} = Selection.select(si)
    assert si2.intent == :translate
    assert si2.confidence > 0.3
  end

  test "fallback ask_info" do
    si = %{sentence: "why is the sky blue", tokens: ~w(why is the sky blue)}
    {si2, res} = Selection.select(si)
    assert si2.intent == :ask_info
    assert si2.confidence > 0.1
  end

  test "brain introspect" do
    si = %{sentence: "show Brain.Hippocampus state", tokens: ~w(show brain.hippocampus state)}
    {si2, _} = Selection.select(si)
    assert si2.intent == :brain_introspect
  end

  test "none below threshold" do
    si = %{sentence: "hmm", tokens: ["hmm"]}
    {si2, _} = Selection.select(si, threshold: 0.5)
    assert si2.intent == :none
  end
end
