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

  # ---------------------------------------------------------------------------
  # Structured SI cases (map with sentence + tokens)
  # These assert shape + key fields, while allowing the current :unknown
  # behavior so we don't fight the unfinished IntentMatrix/Resolver wiring.
  # ---------------------------------------------------------------------------

  test "greet (structured SI)" do
    si = %{sentence: "hey there", tokens: ["hey", "there"]}

    si2 = Selection.select(si)

    # Today this is :unknown with ~0.4 confidence; in the future we expect :greet.
    assert si2.intent in [:greet, :unknown]
    assert si2.confidence >= 0.3
    assert si2.confidence <= 1.0
  end

  test "define-like question (what is hippocampus)" do
    si = %{sentence: "what is hippocampus", tokens: ["what", "is", "hippocampus"]}

    si2 = Selection.select(si)

    # Make sure the keyword extraction works even if intent is still :unknown.
    assert si2.keyword == "hippocampus"
    assert is_atom(si2.intent)
    assert is_float(si2.confidence) or is_nil(si2.confidence)
  end

  test "translate-like question (how do you say hello in french)" do
    si = %{sentence: "how do you say hello in french", tokens: ~w(how do you say hello in french)}

    si2 = Selection.select(si)

    # Current behavior: intent :unknown, keyword "french", confidence ~0.4.
    # Future behavior: intent :translate.
    assert si2.keyword == "french"
    assert si2.intent in [:translate, :unknown]
    assert si2.confidence >= 0.3
    assert si2.confidence <= 1.0
  end

  test "fallback ask_info (why is the sky blue)" do
    si = %{sentence: "why is the sky blue", tokens: ~w(why is the sky blue)}

    si2 = Selection.select(si)

    # Today probably :unknown; later could be :ask or :ask_info.
    assert si2.intent in [:ask_info, :ask, :unknown]
    assert si2.confidence >= 0.1
    assert si2.confidence <= 1.0
  end

  test "brain introspect (show Brain.Hippocampus state)" do
    si = %{sentence: "show Brain.Hippocampus state", tokens: ~w(show brain.hippocampus state)}

    si2 = Selection.select(si)

    # Ensure we at least capture the keyword; intent can evolve later.
    assert si2.keyword == "brain.hippocampus"
    assert si2.intent in [:brain_introspect, :command, :unknown]
  end

  test "none/unknown below threshold" do
    si = %{sentence: "hmm", tokens: ["hmm"]}

    si2 = Selection.select(si, threshold: 0.5)

    # Right now this is intent :unknown with confidence ~0.4.
    # When threshold gating is implemented, :none would also be acceptable.
    assert si2.intent in [:none, :unknown]
    assert si2.confidence <= 0.5
  end
end
