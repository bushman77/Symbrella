defmodule Core.Intent.SelectionTest do
  use ExUnit.Case, async: true
  alias Core.Intent.Selection

  defp intent(s), do: Selection.select(%{sentence: s}).intent
  defp conf(s),   do: Selection.select(%{sentence: s}).confidence

  test "elongated greeting (hello?! → greet, strong conf)" do
    assert intent("Heeellooo?!") == :greet
    assert conf("Heeellooo?!") >= 0.70
  end

  test "abuse phrase is decisive (fuck you!! → abuse, ~0.98)" do
    assert intent("fuck you!!") == :abuse
    assert conf("fuck you!!")   >= 0.95
  end

  test "insult pattern (you're an idiot → insult)" do
    assert intent("you're an idiot") == :insult
    assert conf("you're an idiot")   >= 0.70
  end

  test "translate intent (translate hola to english)" do
    assert intent("translate hola to english") == :translate
    assert conf("translate hola to english")   >= 0.80
  end

  test "question shape (what time is it? → ask, high conf)" do
    assert intent("what time is it?") == :ask
    assert conf("what time is it?")   >= 0.85
  end

  test "expressive greeting does not leak into ask" do
    assert intent("hey??!") == :greet
    assert conf("hey??!")   >= 0.70
  end

  test "unknown fallback (no clear cues)" do
    i = intent("hmm")
    assert i in [:unknown, :greet]  # allow greet if you later expand greetings
  end
test "command: please build the project" do
  assert intent("please build the project") == :command
  assert conf("please build the project")   >= 0.80
end

test "command: send me the logs" do
  assert intent("send me the logs") == :command
  assert conf("send me the logs")   >= 0.80
end

test "feedback: thanks for the help" do
  assert intent("thanks for the help") == :feedback
  assert conf("thanks for the help")   >= 0.80
end

test "feedback: this is not working" do
  assert intent("this is not working") == :feedback
  assert conf("this is not working")   >= 0.70
end

end

