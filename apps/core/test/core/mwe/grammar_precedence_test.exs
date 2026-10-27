defmodule Core.MWE.GrammarPrecedenceTest do
  use ExUnit.Case, async: true

  alias Core.MWE.GrammarPrecedence

  test "keeps what is it compositional when a verb continues the question" do
    for sentence <- ["What is it sitting on?", "What is it used for?"] do
      assert GrammarPrecedence.incompatible_question_mwe?(
               %{sentence: sentence},
               "what is it",
               ["interjection"]
             )
    end
  end

  test "preserves a standalone what is it phrase" do
    refute GrammarPrecedence.incompatible_question_mwe?(
             %{sentence: "What is it?"},
             "what is it",
             ["interjection"]
           )
  end
end
