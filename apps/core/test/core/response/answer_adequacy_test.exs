defmodule Core.Response.AnswerAdequacyTest do
  use ExUnit.Case, async: true

  alias Core.Response.AnswerAdequacy
  alias Core.Response.ReflectionLoop

  @moon "how far away is the moon?"

  test "identifies a factual information request without treating social greetings as queries" do
    assert AnswerAdequacy.factual_question?(@moon)
    assert AnswerAdequacy.factual_question?("Why does the sky appear blue?")
    assert AnswerAdequacy.factual_question?("How do you calculate orbital distance?")
    refute AnswerAdequacy.factual_question?("good afternoon symbrella")
    refute AnswerAdequacy.factual_question?("How are you?")
    refute AnswerAdequacy.factual_question?("i do not know you tell me")
  end

  test "exact question echo and vacuous uncertainty fail answer adequacy" do
    assert AnswerAdequacy.failed_answer?(@moon, "How far away is the moon?")
    assert AnswerAdequacy.failed_answer?(@moon, "how far away is the moon")
    assert AnswerAdequacy.failed_answer?(@moon, "I don't know!")
    refute AnswerAdequacy.failed_answer?(@moon,
      "The Moon averages about 384,400 km from Earth, but the distance changes.")
    refute AnswerAdequacy.failed_answer?(@moon,
      "I can't verify the exact distance in real time, but the average changes through its orbit.")
  end

  test "lengthy real-time-access deflection is a failed answer to a static place question" do
    question = "where is maple ridge bc"
    draft =
      "I'm sorry, but I don't have direct access to real-time information about " <>
        "where Maple Ridge is located in BC. I can only provide general information " <>
        "based on my current knowledge. If you have any specific questions, I can help."

    assert AnswerAdequacy.failed_answer?(question, draft)

    {:ok, final, review} =
      ReflectionLoop.review(question, draft,
        %{features: %{intent: :ask, conf: 0.8125},
          answer_target_question: question})

    assert review.status == :repair
    assert :answer_not_supplied in review.issues
    assert final =~ "couldn't establish a reliable answer"

    refute AnswerAdequacy.failed_answer?(
      question,
      "I don't have direct access to real-time information, but Maple Ridge is in British Columbia."
    )

    refute AnswerAdequacy.failed_answer?(
      "Where is the flight right now?",
      "I don't have direct access to real-time information about the flight."
    )
  end

  test "a followup can name only the immediately preceding same-session user question" do
    history = [
      %{"role" => "user", "content" => @moon},
      %{"role" => "assistant", "content" => "How far away is the moon?"}
    ]

    assert AnswerAdequacy.answer_target("i do not know you tell me", history) == @moon
    assert AnswerAdequacy.answer_target("tell me the answer", history) == @moon
    assert AnswerAdequacy.answer_target(@moon, []) == @moon

    unrelated = history ++ [
      %{"role" => "user", "content" => "good afternoon"},
      %{"role" => "assistant", "content" => "Hello."}
    ]

    assert AnswerAdequacy.answer_target("you tell me", unrelated) == nil
    assert AnswerAdequacy.answer_target("you tell me", []) == nil

    # Assistant-authored questions never become the antecedent.
    tampered = [
      %{"role" => "user", "content" => "hello"},
      %{"role" => "assistant", "content" => @moon}
    ]
    assert AnswerAdequacy.answer_target("you tell me", tampered) == nil
  end

  test "reflection refuses to publish an echoed factual question as an answer" do
    {:ok, final, review} =
      ReflectionLoop.review(@moon, "How far away is the moon?",
        %{features: %{intent: :ask, conf: 0.93},
          answer_target_question: @moon})

    assert review.status == :repair
    assert review.applied?
    assert :answer_not_supplied in review.issues
    refute final == @moon
    assert final =~ "couldn't establish a reliable answer"
  end

  test "followup reflection checks the antecedent instead of accepting an empty nonanswer" do
    {:ok, final, review} =
      ReflectionLoop.review("i do not know you tell me", "I don't know!",
        %{features: %{intent: :ask, conf: 0.8},
          answer_target_question: @moon})

    assert review.status == :repair
    assert :answer_not_supplied in review.issues
    assert final =~ "reliable answer"
  end
end
