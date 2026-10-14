defmodule Core.Response.ConversationRecallTest do
  use ExUnit.Case, async: false

  alias Core.Response
  alias Core.Response.LlmChatHistory

  setup do
    {:ok, session_id: "recall-#{System.unique_integer([:positive])}"}
  end

  test "recognizes an exact earlier question beyond the six-turn LLM window", %{
    session_id: session_id
  } do
    LlmChatHistory.record_turn(session_id, "what is 1 plus 1", "The sum is 2.")

    for i <- 1..9 do
      LlmChatHistory.record_turn(
        session_id,
        "distractor question #{i}?",
        "distractor answer #{i}"
      )
    end

    refute Enum.any?(
             LlmChatHistory.messages(session_id, 6),
             &(Map.get(&1, "content") == "what is 1 plus 1")
           )

    {_tone, answer, meta} =
      Response.memory_reply(%{
        text: "have i asked you what 1 plus 1 is?",
        session_id: session_id,
        intent: :question
      })

    assert answer == "Yes. Earlier you asked: “what is 1 plus 1”."
    assert meta.action == :conversation_recall
    assert meta.retrieval_found? == true
    assert meta.response_source == :conversation_history
    assert meta.inspected_user_turns == 10
  end

  test "does not invent an earlier question when no matching turn exists", %{
    session_id: session_id
  } do
    LlmChatHistory.record_turn(session_id, "what is 1 plus 11?", "12")

    {_tone, answer, meta} =
      Response.memory_reply(%{
        text: "Have I asked you what 1 plus 1 is?",
        session_id: session_id
      })

    assert answer == "I don't see that question in the last 1 user turns I have recorded."
    assert meta.retrieval_found? == false
  end

  test "recognizes an earlier topic without claiming arbitrary similarity", %{
    session_id: session_id
  } do
    LlmChatHistory.record_turn(
      session_id,
      "what is the capital of ukraine",
      "Kyiv is the capital of Ukraine."
    )

    {_tone, answer, meta} =
      Response.memory_reply(%{
        text: "did i ask you about the capital of ukraine?",
        session_id: session_id
      })

    assert answer == "Yes. Earlier you asked: “what is the capital of ukraine”."
    assert meta.retrieval_found? == true
  end

  test "can quote the last prior user turn", %{session_id: session_id} do
    LlmChatHistory.record_turn(session_id, "where is Maple Ridge BC?", "In British Columbia.")
    LlmChatHistory.record_turn(session_id, "what is 1 plus 1?", "2")

    {_tone, answer, meta} =
      Response.memory_reply(%{
        text: "what was the last question i asked you?",
        session_id: session_id
      })

    assert answer == "The last thing you asked me was: “what is 1 plus 1?”."
    assert meta.action == :conversation_recall
  end

  test "another session cannot see the first session's chat", %{
    session_id: session_id
  } do
    LlmChatHistory.record_turn(session_id, "what is 1 plus 1?", "2")

    {_tone, answer, meta} =
      Response.memory_reply(%{
        text: "have i asked you what 1 plus 1 is?",
        session_id: session_id <> "-other"
      })

    assert answer == "I don't have any earlier questions recorded for this conversation."
    assert meta.retrieval_found? == false
  end

  test "past-turn evidence is bounded independently of prompt history", %{
    session_id: session_id
  } do
    for i <- 1..105 do
      LlmChatHistory.record_turn(session_id, "question #{i}?", "answer #{i}")
    end

    recall = LlmChatHistory.recall_messages(session_id)
    assert length(recall) == 200
    assert hd(recall)["content"] == "question 6?"
    assert List.last(recall)["content"] == "answer 105"
    assert length(LlmChatHistory.messages(session_id, 6)) == 12
  end

  test "ordinary factual questions still go through the semantic pipeline", %{
    session_id: session_id
  } do
    assert Response.memory_reply(%{
             text: "what is the capital of Ukraine?",
             session_id: session_id
           }) == nil
  end
end
