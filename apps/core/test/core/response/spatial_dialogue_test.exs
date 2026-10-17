defmodule Core.Response.SpatialDialogueTest do
  use ExUnit.Case, async: false

  alias Core.Response
  alias Core.Response.LlmChatHistory
  alias Core.Response.SpatialDialogue

  setup do
    %{sid: "spatial-dialogue-#{System.unique_integer([:positive])}"}
  end

  test "same-turn correction gives the current object location", %{sid: sid} do
    text =
      "I took the copper lantern out of the wooden box and placed it on a shelf. Where is the lantern now?"

    assert {reply, meta} = SpatialDialogue.reply(text, sid)
    assert reply == "According to what you told me, the copper lantern is on a shelf."
    assert meta.memory_source == :same_session_user_statement

    assert {:warm, ^reply, %{response_source: :symbolic_conversation}} =
             Response.memory_reply(%{text: text, session_id: sid, intent: :ask})
  end

  test "new user placement supersedes an older location across turns", %{sid: sid} do
    LlmChatHistory.record_turn(sid, "I have a copper lantern sitting on my desk.",
      "Understood.")
    LlmChatHistory.record_turn(sid, "I moved it into a wooden box.",
      "The lantern is in the box.")
    LlmChatHistory.record_turn(sid,
      "Actually, I took the lantern out of the box and placed it on a shelf.",
      "The lantern is on a shelf.")

    assert {answer, _} = SpatialDialogue.reply("Where is the lantern now?", sid)
    assert answer == "According to what you told me, the lantern is on a shelf."
    refute answer =~ "box"
  end

  test "only the user's assertions establish conversational state", %{sid: sid} do
    LlmChatHistory.record_turn(sid, "What is a lantern?",
      "I placed the copper lantern on a desk.")

    assert SpatialDialogue.reply("Where is the lantern now?", sid) == nil
    assert SpatialDialogue.reply("Where is the lantern now?", sid <> "-other") == nil
    assert SpatialDialogue.reply("Where is Vancouver now?", sid) == nil
  end

  test "rejects unrelated and insufficient placement evidence", %{sid: sid} do
    LlmChatHistory.record_turn(sid, "I moved the book onto a table.", "Okay.")
    assert SpatialDialogue.reply("Where is the lantern now?", sid) == nil
    assert SpatialDialogue.reply("Where is the book now?", sid) ==
             {"According to what you told me, the book is onto a table.",
              %{action: :conversation_recall, source: :user_reported_spatial_event,
                response_source: :symbolic_conversation,
                memory_source: :same_session_user_statement,
                retrieval_found?: true}}
  end
end
