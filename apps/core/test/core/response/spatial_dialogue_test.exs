defmodule Core.Response.SpatialDialogueTest do
  use ExUnit.Case, async: false

  alias Core.Response
  alias Core.Response.LlmChatHistory
  alias Core.Response.SpatialDialogue

  setup do
    sid = "spatial-dialogue-#{System.unique_integer([:positive])}"
    owner = "spatial-owner-#{System.unique_integer([:positive])}"
    previous_owner = Application.get_env(:symbrella, :memory_owner_id)
    Application.put_env(:symbrella, :memory_owner_id, owner)

    on_exit(fn ->
      if is_nil(previous_owner),
        do: Application.delete_env(:symbrella, :memory_owner_id),
        else: Application.put_env(:symbrella, :memory_owner_id, previous_owner)

      import Ecto.Query
      Db.delete_all(from(e in Db.Episode, where: fragment("? ->> 'owner' = ?", e.meta, ^owner)))
    end)

    %{sid: sid, owner: owner}
  end

  test "spatial events persist through Hippocampus and are recallable in a fresh session", %{sid: sid, owner: owner} do
    Process.whereis(Brain.Hippocampus) || start_supervised!(Brain.Hippocampus)

    Core.Response.SideEffects.record_turn(sid,
      "I have a copper lantern sitting on my desk.", "Okay.")
    Core.Response.SideEffects.record_turn(sid,
      "I moved it into a wooden box.", "Understood.")
    Core.Response.SideEffects.record_turn(sid,
      "Actually, I took the lantern out of the box and placed it on a shelf.", "Noted.")

    assert %{location: "a shelf", provenance: :user_reported} =
             Brain.Hippocampus.user_observation(owner, "lantern")

    assert {"According to what you told me, the lantern is on a shelf.", meta} =
             SpatialDialogue.reply("Where is the lantern now?", sid <> "-new-session")

    assert meta.memory_source == :hippocampal_user_observation

    # Persistence survives loss of the volatile hippocampal window.
    :ok = Brain.Hippocampus.reset()
    assert {"According to what you told me, the lantern is on a shelf.", _} =
             SpatialDialogue.reply("Where is the lantern now?", sid <> "-after-reset")

    # Owner isolation, not a global scan of everyone else's chat.
    Application.put_env(:symbrella, :memory_owner_id, owner <> "-other")
    assert SpatialDialogue.reply("Where is the lantern now?", sid <> "-other-owner") == nil
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
