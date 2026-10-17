defmodule Core.Response.EpisodicMemoryTest do
  use ExUnit.Case, async: false
  import Ecto.Query

  alias Core.Response
  alias Core.Response.{EpisodicMemory, SideEffects}
  alias Core.Semantic.EpisodicEvents

  setup do
    Process.whereis(Brain.Hippocampus) || start_supervised!(Brain.Hippocampus)
    owner = "episode-owner-#{System.unique_integer([:positive])}"
    sid = "episode-#{System.unique_integer([:positive])}"
    old = Application.get_env(:symbrella, :memory_owner_id)
    Application.put_env(:symbrella, :memory_owner_id, owner)

    on_exit(fn ->
      if is_nil(old),
        do: Application.delete_env(:symbrella, :memory_owner_id),
        else: Application.put_env(:symbrella, :memory_owner_id, old)

      Db.delete_all(from(e in Db.Episode, where: fragment("? ->> 'owner' = ?", e.meta, ^owner)))
    end)

    %{sid: sid, owner: owner}
  end

  test "corrects a deadline across sessions and shows latest state", %{sid: sid, owner: owner} do
    SideEffects.record_turn(sid, "The project deadline is Friday.", "Noted.")
    SideEffects.record_turn(sid <> "-second", "Actually, the project deadline is Monday.", "Noted.")

    assert %{value: "Monday", relation: "deadline", provenance: :user_reported} =
             Brain.Hippocampus.user_event(owner, "project", "deadline")

    assert {:warm, answer, meta} =
             Response.memory_reply(%{text: "What is the project deadline?",
               session_id: sid <> "-fresh", intent: :question, confidence: 0.9})

    assert answer == "According to what you told me, the project deadline is Monday."
    assert meta.memory_source == :hippocampal_event
    assert meta.response_source == :symbolic_conversation

    history = Brain.Hippocampus.user_event_history(owner, "project", 5)
    assert Enum.map(history, & &1.value) == ["Monday", "Friday"]
    assert Enum.map(history, & &1.session_id) == [sid <> "-second", sid]
    :ok = Brain.Hippocampus.reset()
    assert {^answer, _} = EpisodicMemory.reply("What is the project deadline?")
  end

  test "relationships and event memories share the same storage contract", %{sid: sid, owner: owner} do
    SideEffects.record_turn(sid, "My friend Rowan lives in Victoria.", "Okay.")
    SideEffects.record_turn(sid, "I met Rowan at the library.", "Okay.")
    SideEffects.record_turn(sid, "Rowan is my friend.", "Okay.")
    SideEffects.record_turn(sid, "We named the robot Aster.", "Okay.")
    SideEffects.record_turn(sid, "The meeting is on Tuesday.", "Okay.")

    assert {"You told me Rowan lives in Victoria.", _} =
             EpisodicMemory.reply("Where does Rowan live?")
    assert {"You told me you met Rowan at the library.", _} =
             EpisodicMemory.reply("Where did I meet Rowan?")
    assert {"You told me Rowan is my friend.", _} =
             EpisodicMemory.reply("Who is Rowan to me?")
    assert {"You told me you named the robot Aster.", _} =
             EpisodicMemory.reply("What did we name the robot?")
    assert {"You told me the meeting is scheduled for Tuesday.", _} =
             EpisodicMemory.reply("When is the meeting?")

    assert %{relation: "met_at", value: "the library"} =
             Brain.Hippocampus.user_event(owner, "rowan", "met_at")

    assert {"From what you've told me about Rowan: " <> summary, _} =
             EpisodicMemory.reply("What do you remember about Rowan?")
    assert summary =~ "met_at: the library"
    assert summary =~ "lives_in: Victoria"
  end

  test "generic semantic producer can store new relations and retractions", %{sid: sid} do
    assert :ok = EpisodicMemory.remember_event(
      %{entity: "Aster", relation: "maintainer", value: "Rowan"}, sid)
    assert {"From what you've told me about Aster: maintainer: Rowan.", _} =
             EpisodicMemory.reply("What do you remember about Aster?")

    assert :ok = EpisodicMemory.remember_event(
      %{entity: "Aster", relation: "maintainer", value: "(withdrawn)", status: :retracted}, sid)

    assert %{status: "retracted"} = EpisodicMemory.recall("Aster", "maintainer")
    assert {"From what you've told me about Aster: maintainer: withdrawn.", _} =
             EpisodicMemory.reply("What do you remember about Aster?")
  end

  test "user reports never become external verification or leak between owners", %{sid: sid, owner: owner} do
    SideEffects.record_turn(sid, "Rowan lives in Victoria.", "Okay.")

    assert EpisodicMemory.reply("Where does Rowan live?") != nil
    assert Db.Episode.latest_user_fact("rowan") == nil

    Application.put_env(:symbrella, :memory_owner_id, owner <> "-other")
    assert EpisodicMemory.reply("Where does Rowan live?") == nil
    assert EpisodicMemory.recall("Rowan", "lives_in") == nil
  end

  test "does not store guesses, hypotheticals, questions or model replies", %{sid: sid} do
    for text <- ["Maybe Rowan lives in Paris.", "If Rowan lives in Paris.",
                 "Where does Rowan live?", "I think Rowan lives in Paris."] do
      assert EpisodicEvents.extract(text) == []
      SideEffects.record_turn(sid, text, "Rowan lives in Paris.")
    end

    assert EpisodicMemory.reply("Where does Rowan live?") == nil
  end
end
