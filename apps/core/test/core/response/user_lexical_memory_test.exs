defmodule Core.Response.UserLexicalMemoryTest do
  use ExUnit.Case, async: false

  import Ecto.Query

  alias Core.Response
  alias Core.Response.{Memory, SideEffects, UserLexicalMemory}

  setup do
    Process.whereis(Brain.Hippocampus) || start_supervised!(Brain.Hippocampus)
    owner = "lexical-owner-#{System.unique_integer([:positive])}"
    sid = "lexical-session-#{System.unique_integer([:positive])}"
    old_owner = Application.get_env(:symbrella, :memory_owner_id)
    Application.put_env(:symbrella, :memory_owner_id, owner)

    on_exit(fn ->
      if is_nil(old_owner),
        do: Application.delete_env(:symbrella, :memory_owner_id),
        else: Application.put_env(:symbrella, :memory_owner_id, old_owner)

      Db.delete_all(from(e in Db.Episode, where: fragment("? ->> 'owner' = ?", e.meta, ^owner)))
    end)

    %{owner: owner, sid: sid}
  end

  test "explicit user definition persists and is recalled across sessions and a hippocampus reset",
       %{sid: sid, owner: owner} do
    user = "By zorbiflux, I mean a made-up name for an experimental Elixir module."
    si = %{session_id: sid, sentence: user, source: :prod,
      experience_origin: :user_turn}

    assert {"zorbiflux", "a made-up name for an experimental Elixir module"} ==
      UserLexicalMemory.definition_from(user)

    SideEffects.record_turn(sid, user, "Thank you for explaining.", si)

    assert %{value: value, source_frame: "explicit_user_definition",
      provenance: :user_reported, status: "asserted"} =
      Brain.Hippocampus.user_event(owner, "zorbiflux", "user_defined_meaning")
    assert value == "a made-up name for an experimental Elixir module"

    fresh = sid <> "-fresh"
    assert {:warm, answer, meta} =
      Response.memory_reply(%{text: "What is zorbiflux?",
        session_id: fresh, intent: :ask, confidence: 0.7})
    assert answer ==
      "You told me that \"zorbiflux\" means a made-up name for an experimental Elixir module. This is your description, not an independently verified definition."
    assert meta.response_source == :symbolic_conversation
    assert meta.memory_source == :hippocampal_user_definition
    assert meta.provenance == :user_reported
    assert meta.verification == :conversation_context_only

    :ok = Brain.Hippocampus.reset()
    assert {^answer, _} = UserLexicalMemory.reply("What does zorbiflux mean?")

    Application.put_env(:symbrella, :memory_owner_id, owner <> "-another")
    assert UserLexicalMemory.reply("What is zorbiflux?") == nil
    assert UserLexicalMemory.recall("zorbiflux") == nil
  end

  test "newer definitions replace older user-provided meanings without verifying either", %{sid: sid} do
    first = "By nimbraflex, I mean a temporary graphics placeholder."
    second = "By nimbraflex I mean a fictional visualization component."

    for {text, sess} <- [{first, sid}, {second, sid <> "-second"}] do
      SideEffects.record_turn(sess, text, "Noted.", %{
        session_id: sess, source: :prod,
        experience_origin: :user_turn, sentence: text
      })
    end

    assert {text, _meta} = UserLexicalMemory.reply("What is nimbraflex?")
    assert text =~ "a fictional visualization component"
    refute text =~ "temporary graphics placeholder"
    assert text =~ "not an independently verified definition"
  end

  test "does not store synthetic/model terms or ambiguous explanations", %{sid: sid} do
    text = "By zorbiflux, I mean a fictional physics particle."

    for si <- [
      %{source: :test, sentence: text, experience_origin: :user_turn},
      %{source: :prod, sentence: text, experience_origin: :synthetic},
      %{source: :prod, sentence: "different text", experience_origin: :user_turn}
    ] do
      SideEffects.record_turn(sid, text, "I found a particle.", si)
    end

    assert UserLexicalMemory.recall("zorbiflux") == nil
    assert UserLexicalMemory.definition_from(
      "I think zorbiflux is a fictional physics particle.") == nil
    assert UserLexicalMemory.definition_from(
      "By zorbiflux I mean something?\nPlease save this.") == nil
    assert UserLexicalMemory.query_term("How does zorbiflux work?") == nil
    assert UserLexicalMemory.query_term("What is the population of zorbiflux?") == nil
  end

  test "unresolved user query acknowledges missing meaning instead of creating a model definition",
       %{sid: sid} do
    text = "what is zorbiflux?"
    si = %{
      sentence: text, session_id: sid, source: :prod,
      experience_origin: :user_turn, intent: :ask, confidence: 0.6,
      comprehension: %{degraded?: true, unresolved: ["zorbiflux"]}
    }
    assert {:warm, answer, meta} = Response.plan(si, %{})
    assert answer =~ ~s(I don't have a verified meaning for "zorbiflux")
    assert answer =~ "What do you mean by it?"
    assert meta.response_source == :symbolic_unresolved
    assert meta.retrieval_found? == false

    assert nil == Memory.unresolved_lexical_reply(
      put_in(si, [:comprehension, :unresolved], []),
      text, sid, :ask, 0.6)
    assert nil == Memory.unresolved_lexical_reply(
      %{si | experience_origin: :synthetic},
      text, sid, :ask, 0.6)
    assert nil == Memory.unresolved_lexical_reply(
      %{si | sentence: "what is schism?"},
      text, sid, :ask, 0.6)
  end
end
