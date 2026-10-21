defmodule Core.Response.LlmChatHistoryOwnerTest do
  use ExUnit.Case, async: true

  alias Core.Response.LlmChatHistoryOwner
  alias Core.Response.LlmChatHistory

  test "targeted clarification replaces only the exact newest pair in both histories" do
    sid = "clarification-history-#{System.unique_integer([:positive])}"
    other = "unrelated-history-#{System.unique_integer([:positive])}"
    prompt = "I've been working on a glimmerflux module."
    original = "Congratulations! What have you been learning about in your module?"
    revised = "I couldn't determine what you meant by glimmerflux. Could you explain that term?"

    assert :ok = LlmChatHistory.record_turn(sid, "Hello", "Hi there.")
    assert :ok = LlmChatHistory.record_turn(sid, prompt, original)
    assert :ok = LlmChatHistory.record_turn(other, prompt, original)

    assert :stale = LlmChatHistory.replace_last_assistant(
      sid, "Unrelated earlier message", original, revised)
    assert :stale = LlmChatHistory.replace_last_assistant(
      sid, prompt, "Other original", revised)

    assert :ok = LlmChatHistory.replace_last_assistant(sid, prompt, original, revised)

    assert List.last(LlmChatHistory.messages(sid)) ==
      %{"role" => "assistant", "content" => revised}
    assert List.last(LlmChatHistory.recall_messages(sid)) ==
      %{"role" => "assistant", "content" => revised}
    assert List.last(LlmChatHistory.messages(other)) ==
      %{"role" => "assistant", "content" => original}
    assert length(LlmChatHistory.messages(sid)) == 4

    # Repeating the action cannot corrupt or modify the same turn twice.
    assert :stale = LlmChatHistory.replace_last_assistant(sid, prompt, original, revised)
  end

  test "chat history ETS owner is supervised and survives its caller" do
    owner = Process.whereis(LlmChatHistoryOwner)

    assert is_pid(owner)
    assert Process.alive?(owner)

    assert Enum.any?(Supervisor.which_children(Symbrella.Supervisor), fn
             {LlmChatHistoryOwner, ^owner, :worker, _modules} -> true
             _ -> false
           end)

    # A short-lived request process must not own or terminate the ETS table.
    request = Task.async(fn -> LlmChatHistoryOwner.ensure_table!() end)
    assert :ok = Task.await(request)

    assert Process.whereis(LlmChatHistoryOwner) == owner
    assert Process.alive?(owner)
    assert :ets.whereis(:core_llm_chat_history) != :undefined
    assert :ets.info(:core_llm_chat_history, :owner) == owner
  end
end
