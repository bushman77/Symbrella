defmodule Core.Response.LlmChatHistoryOwnerTest do
  use ExUnit.Case, async: true

  alias Core.Response.LlmChatHistoryOwner

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
