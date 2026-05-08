defmodule SymbrellaWeb.ChatHistoryTest do
  use ExUnit.Case, async: false

  alias SymbrellaWeb.ChatHistory

  setup do
    ChatHistory.clear()
    :ok
  end

  test "keeps only the most recent 100 messages in display order" do
    for i <- 1..105 do
      ChatHistory.append(%{id: "m#{i}", role: :user, text: "message #{i}"})
    end

    messages = ChatHistory.list()

    assert length(messages) == 100
    assert hd(messages).id == "m6"
    assert List.last(messages).id == "m105"
  end

  test "ignores malformed messages" do
    ChatHistory.append(%{id: "", role: :user, text: "missing id"})
    ChatHistory.append(%{id: "m1", role: :bad, text: "bad role"})
    ChatHistory.append(%{id: "m2", role: :assistant, text: "ok"})

    assert [%{id: "m2", role: :assistant, text: "ok"}] = ChatHistory.list()
  end
end
