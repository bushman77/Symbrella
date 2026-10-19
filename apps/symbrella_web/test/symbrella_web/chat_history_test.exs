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

  test "keeps browser histories separate and preserves each one on remount" do
    first = "s-" <> String.duplicate("a", 32)
    second = "s-" <> String.duplicate("b", 32)

    ChatHistory.append(first, %{id: "a-1", role: :user, text: "private alpha"})
    ChatHistory.append(second, %{id: "b-1", role: :user, text: "private beta"})

    assert [%{text: "private alpha"}] = ChatHistory.list(first)
    assert [%{text: "private beta"}] = ChatHistory.list(second)

    ChatHistory.clear(first)
    assert ChatHistory.list(first) == []
    assert [%{text: "private beta"}] = ChatHistory.list(second)
  end

  test "caps total remembered browser histories" do
    for i <- 1..129 do
      ChatHistory.append("browser-#{i}", %{id: "msg-#{i}", role: :user, text: "turn #{i}"})
    end

    assert ChatHistory.list("browser-1") == []
    assert [%{text: "turn 2"}] = ChatHistory.list("browser-2")
    assert [%{text: "turn 129"}] = ChatHistory.list("browser-129")
  end

  test "keeps session and semantic snapshots for inspection routes" do
    si = %Core.SemanticInput{sentence: "good afternoon symbrella", intent: :greet}

    ChatHistory.append(%{
      id: "b1",
      role: :assistant,
      text: "Good afternoon.",
      session_id: "s-chat",
      si: si
    })

    assert [%{session_id: "s-chat", si: ^si}] = ChatHistory.list()
  end
end
