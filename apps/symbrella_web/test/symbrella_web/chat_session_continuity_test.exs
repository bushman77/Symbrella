defmodule SymbrellaWeb.ChatSessionContinuityTest do
  use SymbrellaWeb.ConnCase, async: false

  import Phoenix.LiveViewTest

  alias SymbrellaWeb.ChatHistory

  setup do
    ChatHistory.clear()
    :ok
  end

  test "the LiveView reuses the current runtime conversation on remount", %{conn: conn} do
    session_id = ChatHistory.session_id()
    assert is_binary(session_id)

    {:ok, _first_view, _} = live(conn, ~p"/")
    assert ChatHistory.session_id() == session_id

    {:ok, _second_view, _} = live(conn, ~p"/")
    assert ChatHistory.session_id() == session_id
  end

  test "clearing chat starts a new conversation, rather than reusing old context" do
    old_session_id = ChatHistory.session_id()
    ChatHistory.clear()

    refute ChatHistory.session_id() == old_session_id
  end
end
