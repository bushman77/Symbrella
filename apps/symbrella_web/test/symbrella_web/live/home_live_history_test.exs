defmodule SymbrellaWeb.HomeLiveHistoryTest do
  use SymbrellaWeb.ConnCase, async: false

  import Phoenix.LiveViewTest

  alias SymbrellaWeb.ChatHistory

  setup do
    ChatHistory.clear()
    :ok
  end

  test "renders cached chat history on mount", %{conn: conn} do
    ChatHistory.append([
      %{id: "u-cached", role: :user, text: "cached user message"},
      %{id: "b-cached", role: :assistant, text: "cached assistant reply"}
    ])

    {:ok, _view, html} = live(conn, ~p"/")

    assert html =~ "cached user message"
    assert html =~ "cached assistant reply"
    refute html =~ "Welcome to Symbrella chat"
  end
end
