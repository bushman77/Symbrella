defmodule SymbrellaWeb.PageControllerTest do
  use SymbrellaWeb.ConnCase, async: true

  test "GET / renders the chat shell", %{conn: conn} do
    conn = get(conn, ~p"/")
    body = html_response(conn, 200)

    # Stable signals from your current HTML:
    assert body =~ "Symbrella · Chat"
    assert body =~ ~s(id="chat-root")
  end

  test "GET / includes built assets", %{conn: conn} do
    conn = get(conn, ~p"/")
    body = html_response(conn, 200)

    # Keep these checks to ensure the static pipeline stays wired
    assert body =~ ~s(rel="stylesheet" href="/assets/app.css")
    assert body =~ ~s(src="/assets/app.js")
    assert body =~ ~s(phx-track-static)
  end
end

