defmodule SymbrellaWeb.ChatSessionContinuityTest do
  use SymbrellaWeb.ConnCase, async: false

  import Phoenix.LiveViewTest

  alias SymbrellaWeb.ChatHistory
  alias SymbrellaWeb.Plugs.ChatSession

  setup do
    ChatHistory.clear()
    :ok
  end

  test "distinct browsers receive distinct signed-cookie session keys", %{conn: conn} do
    first = conn |> Plug.Test.init_test_session(%{}) |> ChatSession.call([])
    second = build_conn() |> Plug.Test.init_test_session(%{}) |> ChatSession.call([])

    first_id = Plug.Conn.get_session(first, "symbrella_chat_session_id")
    second_id = Plug.Conn.get_session(second, "symbrella_chat_session_id")

    assert is_binary(first_id)
    assert String.starts_with?(first_id, "s-")
    assert byte_size(first_id) == 34
    refute first_id == second_id

    # A repeat request carrying the same session retains its conversation key.
    assert Plug.Conn.get_session(ChatSession.call(first, []), "symbrella_chat_session_id") ==
             first_id
  end

  test "remount restores only the browser's own conversation", %{conn: conn} do
    first_id = "s-" <> String.duplicate("a", 32)
    other_id = "s-" <> String.duplicate("b", 32)
    first = Plug.Test.init_test_session(conn, %{"symbrella_chat_session_id" => first_id})
    other = Plug.Test.init_test_session(build_conn(), %{"symbrella_chat_session_id" => other_id})

    ChatHistory.append(first_id, %{id: "first-message", role: :user, text: "first-only-message"})
    ChatHistory.append(other_id, %{id: "other-message", role: :user, text: "second-only-message"})

    {:ok, _first_view, first_html} = live(first, ~p"/")
    {:ok, _remounted_view, remount_html} = live(first, ~p"/")
    {:ok, _other_view, other_html} = live(other, ~p"/")

    assert first_html =~ "first-only-message"
    assert remount_html =~ "first-only-message"
    refute remount_html =~ "second-only-message"

    assert other_html =~ "second-only-message"
    refute other_html =~ "first-only-message"
  end
end
