defmodule SymbrellaWeb.WebAuthTest do
  use SymbrellaWeb.ConnCase, async: false

  import Phoenix.LiveViewTest

  alias SymbrellaWeb.Plugs.WebAuth

  @username "symbrella-user"
  @password "long-test-password-123456"

  setup do
    old = Application.get_env(:symbrella_web, :web_auth)

    Application.put_env(:symbrella_web, :web_auth,
      enabled?: true,
      username: @username,
      password: @password
    )

    on_exit(fn ->
      if old == nil do
        Application.delete_env(:symbrella_web, :web_auth)
      else
        Application.put_env(:symbrella_web, :web_auth, old)
      end
    end)

    :ok
  end

  test "denies unauthenticated browser and diagnostic requests", %{conn: conn} do
    for path <- ["/", "/episodes", "/semantic", "/brain", "/brain/report.txt"] do
      response = get(recycle(conn), path)
      assert response.status == 401
      assert response.halted
      assert get_resp_header(response, "www-authenticate") != []
    end
  end

  test "rejects incorrect credentials", %{conn: conn} do
    conn = put_req_header(conn, "authorization", basic_header(@username, "wrong-password"))
    assert get(conn, "/").status == 401
  end

  test "authenticates navigation and issues the signed LiveView session proof", %{conn: conn} do
    authenticated = put_req_header(conn, "authorization", basic_header(@username, @password))
    assert {:ok, _view, _html} = live(authenticated, "/")
  end

  test "the LiveView mount rejects a connection without a signed proof" do
    refute WebAuth.authorized_session?(%{})

    assert {:halt, _socket} =
             WebAuth.on_mount(:require_authenticated, %{}, %{}, %Phoenix.LiveView.Socket{})
  end

  test "session authorization is invalidated when the password changes", %{conn: conn} do
    auth =
      conn
      |> Plug.Test.init_test_session(%{})
      |> put_req_header("authorization", basic_header(@username, @password))
      |> WebAuth.call([])

    cookie_proof = get_session(auth, "symbrella_web_auth")
    assert WebAuth.authorized_session?(%{"symbrella_web_auth" => cookie_proof})

    Application.put_env(:symbrella_web, :web_auth,
      enabled?: true,
      username: @username,
      password: "another-long-test-password"
    )

    refute WebAuth.authorized_session?(%{"symbrella_web_auth" => cookie_proof})
  end

  defp basic_header(user, password), do: "Basic " <> Base.encode64(user <> ":" <> password)
end
