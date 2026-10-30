defmodule Llm.Colony.PublicBrowseTest do
  use ExUnit.Case, async: false

  alias Llm.Colony.Client

  @post_id "12345678-1234-4234-8234-123456789abc"

  setup do
    previous_key = System.get_env("COLONY_API_KEY")
    previous_config = Application.get_env(:llm, :colony)

    # An explicitly empty override ensures the test never reads a developer's
    # private credential and never makes an authenticated HTTP request.
    System.put_env("COLONY_API_KEY", "")
    Application.put_env(:llm, :colony, enabled?: true, mode: :observe, max_posts: 25)

    on_exit(fn ->
      if is_nil(previous_key),
        do: System.delete_env("COLONY_API_KEY"),
        else: System.put_env("COLONY_API_KEY", previous_key)

      if is_nil(previous_config),
        do: Application.delete_env(:llm, :colony),
        else: Application.put_env(:llm, :colony, previous_config)
    end)

    :ok
  end

  defp json_response(conn, payload) do
    conn
    |> Plug.Conn.put_resp_content_type("application/json")
    |> Plug.Conn.resp(200, Jason.encode!(payload))
  end

  test "lists public topics anonymously when no API key exists" do
    plug = fn conn ->
      assert conn.method == "GET"
      assert conn.request_path == "/posts"
      assert Plug.Conn.get_req_header(conn, "authorization") == []

      json_response(conn, %{"posts" => [%{"id" => @post_id, "title" => "Public topic"}]})
    end

    assert {:ok, %{"posts" => [%{"title" => "Public topic"}]}} =
             Client.posts(limit: 25, offset: 0, sort: :newest, plug: plug)
  end

  test "reads a public discussion anonymously when no API key exists" do
    plug = fn conn ->
      assert conn.method == "GET"
      assert conn.request_path == "/posts/#{@post_id}/context"
      assert Plug.Conn.get_req_header(conn, "authorization") == []

      json_response(conn, %{
        "post" => %{"id" => @post_id, "title" => "Public topic"},
        "comments" => []
      })
    end

    assert {:ok, %{"post" => %{"id" => @post_id}}} =
             Client.thread_context(@post_id, plug: plug)
  end

  test "private account endpoint still requires credentials" do
    assert {:error, :missing_colony_api_key} = Client.me()
  end

  test "reviewed comment is still disabled in read-only mode" do
    assert {:error, :colony_comments_disabled} =
             Client.reviewed_comment(@post_id, "Do not post")
  end
end
