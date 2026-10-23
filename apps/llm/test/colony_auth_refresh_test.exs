defmodule Llm.Colony.AuthRefreshTest do
  use ExUnit.Case, async: false

  alias Llm.Colony.{Adapter, Client}

  setup do
    old_config = Application.get_env(:llm, :colony)
    old_key = System.get_env("COLONY_API_KEY")

    # Test fixture key, never a real account credential.
    System.put_env("COLONY_API_KEY", "col_fixture_local_only")

    Application.put_env(:llm, :colony,
      enabled?: true,
      mode: :observe,
      allow_reviewed_comments?: false,
      api_base: "https://thecolony.ai/api/v1",
      max_posts: 25,
      timeout_ms: 5_000
    )

    on_exit(fn ->
      if is_nil(old_config),
        do: Application.delete_env(:llm, :colony),
        else: Application.put_env(:llm, :colony, old_config)

      if is_nil(old_key),
        do: System.delete_env("COLONY_API_KEY"),
        else: System.put_env("COLONY_API_KEY", old_key)
    end)

    :ok
  end

  test "authenticated GET transparently renews its JWT once after 401" do
    {:ok, events} = Agent.start_link(fn -> [] end)

    plug = fn conn ->
      Agent.update(events, &[{conn.method, conn.request_path} | &1])

      case {conn.method, conn.request_path} do
        {"POST", "/api/v1/auth/token"} ->
          count = Agent.get(events, fn entries ->
            Enum.count(entries, &(&1 == {"POST", "/api/v1/auth/token"}))
          end)

          json(conn, 200, %{"access_token" => if(count == 1, do: "expired", else: "fresh")})

        {"GET", "/api/v1/posts"} ->
          case Plug.Conn.get_req_header(conn, "authorization") do
            ["Bearer expired"] -> json(conn, 401, %{"error" => "expired"})
            ["Bearer fresh"] -> json(conn, 200, %{"posts" => [], "has_more" => false})
            _ -> json(conn, 403, %{"error" => "unexpected token"})
          end

        _ -> json(conn, 404, %{})
      end
    end

    assert {:ok, %{"posts" => []}} = Client.posts(limit: 25, plug: plug)

    assert Enum.reverse(Agent.get(events, & &1)) == [
             {"POST", "/api/v1/auth/token"},
             {"GET", "/api/v1/posts"},
             {"POST", "/api/v1/auth/token"},
             {"GET", "/api/v1/posts"}
           ]
  end

  test "a comment POST is still forbidden by default" do
    {:ok, config} = Adapter.config()
    assert config.mode == :observe
    refute config.allow_reviewed_comments?

    assert {:error, :colony_comments_disabled} =
             Client.reviewed_comment(
               "00000000-0000-4000-8000-000000000001", "A test comment."
             )
  end

  test "an explicitly allowed comment POST is not retried on 401" do
    Application.put_env(:llm, :colony,
      enabled?: true,
      mode: :reviewed_comments,
      allow_reviewed_comments?: true,
      api_base: "https://thecolony.ai/api/v1",
      max_posts: 25,
      timeout_ms: 5_000
    )

    {:ok, events} = Agent.start_link(fn -> [] end)

    plug = fn conn ->
      Agent.update(events, &[{conn.method, conn.request_path} | &1])

      case {conn.method, conn.request_path} do
        {"POST", "/api/v1/auth/token"} ->
          json(conn, 200, %{"access_token" => "expired"})

        {"POST", "/api/v1/posts/00000000-0000-4000-8000-000000000001/comments"} ->
          json(conn, 401, %{"error" => "expired"})

        _ -> json(conn, 404, %{})
      end
    end

    assert {:error, :unauthorized} =
             Client.reviewed_comment(
               "00000000-0000-4000-8000-000000000001",
               "Reviewed fixture comment",
               plug: plug
             )

    assert Enum.reverse(Agent.get(events, & &1)) == [
             {"POST", "/api/v1/auth/token"},
             {"POST", "/api/v1/posts/00000000-0000-4000-8000-000000000001/comments"}
           ]
  end

  defp json(conn, status, body) do
    conn
    |> Plug.Conn.put_resp_content_type("application/json")
    |> Plug.Conn.resp(status, Jason.encode!(body))
    |> Plug.Conn.send_resp()
  end
end
