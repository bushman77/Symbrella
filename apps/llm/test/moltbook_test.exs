defmodule Llm.MoltbookTest do
  use ExUnit.Case, async: false

  alias Llm.Moltbook.{Adapter, Client, Context}

  setup {Req.Test, :set_req_test_to_private}

  setup do
    old_config = Application.get_env(:llm, :moltbook)
    old_key = System.get_env("MOLTBOOK_API_KEY")

    on_exit(fn ->
      if is_nil(old_config) do
        Application.delete_env(:llm, :moltbook)
      else
        Application.put_env(:llm, :moltbook, old_config)
      end

      if is_nil(old_key) do
        System.delete_env("MOLTBOOK_API_KEY")
      else
        System.put_env("MOLTBOOK_API_KEY", old_key)
      end
    end)

    Application.put_env(:llm, :moltbook,
      enabled?: true,
      mode: :observe,
      api_base: "https://www.moltbook.com/api/v1",
      max_posts: 10,
      timeout_ms: 5_000
    )

    System.put_env("MOLTBOOK_API_KEY", "moltbook_test_secret")
    :ok
  end

  test "status describes local readiness without disclosing credentials" do
    assert {:ok, status} = Adapter.status()
    assert status.enabled?
    assert status.mode == :observe
    assert status.credentials_configured?
    assert status.ready?
    refute inspect(status) =~ "moltbook_test_secret"
    refute Map.has_key?(status, :authenticated?)
  end

  test "disabled mode prevents reads, even with a key configured" do
    Application.put_env(:llm, :moltbook, enabled?: false)

    assert {:ok, %{ready?: false}} = Adapter.status()
    assert {:error, :moltbook_disabled} = Client.posts()
    assert {:error, :moltbook_disabled} = Client.me()
  end

  test "missing credentials fails closed" do
    System.delete_env("MOLTBOOK_API_KEY")

    assert {:ok, %{credentials_configured?: false, ready?: false}} = Adapter.status()
    assert {:error, :missing_api_key} = Client.posts()
    assert {:error, :missing_api_key} = Client.me()
  end

  test "unknown mode and non-official API hosts are rejected" do
    Application.put_env(:llm, :moltbook, enabled?: true, mode: :publish)

    assert {:error, {:invalid_moltbook_config, :mode}} = Adapter.config()
    assert {:error, {:invalid_moltbook_config, :mode}} = Client.posts()

    Application.put_env(:llm, :moltbook,
      enabled?: true,
      mode: :observe,
      api_base: "https://other.example/api/v1"
    )

    assert {:error, {:invalid_moltbook_config, :api_base}} = Adapter.config()
    assert {:error, {:invalid_moltbook_config, :api_base}} = Client.posts()
  end

  test "GET posts uses authenticated, bounded parameters" do
    Req.Test.stub(__MODULE__, fn conn ->
      assert conn.method == "GET"
      assert conn.request_path == "/api/v1/posts"

      assert URI.decode_query(conn.query_string) == %{
               "sort" => "new",
               "limit" => "5",
               "submolt" => "general"
             }

      assert Plug.Conn.get_req_header(conn, "authorization") == [
               "Bearer moltbook_test_secret"
             ]

      Req.Test.json(conn, %{
        "success" => true,
        "posts" => [%{"id" => "abc", "title" => "A test", "content" => "Hello"}]
      })
    end)

    assert {:ok, %{"posts" => [%{"id" => "abc"}]}} =
             Client.posts(limit: 5, sort: :new, submolt: "general", plug: {Req.Test, __MODULE__})
  end

  test "limits and query options are validated before making a request" do
    assert {:error, :invalid_limit} = Client.posts(limit: 11)
    assert {:error, :invalid_limit} = Client.posts(limit: 0)
    assert {:error, :invalid_sort} = Client.posts(sort: :invalid)
    assert {:error, :invalid_submolt} = Client.posts(submolt: "../../other")
    assert {:error, :invalid_cursor} = Client.posts(cursor: "")
  end

  test "profile check is an explicit authenticated GET" do
    Req.Test.stub(__MODULE__, fn conn ->
      assert conn.method == "GET"
      assert conn.request_path == "/api/v1/agents/me"
      assert Plug.Conn.get_req_header(conn, "authorization") == ["Bearer moltbook_test_secret"]
      Req.Test.json(conn, %{"agent" => %{"name" => "Symbrella"}})
    end)

    assert {:ok, %{"agent" => %{"name" => "Symbrella"}}} =
             Client.me(plug: {Req.Test, __MODULE__})
  end

  test "HTTP authorization and rate limit failures are classified" do
    Req.Test.stub(__MODULE__, fn conn -> Plug.Conn.send_resp(conn, 401, "unauthorized") end)
    assert {:error, :unauthorized} = Client.me(plug: {Req.Test, __MODULE__})

    Req.Test.stub(__MODULE__, fn conn -> Plug.Conn.send_resp(conn, 429, "too many requests") end)
    assert {:error, :rate_limited} = Client.me(plug: {Req.Test, __MODULE__})
  end

  test "redirects are not followed with a bearer credential" do
    Req.Test.stub(__MODULE__, fn conn ->
      conn
      |> Plug.Conn.put_resp_header("location", "https://other.example/collect")
      |> Plug.Conn.send_resp(302, "")
    end)

    assert {:error, {:http_status, 302}} = Client.me(plug: {Req.Test, __MODULE__})
  end

  test "observe returns bounded, untrusted text without memory writes" do
    Req.Test.stub(__MODULE__, fn conn ->
      Req.Test.json(conn, %{
        "posts" => [
          %{
            "id" => "post-1",
            "title" => "A discussion",
            "content" => String.duplicate("x", 2_500),
            "author" => %{"name" => "other-agent"},
            "submolt" => %{"name" => "general"}
          }
        ]
      })
    end)

    assert {:ok, observation} = Client.observe(plug: {Req.Test, __MODULE__})
    assert observation.source == :moltbook
    assert observation.trust == :untrusted_external
    assert [post] = observation.posts
    assert post.id == "post-1"
    assert post.author == "other-agent"
    assert post.submolt == "general"
    assert String.length(post.content) == 2_000
    assert post.trust == :untrusted_external
  end

  test "invalid or rejected API feed is never treated as an observation" do
    assert {:error, :invalid_feed} = Context.from_feed(%{"posts" => "invalid"})
    assert {:error, :api_rejected} = Context.from_feed(%{"success" => false, "posts" => []})
  end
end
