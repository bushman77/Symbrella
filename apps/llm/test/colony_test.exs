defmodule Llm.ColonyTest do
  use ExUnit.Case, async: false

  alias Llm.Colony.{Adapter, Client, Context}

  setup {Req.Test, :set_req_test_to_private}

  setup do
    previous_config = Application.get_env(:llm, :colony)
    previous_key = System.get_env("COLONY_API_KEY")

    on_exit(fn ->
      if is_nil(previous_config) do
        Application.delete_env(:llm, :colony)
      else
        Application.put_env(:llm, :colony, previous_config)
      end

      if is_nil(previous_key) do
        System.delete_env("COLONY_API_KEY")
      else
        System.put_env("COLONY_API_KEY", previous_key)
      end
    end)

    Application.put_env(:llm, :colony,
      enabled?: true,
      mode: :observe,
      api_base: "https://thecolony.ai/api/v1",
      max_posts: 10,
      timeout_ms: 5_000
    )

    # A test-only environment key overrides any actual private file.
    System.put_env("COLONY_API_KEY", "col_test_secret")
    :ok
  end

  test "local status is non-secret and does not claim remote authentication" do
    assert {:ok, status} = Adapter.status()
    assert status.ready?
    assert status.credentials_configured?
    assert status.enabled?
    assert status.mode == :observe
    refute inspect(status) =~ "col_test_secret"
    refute Map.has_key?(status, :authenticated?)
  end

  test "disabled mode cannot issue HTTP requests" do
    Application.put_env(:llm, :colony, enabled?: false)

    assert {:ok, %{ready?: false}} = Adapter.status()
    assert {:error, :colony_disabled} = Client.me()
    assert {:error, :colony_disabled} = Client.posts()
    assert {:error, :colony_disabled} = Client.feed()
  end

  test "missing credentials do not fall back to the real private file" do
    System.put_env("COLONY_API_KEY", "")

    assert {:ok, %{ready?: false, credentials_configured?: false}} = Adapter.status()
    assert {:error, :missing_colony_api_key} = Client.me()
  end

  test "invalid mode and alternate API base are rejected" do
    Application.put_env(:llm, :colony, enabled?: true, mode: :publish)
    assert {:error, {:invalid_colony_config, :mode}} = Adapter.config()
    assert {:error, {:invalid_colony_config, :mode}} = Client.posts()

    Application.put_env(:llm, :colony,
      enabled?: true,
      mode: :observe,
      api_base: "https://example.net/api/v1"
    )

    assert {:error, {:invalid_colony_config, :api_base}} = Adapter.config()
    assert {:error, {:invalid_colony_config, :api_base}} = Client.me()
  end

  test "profile uses JWT exchanged from key, never the raw key as bearer" do
    Req.Test.stub(__MODULE__, fn conn ->
      case {conn.method, conn.request_path} do
        {"POST", "/api/v1/auth/token"} ->
          {:ok, request_body, conn} = Plug.Conn.read_body(conn)
          assert Jason.decode!(request_body) == %{"api_key" => "col_test_secret"}
          Req.Test.json(conn, %{"access_token" => "jwt-secret", "token_type" => "bearer"})

        {"GET", "/api/v1/users/me"} ->
          assert Plug.Conn.get_req_header(conn, "authorization") == ["Bearer jwt-secret"]
          Req.Test.json(conn, %{"username" => "symbrella", "display_name" => "Symbrella"})

        unexpected ->
          flunk("Unexpected HTTP request: #{inspect(unexpected)}")
      end
    end)

    assert {:ok, %{"username" => "symbrella"}} =
             Client.me(plug: {Req.Test, __MODULE__})
  end

  test "posts use bounded parameters and the current newest sort" do
    Req.Test.stub(__MODULE__, fn conn ->
      case conn.request_path do
        "/api/v1/auth/token" ->
          Req.Test.json(conn, %{"access_token" => "jwt-secret"})

        "/api/v1/posts" ->
          assert conn.method == "GET"
          assert Plug.Conn.get_req_header(conn, "authorization") == ["Bearer jwt-secret"]

          assert URI.decode_query(conn.query_string) == %{
                   "limit" => "5",
                   "sort" => "newest",
                   "offset" => "2",
                   "colony" => "general"
                 }

          Req.Test.json(conn, %{"items" => [%{"id" => "post-1", "body" => "Hello"}]})

        unexpected ->
          flunk("Unexpected HTTP path: #{unexpected}")
      end
    end)

    assert {:ok, %{"items" => [%{"id" => "post-1"}]}} =
             Client.posts(
               limit: 5,
               offset: 2,
               colony: "general",
               plug: {Req.Test, __MODULE__}
             )
  end

  test "feed uses posts-only mode and bounded limit" do
    Req.Test.stub(__MODULE__, fn conn ->
      case conn.request_path do
        "/api/v1/auth/token" ->
          Req.Test.json(conn, %{"access_token" => "jwt-secret"})

        "/api/v1/feed/for-you" ->
          assert conn.method == "GET"

          assert URI.decode_query(conn.query_string) == %{
                   "limit" => "3",
                   "kinds" => "posts",
                   "offset" => "0"
                 }

          Req.Test.json(conn, %{"items" => [], "next_cursor" => nil})
      end
    end)

    assert {:ok, %{"items" => []}} = Client.feed(limit: 3, plug: {Req.Test, __MODULE__})
  end

  test "bad filters are rejected before any authentication exchange" do
    assert {:error, :invalid_limit} = Client.posts(limit: 51)
    assert {:error, :invalid_limit} = Client.feed(limit: 0)
    assert {:error, :invalid_offset} = Client.posts(offset: -1)
    assert {:error, :invalid_sort} = Client.posts(sort: :rising)
    assert {:error, :invalid_colony} = Client.posts(colony: "../other")
    assert {:error, :invalid_cursor} = Client.feed(cursor: "")
    assert {:error, :invalid_options} = Client.posts([:bad])
  end

  test "expired JWT gets one fresh token exchange and GET retry" do
    exchanges = :atomics.new(1, [])
    Req.Test.stub(__MODULE__, fn conn ->
      case conn.request_path do
        "/api/v1/auth/token" ->
          n = :atomics.add_get(exchanges, 1, 1)
          Req.Test.json(conn, %{"access_token" => "jwt-#{n}"})

        "/api/v1/users/me" ->
          case Plug.Conn.get_req_header(conn, "authorization") do
            ["Bearer jwt-1"] -> Plug.Conn.send_resp(conn, 401, "expired")
            ["Bearer jwt-2"] -> Req.Test.json(conn, %{"username" => "symbrella"})
          end
      end
    end)

    assert {:ok, %{"username" => "symbrella"}} =
             Client.me(plug: {Req.Test, __MODULE__})

    assert :atomics.get(exchanges, 1) == 2
  end

  test "authentication errors, rate limits and redirects are not bypassed" do
    Req.Test.stub(__MODULE__, fn conn ->
      Plug.Conn.send_resp(conn, 401, "invalid key")
    end)
    assert {:error, :unauthorized} = Client.me(plug: {Req.Test, __MODULE__})

    Req.Test.stub(__MODULE__, fn conn ->
      Plug.Conn.send_resp(conn, 429, "rate limited")
    end)
    assert {:error, :rate_limited} = Client.me(plug: {Req.Test, __MODULE__})

    Req.Test.stub(__MODULE__, fn conn ->
      conn
      |> Plug.Conn.put_resp_header("location", "https://example.net/collect")
      |> Plug.Conn.send_resp(302, "")
    end)
    assert {:error, {:http_status, 302}} = Client.me(plug: {Req.Test, __MODULE__})
  end

  test "observations bound post text and preserve an untrusted provenance" do
    Req.Test.stub(__MODULE__, fn conn ->
      case conn.request_path do
        "/api/v1/auth/token" ->
          Req.Test.json(conn, %{"access_token" => "jwt-secret"})

        "/api/v1/feed/for-you" ->
          Req.Test.json(conn, %{
            "items" => [
              %{
                "reason" => "Discussion you might enjoy",
                "post" => %{
                  "id" => "post-1",
                  "title" => "LLM reasoning",
                  "body" => String.duplicate("x", 2_500),
                  "author" => %{"username" => "other-agent"},
                  "colony" => %{"name" => "general"},
                  "post_type" => "discussion",
                  "created_at" => "2026-10-21T00:00:00Z"
                }
              }
            ]
          })
      end
    end)

    assert {:ok, observation} = Client.observe(plug: {Req.Test, __MODULE__})
    assert observation.source == :colony
    assert observation.trust == :untrusted_external
    assert [post] = observation.posts
    assert post.author == "other-agent"
    assert post.colony == "general"
    assert post.id == "post-1"
    assert String.length(post.body) == 2_000
    assert post.trust == :untrusted_external
  end

  test "invalid feeds are rejected and raw input is bounded" do
    assert {:error, :invalid_colony_feed} = Context.from_feed(%{"items" => :invalid})
    assert {:error, :invalid_colony_feed} = Context.from_feed(%{"other" => "shape"})

    assert {:ok, %{posts: posts}} =
             Context.from_feed(
               %{"items" => Enum.map(1..20, &%{"id" => "#{&1}", "body" => "hello"})},
               max_posts: 2
             )

    assert length(posts) == 2
    refute Map.has_key?(hd(posts), :instructions)
  end
end
