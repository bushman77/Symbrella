defmodule Llm.Colony.Client do
  @moduledoc """
  Colony API client with read-by-default behavior and separately gated comments.

  POST /auth/token exchanges the locally stored API key for a short-lived
  JWT. Account/content reads use GET. Only reviewed_comment/3 can send a
  content POST, and only after two explicit local configuration gates.
  No JWT is written to disk; failed GETs may reauthenticate and retry once.
  Comment POSTs are never retried to avoid duplicate public replies.
  """

  alias Llm.Colony.{Adapter, Context}

  @sorts [:newest, :top, :hot, :discussed]
  @colony_pattern ~r/\A[a-zA-Z0-9_-]{1,100}\z/

  @doc "Read the authenticated Symbrella agent profile."
  def me(opts \\ [])

  def me(opts) when is_list(opts) do
    with {:ok, config} <- Adapter.require_observe(),
         :ok <- validate_opts(opts) do
      authorized_get("/users/me", %{}, config, opts)
    end
  end

  def me(_), do: {:error, :invalid_options}

  @doc "Retrieve a bounded page of public posts, optionally filtered by colony."
  def posts(opts \\ [])

  def posts(opts) when is_list(opts) do
    with {:ok, config} <- Adapter.require_observe(),
         {:ok, params} <- posts_params(opts, config.max_posts) do
      authorized_get("/posts", params, config, opts)
    end
  end

  def posts(_), do: {:error, :invalid_options}

  @doc "Retrieve the agent's personalised feed; restrict to posts, not replies."
  def feed(opts \\ [])

  def feed(opts) when is_list(opts) do
    with {:ok, config} <- Adapter.require_observe(),
         {:ok, params} <- feed_params(opts, config.max_posts) do
      authorized_get("/feed/for-you", params, config, opts)
    end
  end

  def feed(_), do: {:error, :invalid_options}

  @doc """
  Read and normalize social content as untrusted external observations.

  No content is sent to LIFG, Hippocampus, the LLM, or a database yet.
  The cognitive routing bridge is a separate milestone.
  """
  def observe(opts \\ []) do
    with {:ok, config} <- Adapter.require_observe(),
         {:ok, result} <- feed(opts) do
      Context.from_feed(result, max_posts: config.max_posts)
    end
  end

  @post_id ~r/\A[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}\z/

  @doc "Fetch the full discussion context (post plus existing comments)."
  def thread_context(post_id, opts \\ [])

  def thread_context(post_id, opts) when is_binary(post_id) and is_list(opts) do
    with {:ok, config} <- Adapter.require_observe(),
         :ok <- validate_opts(opts),
         :ok <- valid_post_id(post_id) do
      authorized_get("/posts/" <> post_id <> "/context", %{}, config, opts)
    end
  end

  def thread_context(_, _), do: {:error, :invalid_post_id}

  @doc """
  Post one comment. This method is unusable in the default :observe mode,
  and requires the separately opted-in :reviewed_comments permission.
  Callers are responsible for an independently reviewed draft.
  """
  def reviewed_comment(post_id, body, opts \\ [])

  def reviewed_comment(post_id, body, opts)
      when is_binary(post_id) and is_binary(body) and is_list(opts) do
    with {:ok, config} <- Adapter.require_reviewed_comments(),
         :ok <- validate_opts(opts),
         :ok <- valid_post_id(post_id),
         :ok <- valid_comment(body) do
      # Writes are deliberately never retried, including after 401.
      with {:ok, token} <- exchange_token(config, opts) do
        request =
          request_opts("/posts/" <> post_id <> "/comments", config, opts)
          |> Keyword.put(:auth, {:bearer, token})
          |> Keyword.put(:json, %{body: String.trim(body)})

        case Req.post(request) do
          {:ok, %Req.Response{status: status, body: response}}
          when status in 200..299 and is_map(response) ->
            {:ok, response}

          {:ok, %Req.Response{status: status}} when status in 200..299 ->
            {:ok, %{status: :accepted}}

          {:ok, %Req.Response{status: status}} ->
            response_error(status)

          {:error, _} ->
            {:error, :transport_error}
        end
      end
    end
  end

  def reviewed_comment(_, _, _), do: {:error, :invalid_comment}

  defp valid_post_id(value) do
    if Regex.match?(@post_id, value), do: :ok, else: {:error, :invalid_post_id}
  end

  defp valid_comment(value) do
    size = String.length(String.trim(value))
    if size in 1..1800, do: :ok, else: {:error, :invalid_comment}
  end

  defp posts_params(opts, max) do
    with :ok <- validate_opts(opts) do
      limit = Keyword.get(opts, :limit, max)
      sort = Keyword.get(opts, :sort, :newest)
      offset = Keyword.get(opts, :offset, 0)
      colony = Keyword.get(opts, :colony)

      cond do
        not valid_limit?(limit, max) ->
          {:error, :invalid_limit}

        sort not in @sorts ->
          {:error, :invalid_sort}

        not (is_integer(offset) and offset in 0..100_000) ->
          {:error, :invalid_offset}

        not (is_nil(colony) or
               (is_binary(colony) and Regex.match?(@colony_pattern, colony))) ->
          {:error, :invalid_colony}

        true ->
          params = %{limit: limit, sort: Atom.to_string(sort), offset: offset}
          {:ok, if(colony, do: Map.put(params, :colony, colony), else: params)}
      end
    end
  end

  defp feed_params(opts, max) do
    with :ok <- validate_opts(opts) do
      limit = Keyword.get(opts, :limit, max)
      offset = Keyword.get(opts, :offset, 0)
      cursor = Keyword.get(opts, :cursor)

      cond do
        not valid_limit?(limit, max) ->
          {:error, :invalid_limit}

        not (is_integer(offset) and offset >= 0) ->
          {:error, :invalid_offset}

        not (is_nil(cursor) or
               (is_binary(cursor) and byte_size(cursor) in 1..1024)) ->
          {:error, :invalid_cursor}

        true ->
          params = %{limit: limit, kinds: "posts"}

          # The cursor encodes a frozen snapshot; offset is not sent with it.
          params =
            if cursor, do: Map.put(params, :cursor, cursor), else: Map.put(params, :offset, offset)

          {:ok, params}
      end
    end
  end

  defp validate_opts(opts) do
    if Keyword.keyword?(opts), do: :ok, else: {:error, :invalid_options}
  end

  defp valid_limit?(value, max), do: is_integer(value) and value >= 1 and value <= max

  defp authorized_get(path, params, config, opts) do
    with {:ok, token} <- exchange_token(config, opts) do
      case do_get(path, params, token, config, opts) do
        {:error, :unauthorized} ->
          with {:ok, new_token} <- exchange_token(config, opts) do
            do_get(path, params, new_token, config, opts)
          end

        other ->
          other
      end
    end
  end

  defp exchange_token(config, opts) do
    with {:ok, key} <- Adapter.api_key() do
      request_opts =
        request_opts("/auth/token", config, opts)
        |> Keyword.put(:json, %{api_key: key})

      case Req.post(request_opts) do
        {:ok, %Req.Response{status: status, body: %{"access_token" => token}}}
        when status >= 200 and status < 300 and is_binary(token) and byte_size(token) > 0 ->
          {:ok, token}

        {:ok, %Req.Response{status: status}} ->
          response_error(status)

        {:error, _} ->
          {:error, :transport_error}
      end
    end
  end

  defp do_get(path, params, token, config, opts) do
    request_opts =
      request_opts(path, config, opts)
      |> Keyword.put(:params, params)
      |> Keyword.put(:auth, {:bearer, token})

    case Req.get(request_opts) do
      {:ok, %Req.Response{status: status, body: body}}
      when status >= 200 and status < 300 and is_map(body) ->
        {:ok, body}

      {:ok, %Req.Response{status: status}} ->
        response_error(status)

      {:error, _} ->
        # Never return raw Req errors containing Authorization or request bodies.
        {:error, :transport_error}
    end
  end

  defp request_opts(path, config, opts) do
    request = [
      url: config.api_base <> path,
      receive_timeout: config.timeout_ms,
      retry: false,
      redirect: false
    ]

    # Req.Test only: allow in-process, deterministic HTTP tests.
    case Keyword.fetch(opts, :plug) do
      {:ok, plug} -> Keyword.put(request, :plug, plug)
      :error -> request
    end
  end

  defp response_error(401), do: {:error, :unauthorized}
  defp response_error(403), do: {:error, :forbidden}
  defp response_error(429), do: {:error, :rate_limited}
  defp response_error(status) when status >= 300, do: {:error, {:http_status, status}}
  defp response_error(_), do: {:error, :invalid_api_response}
end
