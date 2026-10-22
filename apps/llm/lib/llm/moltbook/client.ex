defmodule Llm.Moltbook.Client do
  @moduledoc """
  Read-only Moltbook API client for the observation milestone.

  Only GET requests are implemented. Enabling this module cannot register an
  agent, post, comment, vote, or otherwise change remote state.
  """

  alias Llm.Moltbook.{Adapter, Context}

  @sorts [:new, :hot, :top, :rising]
  @submolt_pattern ~r/\A[a-zA-Z0-9_-]{1,64}\z/

  @doc "Retrieve a bounded page of Moltbook posts. Returns the API response map."
  def posts(opts \\ [])

  def posts(opts) when is_list(opts) do
    with {:ok, config} <- Adapter.require_observe(),
         {:ok, params} <- post_params(opts, config.max_posts),
         {:ok, body} <- get("/posts", params, config, opts) do
      {:ok, body}
    end
  end

  def posts(_opts), do: {:error, :invalid_options}

  @doc "Read the authenticated agent profile; useful for validating an API key."
  def me(opts \\ [])

  def me(opts) when is_list(opts) do
    with {:ok, config} <- Adapter.require_observe() do
      get("/agents/me", %{}, config, opts)
    end
  end

  def me(_opts), do: {:error, :invalid_options}

  @doc """
  Fetch posts and label their text as untrusted external observations.
  No data is written into Brain, Core, memory, or the LLM conversation.
  """
  def observe(opts \\ []) do
    with {:ok, body} <- posts(opts) do
      Context.from_feed(body)
    end
  end

  defp post_params(opts, max_posts) do
    sort = Keyword.get(opts, :sort, :new)
    limit = Keyword.get(opts, :limit, max_posts)
    cursor = Keyword.get(opts, :cursor)
    submolt = Keyword.get(opts, :submolt)

    cond do
      sort not in @sorts ->
        {:error, :invalid_sort}

      not (is_integer(limit) and limit >= 1 and limit <= max_posts) ->
        {:error, :invalid_limit}

      not (is_nil(cursor) or
             (is_binary(cursor) and byte_size(cursor) in 1..1024)) ->
        {:error, :invalid_cursor}

      not (is_nil(submolt) or
             (is_binary(submolt) and Regex.match?(@submolt_pattern, submolt))) ->
        {:error, :invalid_submolt}

      true ->
        params = %{sort: Atom.to_string(sort), limit: limit}
        params = if cursor, do: Map.put(params, :cursor, cursor), else: params
        params = if submolt, do: Map.put(params, :submolt, submolt), else: params
        {:ok, params}
    end
  end

  defp get(path, params, config, opts) do
    request_opts = [
      url: config.api_base <> path,
      params: params,
      auth: {:bearer, Adapter.api_key()},
      receive_timeout: config.timeout_ms,
      retry: false,
      redirect: false
    ]

    # A Req.Test plug can replace the network adapter for deterministic tests.
    request_opts =
      case Keyword.fetch(opts, :plug) do
        {:ok, plug} -> Keyword.put(request_opts, :plug, plug)
        :error -> request_opts
      end

    case Req.get(request_opts) do
      {:ok, %Req.Response{status: status, body: body}}
      when status >= 200 and status < 300 and is_map(body) ->
        {:ok, body}

      {:ok, %Req.Response{status: 401}} ->
        {:error, :unauthorized}

      {:ok, %Req.Response{status: 403}} ->
        {:error, :forbidden}

      {:ok, %Req.Response{status: 429}} ->
        {:error, :rate_limited}

      {:ok, %Req.Response{status: status}} when status >= 300 ->
        {:error, {:http_status, status}}

      {:ok, _response} ->
        {:error, :invalid_api_response}

      {:error, _reason} ->
        # Do not expose request details or Authorization headers.
        {:error, :transport_error}
    end
  end
end
