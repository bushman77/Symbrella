defmodule Llm.Colony.TokenCache do
  @moduledoc """
  Process-memory-only Colony JWT cache.

  A long-lived API key remains in the existing private file/environment.
  The 24-hour JWT never touches a file, env var, database or logs.
  A single GenServer serializes on-demand renewal so simultaneous browser
  requests do not each exchange the same permanent key.

  Expiration is based on the JWT's `exp` when present and is capped
  at the documented 24-hour lifespan. We refresh five minutes early, or
  invalidate immediately after an HTTP 401. An opaque test/legacy token
  is conservatively cached for fifteen minutes at most.
  """

  use GenServer

  @refresh_margin 300
  @maximum_lifetime 86_400
  @fallback_lifetime 900

  def start_link(_opts), do: GenServer.start_link(__MODULE__, %{}, name: __MODULE__)

  @doc "Get a cached token, exchanging the permanent key when it has expired."
  def get(cache_key, exchange) when is_binary(cache_key) and is_function(exchange, 0) do
    GenServer.call(__MODULE__, {:get, cache_key, exchange}, 60_000)
  end

  @doc "Invalidate only the JWT that failed, never a newer replacement."
  def invalidate(token) when is_binary(token) do
    GenServer.call(__MODULE__, {:invalidate, token})
  end

  @impl true
  def init(_) do
    {:ok, empty()}
  end

  @impl true
  def handle_call({:get, key, exchange}, _from, state) do
    now = System.system_time(:second)

    if state.cache_key == key and is_binary(state.token) and state.expires_at > now do
      {:reply, {:ok, state.token}, state}
    else
      # Exchange while handling the call, rather than racing concurrent
      # requests to obtain tokens. Errors never retain an expired token.
      case exchange.() do
        {:ok, token} when is_binary(token) and byte_size(token) > 0 ->
          {:reply, {:ok, token},
           %{cache_key: key, token: token, expires_at: deadline(token, now)}}

        {:error, _} = error ->
          {:reply, error, empty()}

        _other ->
          {:reply, {:error, :invalid_api_response}, empty()}
      end
    end
  end

  @impl true
  def handle_call({:invalidate, token}, _from, state) do
    next = if state.token == token, do: empty(), else: state
    {:reply, :ok, next}
  end

  defp empty, do: %{cache_key: nil, token: nil, expires_at: 0}

  defp deadline(token, now) do
    case jwt_exp(token) do
      exp when is_integer(exp) and exp <= now ->
        # Never cache a token whose signed expiry claim is already past.
        now

      exp when is_integer(exp) ->
        min(exp, now + @maximum_lifetime) - @refresh_margin

      _ ->
        now + @fallback_lifetime
    end
  end

  # Decode only the public expiry metadata; this is NOT a claim verification.
  # Colony itself validates the JWT when it receives each API request.
  defp jwt_exp(token) do
    case String.split(token, ".") do
      [_header, payload, _signature] when byte_size(payload) in 1..8_192 ->
        with {:ok, bytes} <- Base.url_decode64(payload, padding: false),
             {:ok, %{"exp" => exp}} <- Jason.decode(bytes),
             true <- is_integer(exp) do
          exp
        else
          _ -> nil
        end

      _ ->
        nil
    end
  end
end
