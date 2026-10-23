defmodule Llm.Colony.TokenCacheTest do
  use ExUnit.Case, async: false

  alias Llm.Colony.TokenCache

  defp jwt(expiry) do
    header = Base.url_encode64(Jason.encode!(%{"alg" => "HS256", "typ" => "JWT"}), padding: false)
    payload = Base.url_encode64(Jason.encode!(%{"exp" => expiry}), padding: false)
    header <> "." <> payload <> ".signature"
  end

  defp unique_key, do: "colony-test-" <> Integer.to_string(System.unique_integer([:positive]))

  test "reuses a JWT until its expiry approaches without touching the exchange" do
    key = unique_key()
    token = jwt(System.system_time(:second) + 86_400)
    {:ok, counter} = Agent.start_link(fn -> 0 end)

    exchange = fn ->
      Agent.update(counter, &(&1 + 1))
      {:ok, token}
    end

    assert {:ok, ^token} = TokenCache.get(key, exchange)
    assert {:ok, ^token} = TokenCache.get(key, exchange)
    assert Agent.get(counter, & &1) == 1
  end

  test "refreshes before expiry instead of reusing a nearly expired JWT" do
    key = unique_key()
    expiring = jwt(System.system_time(:second) + 100)
    fresh = jwt(System.system_time(:second) + 8_000)
    {:ok, counter} = Agent.start_link(fn -> 0 end)

    exchange = fn ->
      n = Agent.get_and_update(counter, fn old -> {old, old + 1} end)
      {:ok, if(n == 0, do: expiring, else: fresh)}
    end

    assert {:ok, ^expiring} = TokenCache.get(key, exchange)
    assert {:ok, ^fresh} = TokenCache.get(key, exchange)
    assert {:ok, ^fresh} = TokenCache.get(key, exchange)
    assert Agent.get(counter, & &1) == 2
  end

  test "never reuses a JWT already expired at exchange time" do
    key = unique_key()
    expired = jwt(System.system_time(:second) - 20)
    fresh = jwt(System.system_time(:second) + 3_600)
    {:ok, counter} = Agent.start_link(fn -> 0 end)

    exchange = fn ->
      n = Agent.get_and_update(counter, fn old -> {old, old + 1} end)
      {:ok, if(n == 0, do: expired, else: fresh)}
    end

    assert {:ok, ^expired} = TokenCache.get(key, exchange)
    assert {:ok, ^fresh} = TokenCache.get(key, exchange)
    assert Agent.get(counter, & &1) == 2
  end

  test "a failed token is invalidated on 401 without evicting newer replacements" do
    key = unique_key()
    first = jwt(System.system_time(:second) + 3_600)
    second = jwt(System.system_time(:second) + 7_200)

    assert {:ok, ^first} = TokenCache.get(key, fn -> {:ok, first} end)
    assert :ok = TokenCache.invalidate(first)
    assert {:ok, ^second} = TokenCache.get(key, fn -> {:ok, second} end)
    assert :ok = TokenCache.invalidate(first)
    assert {:ok, ^second} = TokenCache.get(key, fn -> flunk("must reuse newer JWT") end)
  end

  test "rotating the permanent API key forces a new token without storing either key" do
    old_key = unique_key()
    rotated_key = unique_key()
    first = jwt(System.system_time(:second) + 3_600)
    second = jwt(System.system_time(:second) + 3_700)

    assert {:ok, ^first} = TokenCache.get(old_key, fn -> {:ok, first} end)
    assert {:ok, ^second} = TokenCache.get(rotated_key, fn -> {:ok, second} end)
    assert {:ok, ^second} = TokenCache.get(rotated_key, fn -> flunk("must cache rotated JWT") end)
  end

  test "exchange failures are not cached as tokens and can recover" do
    key = unique_key()
    token = jwt(System.system_time(:second) + 3_600)

    assert {:error, :transport_error} =
             TokenCache.get(key, fn -> {:error, :transport_error} end)

    assert {:ok, ^token} = TokenCache.get(key, fn -> {:ok, token} end)
  end
end
