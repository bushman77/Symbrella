defmodule Core.Recall.Synonyms.Cache do
  @moduledoc """
  Minimal ETS cache with TTL, created lazily on first use.
  Key: {word :: String.t(), pos :: atom() | nil}
  Value: {inserted_ms :: integer, list :: [entry()]}

  NOTE: This is a convenience cache for Core. If you prefer a supervised cache,
  we can add a proper child under Symbrella.Application later (per Guardrails).
  """

  @table :core_synonyms_cache

  @spec get(term(), non_neg_integer()) :: nil | [map()]
  def get(key, ttl_ms) do
    ensure_table!()

    with [{^key, {t0, list}}] <- :ets.lookup(@table, key),
         true <- not expired?(t0, ttl_ms) do
      list
    else
      _ -> nil
    end
  end

  @spec put(term(), [map()]) :: true
  def put(key, list) when is_list(list) do
    ensure_table!()
    :ets.insert(@table, {key, {System.monotonic_time(:millisecond), list}})
  end

  defp expired?(t0, ttl_ms), do: System.monotonic_time(:millisecond) - t0 > ttl_ms

  defp ensure_table!() do
    case :ets.whereis(@table) do
      :undefined ->
        :ets.new(@table, [:set, :public, :named_table, read_concurrency: true])
        true

      _tid ->
        true
    end
  end
end

