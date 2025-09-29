defmodule Brain.LIFG.Priming do
  @moduledoc ~S"""
  Recency priming cache for LIFG.

  Stores recent winners by `id` and returns a **decayed boost** for scoring.
  - Backed by a named ETS table (default: `:lifg_priming_cache`).
  - Exponential decay using half-life: boost(id) = cap * (1 - 0.5^(age / half_life)) * norm(score)
    (or configure however you like below).
  - Side-effect free by default unless you call `bump/2`.

  Options:
    * :table            - ETS named table atom (default: :lifg_priming_cache)
    * :half_life_sec    - decay half-life in seconds (default: 300.0)
    * :cap              - maximum boost returned (default: 1.0)
    * :now_ms           - override wall clock (ms) for testing

  Table row: {id :: String.t(), score :: float(), updated_ms :: integer()}
  """

  @type opts :: Keyword.t()

  @spec ensure_table(opts) :: :ok
  @spec ensure_table(opts) :: :ok
  def ensure_table(opts \\ []) do
    table = Keyword.get(opts, :table, :lifg_priming_cache)

    case :ets.whereis(table) do
      :undefined ->
        # Two tests might try to create at the same time — treat "already exists" as OK.
        try do
          :ets.new(table, [
            :named_table,
            :set,
            :public,
            read_concurrency: true,
            write_concurrency: true
          ])

          :ok
        rescue
          ArgumentError -> :ok
        end

      _tid ->
        :ok
    end
  end

  @doc "Increase priming for `id` by `delta` (default 1.0). Creates the row if missing."
  @spec bump(String.t(), opts) :: :ok
  def bump(id, opts \\ []) when is_binary(id) do
    ensure_table(opts)
    table = Keyword.get(opts, :table, :lifg_priming_cache)
    now_ms = Keyword.get(opts, :now_ms, System.system_time(:millisecond))
    delta = Keyword.get(opts, :delta, 1.0) * 1.0

    case :ets.lookup(table, id) do
      [{^id, score, _old_ms}] ->
        :ets.insert(table, {id, score + delta, now_ms})

      [] ->
        :ets.insert(table, {id, max(delta, 0.0), now_ms})
    end

    :ok
  end

  @doc "Return a decayed boost in [0, cap] for `id`."
  @spec boost_for(String.t(), opts) :: float()
  def boost_for(id, opts \\ []) when is_binary(id) do
    ensure_table(opts)
    table = Keyword.get(opts, :table, :lifg_priming_cache)
    half = Keyword.get(opts, :half_life_sec, 300.0) * 1.0
    cap = Keyword.get(opts, :cap, 1.0) * 1.0
    now_ms = Keyword.get(opts, :now_ms, System.system_time(:millisecond))

    case :ets.lookup(table, id) do
      [{^id, score, updated_ms}] ->
        age_s = max(0.0, (now_ms - updated_ms) / 1000.0)
        # true exponential decay: immediate boost at age 0, then halves each half-life
        decay = :math.pow(0.5, age_s / max(half, 1.0))
        cap * clamp(score) * decay

      _ ->
        0.0
    end
  end

  @spec clear(opts) :: :ok
  def clear(opts \\ []) do
    table = Keyword.get(opts, :table, :lifg_priming_cache)
    ensure_table(opts)

    # Table might have been deleted/created concurrently in async tests — ignore errors.
    try do
      :ets.delete_all_objects(table)
      :ok
    rescue
      ArgumentError ->
        # If it vanished in the meantime, just recreate it empty.
        ensure_table(opts)
        :ok
    end
  end

  defp clamp(x) when is_number(x) and x < 0.0, do: 0.0
  defp clamp(x) when is_number(x) and x > 1.0, do: 1.0
  defp clamp(x) when is_number(x), do: x * 1.0
end
