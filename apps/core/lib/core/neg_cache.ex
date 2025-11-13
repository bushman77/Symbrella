# apps/core/lib/core/neg_cache.ex
defmodule Core.NegCache do
  @moduledoc """
  Minimal DETS-backed negative cache for phrases that fully miss
  (active → DB → remote).

  • Key: Core.Text.normalize(phrase)
  • Value: expiry timestamp (unix ms)
  • TTL: seconds (default 6h). Expired entries are removed on read or purge.

  Public API:
    - exists?(phrase)             :: boolean
    - put(phrase)                 :: :ok
    - purge()                     :: non_neg_integer
    - pop_expired_batch(limit \\ 32) :: [binary()]   # NEW: for Curiosity
    - stats()                     :: map()           # NEW: quick snapshot
  """

  use GenServer

  # ttl in seconds
  @type opts :: [dets_path: String.t(), ttl: pos_integer()]

  # ——— Public API ———

  def start_link(opts),
    do: GenServer.start_link(__MODULE__, opts, name: __MODULE__)

  def child_spec(opts) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [opts]},
      type: :worker,
      restart: :permanent,
      shutdown: 5_000
    }
  end

  @doc "Returns true if the phrase is cached as a miss (and not expired)."
  def exists?(phrase),
    do: GenServer.call(__MODULE__, {:exists?, phrase})

  @doc "Insert phrase with default TTL."
  def put(phrase),
    do: GenServer.cast(__MODULE__, {:put, phrase})

  @doc "Sweep expired entries now (ETS cache only); returns number removed."
  def purge,
    do: GenServer.call(__MODULE__, :purge)

  @doc """
  NEW: Pop up to `limit` expired phrases (exp <= now) *and remove them* from DETS and ETS.
  Returns a list of normalized phrases. Intended for Curiosity re-probe cycles.
  """
  @spec pop_expired_batch(pos_integer()) :: [binary()]
  def pop_expired_batch(limit \\ 32) when is_integer(limit) and limit > 0,
    do: GenServer.call(__MODULE__, {:pop_expired_batch, limit})

  @doc "NEW: Return a small stats snapshot."
  @spec stats() :: %{
          dets_size: non_neg_integer(),
          ets_size: non_neg_integer(),
          ttl_ms: non_neg_integer()
        }
  def stats, do: GenServer.call(__MODULE__, :stats)

  # ——— GenServer ———

  @impl true
  def init(opts) do
    # default 6h
    ttl_ms =
      (Keyword.get(opts, :ttl, 21_600) |> max(1)) * 1_000

    dets_path =
      opts[:dets_path] || Application.app_dir(:core, "priv/negcache/negcache.dets")

    File.mkdir_p!(Path.dirname(dets_path))

    {:ok, dets} =
      :dets.open_file(__MODULE__,
        file: String.to_charlist(dets_path),
        type: :set,
        auto_save: 5_000
      )

    ets = :ets.new(__MODULE__, [:set, :public, read_concurrency: true])

    {:ok, %{dets: dets, ets: ets, ttl_ms: ttl_ms}}
  end

  @impl true
  def terminate(_reason, %{dets: dets}) do
    :dets.close(dets)
    :ok
  end

  @impl true
  def handle_call({:exists?, phrase}, _from, state) do
    norm = normalize(phrase)
    now = now_ms()

    reply =
      case :ets.lookup(state.ets, norm) do
        [{^norm, exp}] ->
          if exp > now do
            true
          else
            :ets.delete(state.ets, norm)
            :dets.delete(state.dets, norm)
            false
          end

        [] ->
          case :dets.lookup(state.dets, norm) do
            [{^norm, exp}] when exp > now ->
              true = :ets.insert(state.ets, {norm, exp})
              true

            [{^norm, _exp}] ->
              :dets.delete(state.dets, norm)
              false

            [] ->
              false
          end
      end

    {:reply, reply, state}
  end

  @impl true
  def handle_call(:purge, _from, state) do
    now = now_ms()

    removed =
      :ets.tab2list(state.ets)
      |> Enum.reduce(0, fn {k, exp}, acc ->
        if exp <= now do
          :ets.delete(state.ets, k)
          :dets.delete(state.dets, k)
          acc + 1
        else
          acc
        end
      end)

    {:reply, removed, state}
  end

  @impl true
  def handle_call({:pop_expired_batch, limit}, _from, %{dets: dets, ets: ets} = state) do
    now = now_ms()

    # Match-spec: select {key, exp} where exp =< now, return key
    ms = [{{:"$1", :"$2"}, [{:"=<", :"$2", now}], [:"$1"]}]

    expired_keys =
      case :dets.select(dets, ms, limit) do
        {keys, _cont} when is_list(keys) -> keys
        :"$end_of_table" -> []
      end

    # Remove from both stores so callers won't see them again unless reinserted.
    Enum.each(expired_keys, fn k ->
      :ets.delete(ets, k)
      :dets.delete(dets, k)
    end)

    {:reply, expired_keys, state}
  end

  @impl true
  def handle_call(:stats, _from, %{dets: dets, ets: ets, ttl_ms: ttl} = state) do
    {:reply,
     %{
       dets_size: :dets.info(dets, :size),
       ets_size: :ets.info(ets, :size),
       ttl_ms: ttl
     }, state}
  end

  @impl true
  def handle_cast({:put, phrase}, state) do
    norm = normalize(phrase)
    exp = now_ms() + state.ttl_ms
    true = :ets.insert(state.ets, {norm, exp})
    :ok = :dets.insert(state.dets, {norm, exp})
    {:noreply, state}
  end

  @doc """
  Remove tokens whose phrase is neg-cached (and not expired).

  By default it prunes only multi-word tokens (n >= 2) so you always
  keep a unigram backbone. Override with `min_n:` if needed.
  """
  @spec prune(Core.SemanticInput.t(), keyword()) :: Core.SemanticInput.t()
  def prune(%Core.SemanticInput{} = si, opts \\ []) do
    min_n = Keyword.get(opts, :min_n, 2)

    pruned =
      Enum.reject(si.tokens, fn t ->
        n = Map.get(t, :n) || Regex.scan(~r/\S+/u, t.phrase) |> length()
        n >= min_n and exists?(t.phrase)
      end)

    %{si | tokens: pruned}
  end

  # ——— Internals ———

  defp normalize(phrase) when is_binary(phrase), do: Core.Text.normalize(phrase)
  defp normalize(_), do: ""

  defp now_ms, do: System.system_time(:millisecond)
end
