defmodule Core.Curiosity do
  @moduledoc """
  Periodically re-probes expired NegCache entries:
  - pops a small batch of expired phrases
  - tries Llm.Pos.run/2 to enrich
  - on success: persist fallback senses (if available), do NOT reinsert into NegCache
  - on failure: reinsert with fresh TTL via Core.NegCache.put/1
  """

  use GenServer
  require Logger

  @default_interval_ms 300_000   # 5m
  @default_batch_size  16
  @default_concurrency 2

  @type state :: %{
    interval_ms: pos_integer(),
    batch_size: pos_integer(),
    concurrency: pos_integer(),
    llm_model: binary() | nil
  }

  # -- Public -------------------------------------------------

  def start_link(opts \\ []),
    do: GenServer.start_link(__MODULE__, opts, name: Keyword.get(opts, :name, __MODULE__))

  def child_spec(opts),
    do: %{id: __MODULE__, start: {__MODULE__, :start_link, [opts]}, type: :worker, restart: :permanent, shutdown: 5_000}

  @doc "Kick a cycle immediately."
  def probe_now(server \\ __MODULE__), do: GenServer.cast(server, :probe)

  @doc "Snapshot current settings."
  def snapshot(server \\ __MODULE__), do: GenServer.call(server, :snapshot)

  # -- GenServer ---------------------------------------------

  @impl true
  def init(opts) do
    state = %{
      interval_ms: Keyword.get(opts, :interval_ms, @default_interval_ms),
      batch_size:  Keyword.get(opts, :batch_size,  @default_batch_size),
      concurrency: Keyword.get(opts, :concurrency, @default_concurrency),
      llm_model:   Keyword.get(opts, :llm_model,   nil)
    }

    schedule_tick(state.interval_ms)
    {:ok, state}
  end

  @impl true
  def handle_call(:snapshot, _from, state), do: {:reply, state, state}

  @impl true
  def handle_cast(:probe, state) do
    process_batch(state)
    {:noreply, state}
  end

  @impl true
  def handle_info(:tick, state) do
    process_batch(state)
    schedule_tick(state.interval_ms)
    {:noreply, state}
  end

  # -- Core loop ---------------------------------------------

  defp process_batch(%{batch_size: n, concurrency: c} = state) do
    phrases = Core.NegCache.pop_expired_batch(n)

    if phrases == [] do
      :ok
    else
      phrases
      |> Task.async_stream(&process_phrase(&1, state),
           max_concurrency: c,
           timeout: :timer.seconds(30)
         )
      |> Stream.run()
    end
  end

  defp process_phrase(phrase, state) do
    res =
      Llm.Pos.run(phrase,
        model: state.llm_model,
        keep_alive: "5m",
        timeout: :timer.seconds(20),
        options: %{num_predict: 256, num_ctx: 1024, temperature: 0.0},
        allow_builtin_lexicon: true,
        require_nonempty_syn_ant?: false
      )

    case res do
      {:ok, %{"entries" => entries}} when is_list(entries) and entries != [] ->
        persist(entries)
      _ ->
        # Still unknown → renew TTL to avoid hot-looping
        Core.NegCache.put(phrase)
        :ok
    end
  end

  defp persist(entries) do
    cond do
      Code.ensure_loaded?(Core.Lexicon) and function_exported?(Core.Lexicon, :upsert_fallbacks, 1) ->
        Core.Lexicon.upsert_fallbacks(entries)

      Code.ensure_loaded?(Db.Lexicon) and function_exported?(Db.Lexicon, :bulk_upsert_senses, 1) ->
        Db.Lexicon.bulk_upsert_senses(entries)

      true ->
        Logger.warning("Curiosity: no Lexicon upsert function found — skipping persist")
        :ok
    end
  end

  defp schedule_tick(ms), do: Process.send_after(self(), :tick, ms)
end

