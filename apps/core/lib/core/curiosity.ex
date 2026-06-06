# apps/core/lib/core/curiosity.ex
defmodule Core.Curiosity do
  @moduledoc """
  Periodically re-probes expired NegCache entries:

  - pops a small batch of expired phrases
  - tries Llm.Pos.run/2 to enrich
  - on success:
      • write episodes to Brain.Hippocampus (if available)
      • optional lexicon upsert (config-gated; off by default)
      • do NOT reinsert into NegCache
  - on failure:
      • reinsert with fresh TTL via Core.NegCache.put/1

  All external writes use dynamic `apply/3` to avoid compile-time coupling.

  NOTE: During Brain migration, the LLM path is fully optional.
  If Llm.Pos is not available, we skip enrichment and renew NegCache TTL.
  """

  use GenServer

  alias Core.Curiosity.Enricher
  alias Core.Curiosity.EnrichmentPolicy
  alias Core.Curiosity.Persistence

  # 5m
  @default_interval_ms 300_000
  @default_batch_size 16
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
    do: %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [opts]},
      type: :worker,
      restart: :permanent,
      shutdown: 5_000
    }

  @doc "Kick a cycle immediately."
  def probe_now(server \\ __MODULE__), do: GenServer.cast(server, :probe)

  @doc "Snapshot current settings."
  def snapshot(server \\ __MODULE__), do: GenServer.call(server, :snapshot)

  # -- GenServer ---------------------------------------------

  @impl true
  def init(opts) do
    state = %{
      interval_ms: Keyword.get(opts, :interval_ms, @default_interval_ms),
      batch_size: Keyword.get(opts, :batch_size, @default_batch_size),
      concurrency: Keyword.get(opts, :concurrency, @default_concurrency),
      llm_model: Keyword.get(opts, :llm_model, nil)
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
    phrase
    |> Enricher.enrich(state)
    |> EnrichmentPolicy.action_for_enrichment()
    |> case do
      {:persist, entries} ->
        Persistence.persist(entries)

      :renew_negative_cache ->
        Core.NegCache.put(phrase)
        :ok
    end
  end

  defp schedule_tick(ms), do: Process.send_after(self(), :tick, ms)
end
