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

  # Suppress compile-time warnings about optional Llm.Pos
  @compile {:no_warn_undefined, Llm.Pos}

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
    res = llm_enrich(phrase, state)

    case res do
      {:ok, %{"entries" => entries}} when is_list(entries) and entries != [] ->
        persist(entries)

      _ ->
        # Still unknown or LLM unavailable → renew TTL to avoid hot-looping
        Core.NegCache.put(phrase)
        :ok
    end
  end

  # LLM enrichment is optional and fully guarded
  defp llm_enrich(phrase, state) do
    if Code.ensure_loaded?(Llm.Pos) and function_exported?(Llm.Pos, :run, 2) do
      try do
        Llm.Pos.run(
          phrase,
          model: state.llm_model,
          keep_alive: "5m",
          timeout: :timer.seconds(20),
          options: %{num_predict: 256, num_ctx: 1024, temperature: 0.0},
          allow_builtin_lexicon: true,
          require_nonempty_syn_ant?: false
        )
      rescue
        e ->
          Logger.warning("Curiosity: Llm.Pos.run/2 crashed: #{inspect(e)}")
          {:error, e}
      catch
        kind, reason ->
          Logger.warning("Curiosity: Llm.Pos.run/2 threw: #{inspect({kind, reason})}")
          {:error, reason}
      end
    else
      :undef
    end
  end

  # -- Persistence -------------------------------------------

  defp persist(entries) when is_list(entries) and entries != [] do
    write_as_episodes(entries)
    maybe_upsert_lexicon(entries)
    :ok
  end

  defp persist(_), do: :ok

  defp write_as_episodes(entries) do
    ep_at = System.system_time(:millisecond)
    hippo = Module.concat([Brain, Hippocampus])

    for e <- entries do
      episode = %{
        source: :curiosity,
        kind: :lexicon_seed,
        payload: e,
        at: ep_at
      }

      safe_apply(hippo, :write, [episode])
    end

    :ok
  end

  defp maybe_upsert_lexicon(entries) do
    if Application.get_env(:core, :curiosity_allow_lexicon_writes, false) do
      core_lex = Module.concat([Core, Lexicon])
      db_lex   = Module.concat([Db,   Lexicon])

      cond do
        export?(core_lex, :upsert_fallbacks, 1) ->
          safe_apply(core_lex, :upsert_fallbacks, [entries])

        export?(db_lex, :bulk_upsert_senses, 1) ->
          safe_apply(db_lex, :bulk_upsert_senses, [entries])

        true ->
          Logger.warning("Curiosity: no Lexicon upsert function found — skipping persist")
          :ok
      end
    else
      :ok
    end
  end

  # -- Helpers -----------------------------------------------

  defp export?(mod, fun, arity),
    do: Code.ensure_loaded?(mod) and function_exported?(mod, fun, arity)

  defp safe_apply(mod, fun, args) do
    if export?(mod, fun, length(args)) do
      try do
        apply(mod, fun, args)
      rescue
        e ->
          Logger.warning("Curiosity: #{inspect(mod)}.#{fun}/#{length(args)} failed: #{inspect(e)}")
          :error
      catch
        _, _ -> :error
      end
    else
      :undef
    end
  end

  defp schedule_tick(ms), do: Process.send_after(self(), :tick, ms)
end

