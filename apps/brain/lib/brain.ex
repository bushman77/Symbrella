# apps/brain/lib/brain.ex
defmodule Brain do
  @moduledoc """
  Central coordinator for `Brain.Cell` processes and a lightweight region macro.

  This module serves two roles:

  1) **OTP Coordinator (GenServer)**

     Manages:
     * Working Memory (WM) and attention context
     * Stage-1 pipeline orchestration (delegates to `Brain.Pipeline.LIFGStage1`)
     * Fan-out “control signals” (boost/inhibit) to cell processes
     * Optional recall-to-WM ingestion from Hippocampus

  2) **Region helper macro**

     Re-exports `Brain.Region` so region modules can do:

      * `use Brain, region: :lifg`

  ## Runtime safety

  Many side-effecting operations (cells, mood core, bus) are designed to be safe in reduced
  test contexts. Some features require supporting processes/modules to be running:

  * `Brain.Registry` (a `Registry`) is required for per-cell lookups.
  * Some pipeline calls require downstream modules (LIFG/ATL/PMTG/Hippocampus).

  Doctests in this module avoid hard coupling by conditionally starting `Brain` and
  `Brain.Registry` only when absent.
  """

  alias Brain.Cell.Runtime, as: CellRT
  alias Brain.WM.Focus, as: WMFocus
  alias Brain.Config, as: BrainConfig

  # ───────────────────────── Region Macro (re-export) ─────────────────────────

  @doc ~S"""
  Re-export `Brain.Region` so region modules can use `use Brain, region: ...`.

  This macro exists to keep existing region modules stable while allowing the
  region implementation to live in `Brain.Region`.

  ## Example

      iex> [{mod, _bin}] =
      ...>   Code.compile_string(
      ...>     "defmodule Brain.DocTestRegion do\\n  use Brain, region: :lifg\\nend\\n"
      ...>   )
      iex> mod == Brain.DocTestRegion
      true

  """
  defmacro __using__(opts) do
    quote location: :keep do
      use Brain.Region, unquote(opts)
    end
  end

  # ───────────────────────── Brain Server ─────────────────────────

  use GenServer
  require Logger

  alias Db.BrainCell, as: Row
  alias Brain.Utils.ControlSignals
  alias Brain.Utils.Numbers
  alias Brain.Utils.Tokens
  alias Brain.WorkingMemory

  @name __MODULE__
  @registry Brain.Registry
  @cell_timeout 2_000

  # Telemetry constants
  @wm_update_event [:brain, :wm, :update]

  @wm_defaults %{
    capacity: 7,
    decay_ms: 30_000,
    gate_threshold: 0.4,
    merge_duplicates?: true,
    # Optional knobs (policy defaults preserve prior behavior):
    lemma_budget: 2,
    replace_margin: 0.10,
    diversity_lambda: 0.06,
    allow_unk?: true,
    allow_seed?: true,
    fallback_scale: 0.70,
    # P-206: fallbacks must meet the normal threshold unless explicitly allowed
    allow_fallback_into_wm?: false
  }

  @type id :: any()
  @type wm_item :: map()

  @type wm_cfg :: %{
          capacity: pos_integer(),
          decay_ms: pos_integer(),
          gate_threshold: number(),
          merge_duplicates?: boolean(),
          lemma_budget: pos_integer(),
          replace_margin: number(),
          diversity_lambda: number(),
          allow_unk?: boolean(),
          allow_seed?: boolean(),
          fallback_scale: number(),
          allow_fallback_into_wm?: boolean()
        }

  @type state :: %{
          history: list(),
          active_cells: %{optional(String.t()) => number()},
          attention: map(),
          wm: [wm_item()],
          wm_cfg: wm_cfg(),
          activation_log: list(),
          wm_last_ms: non_neg_integer() | nil,
          last_intent: map() | nil
        }

  # ── Small wrappers (hide raw GenServer.* from call-sites) ──────────────────

  defp gencall(name, msg, timeout \\ 5_000), do: :gen_server.call(name, msg, timeout)
  defp gencast(name, msg), do: :gen_server.cast(name, msg)

  # ── Pipeline bridge (internal-use) ───────────────────────────────────────────

  @doc false
  def __pipeline_do_focus__(state, cands_or_si, opts), do: WMFocus.run(state, cands_or_si, opts)

  @doc false
  def __pipeline_emit_wm_update__(capacity, size, added, removed, reason \\ nil),
    do: emit_wm_update(capacity, size, added, removed, reason)

  @doc false
  def __pipeline_safe_mood_update_wm__(wm_list), do: safe_mood_update_wm(wm_list)

  @doc false
  def __pipeline_apply_control_signals__(boosts, inhibitions, opts),
    do: apply_control_signals(boosts, inhibitions, opts)

  # ── Public API: WM/Attention ────────────────────────────────────────────────

  @doc ~S"""
  Configure working-memory policy knobs.

  This updates the in-server WM configuration. Any missing config keys are filled
  from module defaults. WM is also trimmed to the new capacity immediately.

  Common options:

  * `:capacity` (pos_integer) — max WM items retained
  * `:decay_ms` (pos_integer) — policy half-life / decay control (implementation-dependent)
  * `:gate_threshold` (0.0..1.0) — minimum score to admit items into WM
  * `:merge_duplicates?` (boolean) — whether duplicates should merge
  * and additional policy knobs present in `@wm_defaults`

  Returns `:ok`.

  ## Example

      iex> pid0 = Process.whereis(Brain)
      iex> started? = is_nil(pid0)
      iex> if started?, do: {:ok, _} = GenServer.start_link(Brain, :ok, name: Brain)
      iex> :ok = Brain.configure_wm(capacity: 3)
      iex> Brain.snapshot_wm().cfg.capacity
      3
      iex> if started?, do: GenServer.stop(Process.whereis(Brain)), else: :ok
      :ok

  """
  @spec configure_wm(keyword()) :: :ok
  def configure_wm(opts) when is_list(opts), do: gencall(@name, {:configure_wm, Map.new(opts)})

  @doc ~S"""
  Set an attention context map (asynchronous).

  Attention is free-form metadata used by downstream policy and UI layers.
  This is a `cast`, so it is applied asynchronously.

  Returns `:ok`.

  ## Example

      iex> pid0 = Process.whereis(Brain)
      iex> started? = is_nil(pid0)
      iex> if started?, do: {:ok, _} = GenServer.start_link(Brain, :ok, name: Brain)
      iex> :ok = Brain.set_attention(%{topic: :demo})
      iex> Process.sleep(5)
      iex> Brain.snapshot_wm().attention[:topic]
      :demo
      iex> if started?, do: GenServer.stop(Process.whereis(Brain)), else: :ok
      :ok

  """
  @spec set_attention(map()) :: :ok
  def set_attention(ctx) when is_map(ctx), do: gencast(@name, {:set_attention, ctx})

  @doc ~S"""
  Gate and update working memory given candidates (list) or an SI-like map.

  This calls the WM focus policy (`Brain.WM.Focus.run/3`) and updates WM in the server.
  The returned WM list is **newest-first**.

  Telemetry emitted:

  * `[:brain, :wm, :update]` with measurements: `size`, `added`, `removed`, `capacity`

  Returns the updated WM list.

  ## Example

      iex> pid0 = Process.whereis(Brain)
      iex> started? = is_nil(pid0)
      iex> if started?, do: {:ok, _} = GenServer.start_link(Brain, :ok, name: Brain)
      iex> is_list(Brain.focus([], []))
      true
      iex> if started?, do: GenServer.stop(Process.whereis(Brain)), else: :ok
      :ok

  """
  @spec focus(list() | map(), keyword()) :: [wm_item()]
  def focus(cands_or_si, opts \\ []), do: gencall(@name, {:focus, cands_or_si, Map.new(opts)})

  @doc ~S"""
  Remove an item from WM by id or predicate.

  The argument may be:

  * an id (as stored in WM entries), or
  * a predicate function `(wm_item() -> boolean())`

  This triggers a WM update telemetry event (reason `:defocus`) and updates MoodCore
  (if available).

  Returns `:ok`.

  ## Example

      iex> pid0 = Process.whereis(Brain)
      iex> started? = is_nil(pid0)
      iex> if started?, do: {:ok, _} = GenServer.start_link(Brain, :ok, name: Brain)
      iex> :ok = Brain.defocus("missing-id")
      iex> if started?, do: GenServer.stop(Process.whereis(Brain)), else: :ok
      :ok

  """
  @spec defocus(id() | (wm_item() -> boolean())) :: :ok
  def defocus(id_or_fun), do: gencall(@name, {:defocus, id_or_fun})

  @doc ~S"""
  Snapshot WM state for UI/debug.

  Returns a map:

  * `:wm` — current WM list (newest-first)
  * `:cfg` — current WM config
  * `:attention` — current attention context

  ## Example

      iex> pid0 = Process.whereis(Brain)
      iex> started? = is_nil(pid0)
      iex> if started?, do: {:ok, _} = GenServer.start_link(Brain, :ok, name: Brain)
      iex> snap = Brain.snapshot_wm()
      iex> is_list(snap.wm) and is_map(snap.cfg) and is_map(snap.attention)
      true
      iex> if started?, do: GenServer.stop(Process.whereis(Brain)), else: :ok
      :ok

  """
  @spec snapshot_wm() :: %{wm: [wm_item()], cfg: wm_cfg(), attention: map()}
  def snapshot_wm, do: gencall(@name, :snapshot_wm)

  # ── Public API: existing surface (unchanged) ────────────────────────────────

  @doc ~S"""
  Child spec for supervising `Brain` as a worker.

  This is suitable for inclusion in a supervisor tree.

  ## Example

      iex> spec = Brain.child_spec([])
      iex> spec.id == Brain and spec.type == :worker and is_map(spec)
      true

  """
  @spec child_spec(keyword()) :: Supervisor.child_spec()
  def child_spec(opts) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [opts]},
      type: :worker,
      restart: :permanent,
      shutdown: 500
    }
  end

  @doc ~S"""
  Start the `Brain` GenServer registered under the `Brain` module name.

  In most deployments, `Brain` is started by supervision. This function is still
  useful for direct startup in tests.

  Returns `{:ok, pid}` on success.

  ## Example

      iex> pid0 = Process.whereis(Brain)
      iex> if is_nil(pid0) do
      ...>   {:ok, pid} = Brain.start_link([])
      ...>   GenServer.stop(pid)
      ...>   :ok
      ...> else
      ...>   :ok
      ...> end
      :ok

  """
  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(_opts \\ []), do: GenServer.start_link(__MODULE__, :ok, name: @name)

  @doc ~S"""
  Merge short-term memory (STM) snapshot into an SI-like map.

  This is a read-only helper that:

  * attaches `:active_cells` from the server state into the input map
  * normalizes `:tokens` using `Brain.Utils.Tokens.normalize_tokens/2` if a sentence is present

  Returns the updated SI map.

  ## Example

      iex> pid0 = Process.whereis(Brain)
      iex> started? = is_nil(pid0)
      iex> if started?, do: {:ok, _} = GenServer.start_link(Brain, :ok, name: Brain)
      iex> out = Brain.stm(%{sentence: "hi", tokens: []})
      iex> out.active_cells == %{} and is_list(out.tokens)
      true
      iex> if started?, do: GenServer.stop(Process.whereis(Brain)), else: :ok
      :ok

  """
  @spec stm(map()) :: map()
  def stm(si) when is_map(si), do: gencall(@name, {:stm, si})

  @doc ~S"""
  Activate a collection of cells asynchronously.

  Accepts:

  * a list of `%Db.BrainCell{}` rows,
  * ids, or
  * an SI-like map that can be reduced into activation items by `extract_items/1`.

  `payload` defaults to `%{delta: 1}` and is forwarded to the cell runtime.

  Returns `:ok` (cast).

  ## Example

      iex> pid0 = Process.whereis(Brain)
      iex> started? = is_nil(pid0)
      iex> if started?, do: {:ok, _} = GenServer.start_link(Brain, :ok, name: Brain)
      iex> :ok = Brain.activate_cells([], %{delta: 1})
      iex> if started?, do: GenServer.stop(Process.whereis(Brain)), else: :ok
      :ok

  """
  @spec activate_cells([Row.t() | id()] | map(), map()) :: :ok
  def activate_cells(rows_or_ids, payload \\ %{delta: 1}) when is_map(payload) do
    gencast(@name, {:activate_cells, rows_or_ids, payload})
  end

  @doc ~S"""
  Query a specific cell’s status synchronously.

  This call requires `Brain.Registry` to be running (as it uses `Registry.lookup/2`).
  If the id is not registered, returns `{:error, :not_found}`.

  ## Example (not found)

      iex> reg0 = Process.whereis(Brain.Registry)
      iex> if is_nil(reg0), do: {:ok, _} = Registry.start_link(keys: :unique, name: Brain.Registry)
      iex> pid0 = Process.whereis(Brain)
      iex> started? = is_nil(pid0)
      iex> if started?, do: {:ok, _} = GenServer.start_link(Brain, :ok, name: Brain)
      iex> Brain.cell_status("does-not-exist")
      {:error, :not_found}
      iex> if started?, do: GenServer.stop(Process.whereis(Brain)), else: :ok
      :ok

  """
  @spec cell_status(String.t()) :: {:ok, term()} | {:error, :not_found}
  def cell_status(id) when is_binary(id), do: gencall(@name, {:cell, id, :status})

  # LIFG → WM gating (fires a cast; returns :ok)
  @doc false
  def gate_from_lifg(si, opts \\ []) when is_map(si) and is_list(opts) do
    gencast(@name, {:gate_from_lifg, si, opts})
    :ok
  end

  @doc ~S"""
  Send an asynchronous message to a cell by id.

  This requires `Brain.Registry` to be running. If the cell is not present,
  the message is ignored.

  Returns `:ok`.

  ## Example (no-op when missing)

      iex> reg0 = Process.whereis(Brain.Registry)
      iex> if is_nil(reg0), do: {:ok, _} = Registry.start_link(keys: :unique, name: Brain.Registry)
      iex> pid0 = Process.whereis(Brain)
      iex> started? = is_nil(pid0)
      iex> if started?, do: {:ok, _} = GenServer.start_link(Brain, :ok, name: Brain)
      iex> :ok = Brain.cell_cast("does-not-exist", :ping)
      iex> if started?, do: GenServer.stop(Process.whereis(Brain)), else: :ok
      :ok

  """
  @spec cell_cast(String.t(), term()) :: :ok
  def cell_cast(id, msg) when is_binary(id), do: gencast(@name, {:cell, id, msg})

  @doc ~S"""
  Non-mutating snapshot of the server state.

  This is intended for web/debug inspection. It returns the full internal state map.

  ## Example

      iex> pid0 = Process.whereis(Brain)
      iex> started? = is_nil(pid0)
      iex> if started?, do: {:ok, _} = GenServer.start_link(Brain, :ok, name: Brain)
      iex> s = Brain.snapshot()
      iex> is_map(s) and Map.has_key?(s, :wm) and Map.has_key?(s, :wm_cfg)
      true
      iex> if started?, do: GenServer.stop(Process.whereis(Brain)), else: :ok
      :ok

  """
  @spec snapshot() :: state()
  def snapshot, do: gencall(@name, :snapshot)

  @doc ~S"""
  Run LIFG Stage-1 and emit control signals.

  This is the public Stage-1 entry point. Internally, the GenServer delegates to:

  * `Brain.Pipeline.LIFGStage1.run/4`

  It returns:

  * `{:ok, %{choices, boosts, inhibitions, audit}}`, or
  * `{:error, reason}`

  Notes:

  * This call uses `:infinity` timeout because Stage-1 may perform multiple sub-steps.
  * It assumes downstream pipeline modules are available in the running system.

  ## Example (export check)

  The full pipeline is environment-dependent; this doctest only asserts the function exists:

      iex> function_exported?(Brain, :lifg_stage1, 3)
      true

  """
  @spec lifg_stage1(map() | [map()], [number()], keyword()) ::
          {:ok, %{choices: [map()], boosts: list(), inhibitions: list(), audit: map()}}
          | {:error, term()}
  def lifg_stage1(si_or_candidates, context_vec, opts \\ []) when is_list(context_vec) do
    gencall(@name, {:lifg_stage1, si_or_candidates, context_vec, opts}, :infinity)
  end

  # ───────────────────────── GenServer callbacks ──────────────────────────────

  @impl true
  def init(:ok) do
    {:ok,
     %{
       assistant: BrainConfig.assistant(),
       history: [],
       active_cells: %{},
       attention: %{},
       wm: [],
       wm_cfg: @wm_defaults,
       activation_log: [],
       wm_last_ms: nil,
       last_intent: nil
     }}
  end

  # group all handle_call/3 together

  @impl true
  def handle_call({:configure_wm, opts}, _from, state) do
    cfg2 =
      state.wm_cfg
      # guarantee missing keys are filled
      |> Map.merge(@wm_defaults)
      |> merge_wm_opts(Map.new(opts))

    wm2 = WorkingMemory.trim(state.wm, cfg2.capacity)
    {:reply, :ok, %{state | wm_cfg: cfg2, wm: wm2}}
  end

  @impl true
  def handle_call({:focus, cands_or_si, opts}, _from, state) do
    {wm_next, added, removed} = WMFocus.run(state, cands_or_si, opts)
    emit_wm_update(state.wm_cfg.capacity, length(wm_next), added, removed)
    _ = safe_mood_update_wm(wm_next)
    {:reply, wm_next, %{state | wm: wm_next}}
  end

  @impl true
  def handle_call({:defocus, id_or_fun}, _from, state) do
    {wm2, removed} = WorkingMemory.remove(state.wm, id_or_fun)
    emit_wm_update(state.wm_cfg.capacity, length(wm2), 0, removed, :defocus)
    _ = safe_mood_update_wm(wm2)
    {:reply, :ok, %{state | wm: wm2}}
  end

  @impl true
  def handle_call(:snapshot_wm, _from, state),
    do: {:reply, %{wm: state.wm, cfg: state.wm_cfg, attention: state.attention}, state}

  @impl true
  def handle_call({:lifg_stage1, si_or_cands, ctx_vec, opts}, _from, state) do
    {reply, state2} = Brain.Pipeline.LIFGStage1.run(si_or_cands, ctx_vec, opts, state)
    {:reply, reply, state2}
  end

  @impl true
  def handle_call({:stm, si}, _from, state) when is_map(si) do
    sentence = Map.get(si, :sentence) || Map.get(si, "sentence")
    tokens0 = Map.get(si, :tokens) || Map.get(si, "tokens") || []
    tokens1 = if is_list(tokens0), do: Tokens.normalize_tokens(tokens0, sentence), else: []

    si2 =
      si
      |> Map.put(:active_cells, state.active_cells)
      |> Map.put(:tokens, tokens1)

    {:reply, si2, state}
  end

  @impl true
  def handle_call(:snapshot, _from, state), do: {:reply, state, state}

  @impl true
  def handle_call({:cell, id, req}, _from, state) when is_binary(id) do
    case Registry.lookup(@registry, id) do
      [{pid, _}] -> {:reply, GenServer.call(pid, req, @cell_timeout), state}
      [] -> {:reply, {:error, :not_found}, state}
    end
  end

  @impl true
  def handle_call(:latest_intent, _from, state), do: {:reply, state.last_intent, state}

  # group all handle_cast/2 together

  @impl true
  def handle_cast({:set_attention, ctx}, state),
    do: {:noreply, %{state | attention: Map.new(ctx)}}

  @impl true
  def handle_cast({:activate_cells, rows_or_ids, payload}, state) do
    rows_or_ids
    |> extract_items()
    |> Enum.each(fn
      %Row{} = row ->
        CellRT.ensure_start_and_cast(row, payload)

      %{} = map_item ->
        CellRT.handle_map_item_activation(map_item, payload)

      id when is_binary(id) ->
        CellRT.ensure_start_and_cast(id, payload)

      other ->
        :telemetry.execute([:brain, :activate, :unknown_item], %{count: 1}, %{sample: other})
        Logger.debug("Brain.activate_cells: unknown item #{inspect(other)}")
    end)

    {:noreply, state}
  end

  @impl true
  def handle_cast({:activation_report, id, a}, state) when is_binary(id) and is_number(a) do
    new_active = Map.put(state.active_cells, id, a)
    _ = safe_mood_register_activation(new_active)
    {:noreply, %{state | active_cells: new_active}}
  end

  @impl true
  def handle_cast({:cell, id, msg}, state) when is_binary(id) do
    case Registry.lookup(@registry, id) do
      [{pid, _}] -> GenServer.cast(pid, msg)
      [] -> :ok
    end

    {:noreply, state}
  end

  @impl true
  def handle_cast({:set_latest_intent, m}, state) do
    li = normalize_intent_map(m)
    _ = safe_mood_apply_intent(li.intent, li.confidence)
    {:noreply, %{state | last_intent: li}}
  end

  @impl true
  def handle_cast({:gate_from_lifg, si, opts}, state) do
    now_ms = System.system_time(:millisecond)

    with {:ok, %{si: _si2, event: stage2_event}} <-
           Brain.LIFG.Stage2.run(si, opts) do
      decisions = Map.get(stage2_event, :decisions, [])

      wm2 =
        Brain.WorkingMemory.ingest_stage2(
          state.wm,
          decisions,
          now_ms,
          state.wm_cfg
        )

      state2 =
        state
        |> Map.put(:wm, wm2)
        |> Brain.decay_and_evict(now_ms)

      emit_wm_update(
        state.wm_cfg.capacity,
        length(state2.wm),
        count_added(state.wm, state2.wm),
        count_removed(state.wm, state2.wm),
        :lifg_stage2
      )

      _ = safe_mood_update_wm(state2.wm)

      {:noreply, state2}
    else
      {:skip, _reason} ->
        # Stage2 chose not to act; preserve WM exactly
        {:noreply, state}

      {:error, reason} ->
        Logger.error("gate_from_lifg Stage2 error: #{inspect(reason)}")
        {:noreply, state}
    end
  end

  # ───────────────────────── Public helper: recall → WM ───────────────────────

  defp count_added(before, aftr), do: max(length(aftr) - length(before), 0)
  defp count_removed(before, aftr), do: max(length(before) - length(aftr), 0)

  @doc ~S"""
  Return the last intent payload stored in the Brain server, if any.

  This is typically set by `set_latest_intent/1` (called from Core) and used to inform
  mood and UI snapshots.

  Returns a normalized map or `nil`.

  ## Example

      iex> pid0 = Process.whereis(Brain)
      iex> started? = is_nil(pid0)
      iex> if started?, do: {:ok, _} = GenServer.start_link(Brain, :ok, name: Brain)
      iex> Brain.latest_intent()
      nil
      iex> if started?, do: GenServer.stop(Process.whereis(Brain)), else: :ok
      :ok

  """
  @spec latest_intent() :: map() | nil
  def latest_intent, do: gencall(@name, :latest_intent)

  @doc ~S"""
  Update the Brain server’s last intent snapshot (asynchronous).

  The input map is normalized into:

  * `:intent` (atom; unknown becomes `:unknown`)
  * `:keyword` (string)
  * `:confidence` (float)
  * `:at_ms` (millisecond timestamp)

  This also forwards intent to `Brain.MoodCore.apply_intent/2` if MoodCore is available.

  Returns `:ok`.

  ## Example

      iex> pid0 = Process.whereis(Brain)
      iex> started? = is_nil(pid0)
      iex> if started?, do: {:ok, _} = GenServer.start_link(Brain, :ok, name: Brain)
      iex> :ok = Brain.set_latest_intent(%{intent: :greet, keyword: "hello", confidence: 0.9})
      iex> Process.sleep(5)
      iex> li = Brain.latest_intent()
      iex> li.intent == :greet and li.keyword == "hello" and li.confidence > 0.0
      true
      iex> if started?, do: GenServer.stop(Process.whereis(Brain)), else: :ok
      :ok

  """
  @spec set_latest_intent(map()) :: :ok
  def set_latest_intent(m) when is_map(m), do: gencast(@name, {:set_latest_intent, m})

  @doc ~S"""
  Recall from Hippocampus and gate results into WM.

  This function calls `Brain.Hippocampus.recall/2` (best-effort) and converts recall results into
  candidate WM items, then focuses them into WM via `focus/2`.

  Because recall availability is environment-dependent, this doctest only asserts export:

      iex> function_exported?(Brain, :focus_from_recall, 2)
      true

  """
  @spec focus_from_recall(map() | list(), keyword()) :: [map()]
  def focus_from_recall(si_or_cues, recall_opts \\ []) do
    sentence = extract_sentence(si_or_cues)
    results = safe_hippo_recall(si_or_cues, recall_opts)

    cands =
      case results do
        list when is_list(list) and list != [] ->
          Enum.map(list, &recall_result_to_wm_candidate/1)

        _ ->
          si_or_cues
          |> cues_to_candidates(sentence)
          |> Enum.map(&cue_to_wm_candidate/1)
      end

    focus(cands, [])
  end

  defp safe_hippo_recall(si_or_cues, recall_opts) do
    if Code.ensure_loaded?(Brain.Hippocampus) and
         function_exported?(Brain.Hippocampus, :recall, 2) do
      try do
        Brain.Hippocampus.recall(si_or_cues, recall_opts)
      rescue
        _ -> []
      catch
        :exit, _ -> []
      end
    else
      []
    end
  end

  defp recall_result_to_wm_candidate(r) when is_map(r) do
    slate = get_in(r, [:episode, :slate]) || get_in(r, ["episode", "slate"])

    lemma =
      Map.get(r, :lemma) ||
        Map.get(r, "lemma") ||
        (slate && first_lemma_from_slate(slate)) ||
        ""

    lemma_s = to_string(lemma)

    id =
      Map.get(r, :id) ||
        Map.get(r, "id") ||
        (lemma_s != "" && "#{lemma_s}|ltm") ||
        "ltm"

    %{
      token_index: as_nonneg_int(Map.get(r, :token_index) || Map.get(r, "token_index") || 0),
      id: to_string(id),
      lemma: lemma_s,
      score: as_float(Map.get(r, :score) || Map.get(r, "score") || 1.0),
      source: :ltm,
      reason: :hippocampus,
      payload: r
    }
  end

  defp recall_result_to_wm_candidate(other) do
    %{
      token_index: 0,
      id: "ltm",
      lemma: "ltm",
      score: 0.0,
      source: :ltm,
      reason: :hippocampus,
      payload: other
    }
  end

  defp cue_to_wm_candidate(c) when is_map(c) do
    lemma = to_string(c[:lemma] || c["lemma"] || "")
    id = c[:id] || c["id"] || (lemma != "" && "#{lemma}|ltm") || "ltm"

    %{
      token_index: as_nonneg_int(c[:token_index] || c["token_index"] || 0),
      id: to_string(id),
      lemma: lemma,
      score: as_float(c[:score] || c["score"] || 1.0),
      source: :ltm,
      reason: :hippocampus_fallback
    }
  end

  defp cue_to_wm_candidate(other) do
    %{
      token_index: 0,
      id: "ltm",
      lemma: "ltm",
      score: 0.0,
      source: :ltm,
      reason: :hippocampus_fallback,
      payload: other
    }
  end

  defp extract_sentence(%{} = m), do: Map.get(m, :sentence) || Map.get(m, "sentence")
  defp extract_sentence(_), do: nil

  defp as_nonneg_int(n) when is_integer(n) and n >= 0, do: n

  defp as_nonneg_int(n) when is_binary(n) do
    case Integer.parse(String.trim(n)) do
      {i, _} when i >= 0 -> i
      _ -> 0
    end
  end

  defp as_nonneg_int(_), do: 0

  defp as_float(n) when is_number(n), do: n * 1.0

  defp as_float(n) when is_binary(n) do
    case Float.parse(String.trim(n)) do
      {f, _} -> f
      _ -> 0.0
    end
  end

  defp as_float(_), do: 0.0

  # ───────────────────────── WM decay & eviction (τ-model) ────────────────────

  @doc ~S"""
  Apply score decay to WM entries based on elapsed time.

  This is a pure helper that expects a state map with:

  * `:wm` — list of entries (optionally containing `:score`)
  * `:wm_last_ms` — last timestamp used for decay (or `nil`)

  The function computes `dt = now_ms - last_ms`, obtains a multiplicative decay factor
  via `Brain.Utils.Numbers.decay_factor_ms/1`, and clamps scores into `0.0..1.0`.

  Returns an updated state with `:wm` and `:wm_last_ms` adjusted.

  ## Example

      iex> s0 = %{wm: [%{id: "a", score: 1.0}], wm_last_ms: 0}
      iex> s1 = Brain.apply_decay(s0, 10)
      iex> is_list(s1.wm) and is_integer(s1.wm_last_ms)
      true

  """
  def apply_decay(%{wm: wm} = state, now_ms) when is_list(wm) and is_integer(now_ms) do
    last_raw = Map.get(state, :wm_last_ms, nil)
    last = if is_integer(last_raw), do: last_raw, else: now_ms
    dt = max(now_ms - last, 0)
    k = Numbers.decay_factor_ms(dt)

    wm2 =
      Enum.map(wm, fn
        %{score: s} = e when is_number(s) -> %{e | score: Numbers.clamp01(s * k)}
        e -> e
      end)

    state
    |> Map.put(:wm, wm2)
    |> Map.put(:wm_last_ms, now_ms)
  end

  def apply_decay(state, _now_ms), do: state

  @doc ~S"""
  Evict WM entries if the list exceeds configured capacity.

  The function expects a state map containing:

  * `:wm` — list of entries
  * `:wm_cfg.capacity` — integer capacity

  If eviction is needed, entries are sorted by:

  * score (descending), then
  * timestamp `:ts` (descending; defaults to `0`)

  Returns an updated state map.

  ## Example

      iex> st = %{wm: [%{id: 1, score: 0.2}, %{id: 2, score: 0.9}], wm_cfg: %{capacity: 1}}
      iex> out = Brain.evict_if_needed(st)
      iex> length(out.wm)
      1
      iex> hd(out.wm).id
      2

  """
  def evict_if_needed(%{wm: wm, wm_cfg: %{capacity: cap}} = state)
      when is_list(wm) and is_integer(cap) and cap >= 0 do
    if length(wm) <= cap do
      state
    else
      kept =
        wm
        |> Enum.sort_by(fn e -> {Map.get(e, :score, 0.0), Map.get(e, :ts, 0)} end, :desc)
        |> Enum.take(cap)

      Map.put(state, :wm, kept)
    end
  end

  def evict_if_needed(state), do: state

  @doc ~S"""
  Convenience helper to apply decay and then evict to capacity.

  Equivalent to:

  * `state |> apply_decay(now_ms) |> evict_if_needed()`

  ## Example

      iex> st = %{wm: [%{id: 1, score: 0.2}, %{id: 2, score: 0.9}], wm_cfg: %{capacity: 1}, wm_last_ms: 0}
      iex> out = Brain.decay_and_evict(st, 10)
      iex> length(out.wm) == 1 and is_integer(out.wm_last_ms)
      true

  """
  def decay_and_evict(state, now_ms) do
    state
    |> apply_decay(now_ms)
    |> evict_if_needed()
  end

  # ───────────────────────── Misc helpers ─────────────────────────────────────

  defp emit_wm_update(capacity, size, added, removed, reason \\ nil) do
    meta = if reason, do: %{reason: reason}, else: %{}

    :telemetry.execute(
      @wm_update_event,
      %{size: size, added: added, removed: removed, capacity: capacity},
      meta
    )
  end

  @doc ~S"""
  Coalesce `{id, delta}` control-signal pairs.

  This is a thin pass-through to `Brain.Utils.ControlSignals.coalesce_pairs/1`.

  ## Example

      iex> out = Brain.coalesce_pairs([{"a", 1}, {"a", 2}, {"b", -1}])
      iex> is_list(out)
      true

  """
  def coalesce_pairs(list), do: ControlSignals.coalesce_pairs(list)

  @doc ~S"""
  Return the `Registry`-based `via` tuple for a cell id.

  This delegates to `Brain.Cell.Runtime.via/1` and is used to address cells without
  leaking runtime details.

  ## Example

      iex> is_tuple(Brain.via("cell-1"))
      true

  """
  def via(id) when is_binary(id), do: CellRT.via(id)

  # ───────────────────────── Internal helpers ─────────────────────────────────

  defp cues_to_candidates(cues), do: cues_to_candidates(cues, nil)

  defp cues_to_candidates(cues, sentence) do
    winners =
      case cues do
        %{winners: ws} when is_list(ws) -> ws
        %{"winners" => ws} when is_list(ws) -> ws
        l when is_list(l) -> l
        nil -> []
        x -> [x]
      end

    sent = normalize_sentence(sentence)

    Enum.flat_map(winners, fn w ->
      cond do
        is_map(w) and (w[:id] || w["id"]) ->
          id = w[:id] || w["id"]

          [
            %{
              token_index: w[:token_index] || w["token_index"] || 0,
              id: to_string(id),
              score: w[:score] || w["score"] || 1.0
            }
          ]

        is_map(w) and (w[:lemma] || w["lemma"] || w[:phrase] || w["phrase"]) ->
          lemma = (w[:lemma] || w["lemma"] || w[:phrase] || w["phrase"]) |> to_string()

          [
            %{
              token_index: w[:token_index] || w["token_index"] || 0,
              lemma: lemma,
              score: w[:score] || w["score"] || 1.0
            }
          ]

        is_binary(w) and String.contains?(w, "|") ->
          [%{token_index: 0, id: w, score: 1.0}]

        is_binary(w) ->
          l =
            w
            |> String.downcase()
            |> String.replace(~r/\s+/u, " ")
            |> String.trim()

          cond do
            l == "" ->
              []

            # If we can validate against a sentence and it clearly doesn't match, skip.
            sent != "" and not String.contains?(sent, l) ->
              []

            # Otherwise, treat this as an LTM recall cue (lets WMFocus mint "#{lemma}|ltm").
            true ->
              [
                %{
                  token_index: 0,
                  lemma: l,
                  score: 0.30,
                  source: :ltm,
                  reason: :hippocampus_fallback
                }
              ]
          end

        true ->
          []
      end
    end)
  end

  defp normalize_sentence(s) when is_binary(s),
    do: s |> String.downcase() |> String.replace(~r/\s+/u, " ") |> String.trim()

  defp normalize_sentence(_), do: ""

  defp first_lemma_from_slate(%{winners: winners}) when is_list(winners) do
    winners
    |> Enum.find_value(fn w ->
      w[:lemma] || w["lemma"] || parse_id_word(w[:id] || w["id"]) || w[:word] || w["word"]
    end) || "ltm"
  end

  defp first_lemma_from_slate(_), do: "ltm"

  defp parse_id_word(nil), do: nil

  defp parse_id_word(id) when is_binary(id) do
    case String.split(id, "|", parts: 2) do
      [w | _] -> w
      _ -> nil
    end
  end

  # Small normalizers for WM config
  defp norm_pos_int(n, _d) when is_integer(n) and n > 0, do: n
  defp norm_pos_int(_, d), do: d
  defp norm_float_01(x, _d) when is_number(x) and x >= 0 and x <= 1, do: x * 1.0
  defp norm_float_01(_, d), do: d
  defp norm_float_01_or(x, _d) when is_number(x) and x >= 0 and x <= 1, do: x * 1.0
  defp norm_float_01_or(_, d), do: d

  defp extract_items(list) when is_list(list), do: list

  defp extract_items(%{} = si) do
    case Map.get(si, :active_cells, []) do
      list when is_list(list) ->
        list

      other ->
        Logger.warning("Brain.extract_items: :active_cells not a list (got #{inspect(other)})")
        []
    end
  end

  defp extract_items(%Row{} = row), do: [row]
  defp extract_items(id) when is_binary(id), do: [id]
  defp extract_items(_other), do: []

  # ───────────────────────── Internal: WM cfg merge ───────────────────────────

  defp merge_wm_opts(cfg, opts) when is_map(cfg) and is_map(opts) do
    %{
      cfg
      | capacity: norm_pos_int(Map.get(opts, :capacity, cfg.capacity), cfg.capacity),
        decay_ms: norm_pos_int(Map.get(opts, :decay_ms, cfg.decay_ms), cfg.decay_ms),
        gate_threshold:
          norm_float_01(Map.get(opts, :gate_threshold, cfg.gate_threshold), cfg.gate_threshold),
        merge_duplicates?: Map.get(opts, :merge_duplicates?, cfg.merge_duplicates?),
        lemma_budget:
          norm_pos_int(Map.get(opts, :lemma_budget, cfg.lemma_budget), cfg.lemma_budget),
        replace_margin:
          norm_float_01_or(Map.get(opts, :replace_margin, cfg.replace_margin), cfg.replace_margin),
        diversity_lambda:
          norm_float_01_or(
            Map.get(opts, :diversity_lambda, cfg.diversity_lambda),
            cfg.diversity_lambda
          ),
        allow_unk?: Map.get(opts, :allow_unk?, cfg.allow_unk?),
        allow_seed?: Map.get(opts, :allow_seed?, cfg.allow_seed?),
        fallback_scale:
          norm_float_01_or(Map.get(opts, :fallback_scale, cfg.fallback_scale), cfg.fallback_scale),
        allow_fallback_into_wm?:
          Map.get(opts, :allow_fallback_into_wm?, cfg.allow_fallback_into_wm?)
    }
  end

  # ───────────────────────── Control signal emission ─────────────────────────

  defp apply_control_signals(boosts, inhibitions, opts) do
    coalesce? = Keyword.get(opts, :coalesce, true)
    conc = Keyword.get(opts, :signal_concurrency, System.schedulers_online())
    delta_key = Keyword.get(opts, :delta_key, :delta)

    pairs =
      normalize_signal_pairs(boosts, +1, delta_key) ++
        normalize_signal_pairs(inhibitions, -1, delta_key)

    signals = if coalesce?, do: ControlSignals.coalesce_pairs(pairs), else: pairs

    signals
    |> Task.async_stream(
      fn {id, delta} ->
        CellRT.ensure_started(id, id)
        payload = %{delta_key => delta}
        GenServer.cast(CellRT.via(id), {:activate, payload})
      end,
      max_concurrency: conc,
      timeout: :infinity
    )
    |> Stream.run()

    :ok
  end

  defp normalize_signal_pairs(list, sign, delta_key) do
    list
    |> List.wrap()
    |> Enum.flat_map(fn
      {id, delta} ->
        id2 = normalize_id(id)
        d = sign * abs(num(delta))
        if is_nil(id2) or d == 0.0, do: [], else: [{id2, d}]

      %{} = m ->
        id = Map.get(m, :id) || Map.get(m, "id")

        raw =
          Map.get(m, delta_key) || Map.get(m, :delta) || Map.get(m, "delta") ||
            Map.get(m, :amount) || Map.get(m, "amount")

        id2 = normalize_id(id)
        d = sign * abs(num(raw))
        if is_nil(id2) or d == 0.0, do: [], else: [{id2, d}]

      _other ->
        []
    end)
  end

  defp normalize_id(nil), do: nil

  defp normalize_id(v) do
    s = v |> to_string() |> String.trim()
    if s == "", do: nil, else: s
  end

  defp num(v) when is_integer(v), do: v * 1.0
  defp num(v) when is_float(v), do: v

  defp num(v) when is_binary(v) do
    case Float.parse(String.trim(v)) do
      {f, _rest} -> f
      _ -> 0.0
    end
  end

  defp num(_), do: 0.0

  # ───────────────────────── Centralized WM helpers ───────────────────────────

  defp normalize_intent_map(m) do
    intent0 = m[:intent] || m["intent"]

    intent =
      cond do
        is_atom(intent0) ->
          intent0

        is_binary(intent0) ->
          try do
            String.to_existing_atom(intent0)
          rescue
            _ -> :unknown
          end

        true ->
          :unknown
      end

    kw = m[:keyword] || m["keyword"] || ""
    conf0 = m[:confidence] || m["confidence"] || 0.0

    conf =
      cond do
        is_number(conf0) ->
          conf0 * 1.0

        is_binary(conf0) ->
          case Float.parse(conf0) do
            {f, _} -> f
            _ -> 0.0
          end

        true ->
          0.0
      end

    at_ms = m[:at_ms] || m["at_ms"] || System.system_time(:millisecond)

    %{intent: intent, keyword: to_string(kw), confidence: clamp01(conf), at_ms: at_ms}
  end

  if Mix.env() == :test do
    @doc false
    def __test_do_focus__(state, choices, opts \\ %{}) do
      WMFocus.run(state, choices, opts)
    end
  end

  # ───────────────────────── MoodCore safe hooks ──────────────────────────────

  defp safe_mood_apply_intent(intent, conf) do
    if Code.ensure_loaded?(Brain.MoodCore) and
         function_exported?(Brain.MoodCore, :apply_intent, 2) do
      Brain.MoodCore.apply_intent(intent, conf)
    else
      :ok
    end
  end

  defp safe_mood_register_activation(active_cells) do
    if Code.ensure_loaded?(Brain.MoodCore) and
         function_exported?(Brain.MoodCore, :register_activation, 1) do
      Brain.MoodCore.register_activation(active_cells)
    else
      :ok
    end
  end

  defp safe_mood_update_wm(wm_list) do
    if Code.ensure_loaded?(Brain.MoodCore) and function_exported?(Brain.MoodCore, :update_wm, 1) do
      Brain.MoodCore.update_wm(wm_list)
    else
      :ok
    end
  end

  defp clamp01(x) when is_number(x), do: max(0.0, min(1.0, x))
  defp clamp01(_), do: 0.0
end
