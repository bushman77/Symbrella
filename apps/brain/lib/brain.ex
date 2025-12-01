defmodule Brain do
  @moduledoc """
  Central coordinator for `Brain.Cell` processes **and** a tiny `use Brain, region: :xyz` macro.

  Responsibilities:
  - Working Memory (WM) + Attention server
  - Stage-1 pipeline orchestration (delegates heavy lifting to Brain.LIFG + helpers)
  - Fan-out control signals to cells
  - Optional Episodes persistence

  For region helpers (LIFG etc.), prefer `use Brain, region: :lifg` in those modules.
  """

  alias Brain.Cell.Runtime, as: CellRT
  alias Brain.WM.Focus, as: WMFocus

  # ───────────────────────── Region Macro (re-export) ─────────────────────────

  @doc """
  Re-export `Brain.Region` so region modules can continue to do:

      defmodule Brain.LIFG do
        use Brain, region: :lifg
      end
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

  @spec configure_wm(keyword()) :: :ok
  def configure_wm(opts) when is_list(opts), do: gencall(@name, {:configure_wm, Map.new(opts)})

  @spec set_attention(map()) :: :ok
  def set_attention(ctx) when is_map(ctx), do: gencast(@name, {:set_attention, ctx})

  @doc """
  Gate & update working memory with candidates (list or SI-like map).
  Returns the updated WM (newest-first).
  """
  @spec focus(list() | map(), keyword()) :: [wm_item()]
  def focus(cands_or_si, opts \\ []), do: gencall(@name, {:focus, cands_or_si, Map.new(opts)})

  @spec defocus(id() | (wm_item() -> boolean())) :: :ok
  def defocus(id_or_fun), do: gencall(@name, {:defocus, id_or_fun})

  @spec snapshot_wm() :: %{wm: [wm_item()], cfg: wm_cfg(), attention: map()}
  def snapshot_wm, do: gencall(@name, :snapshot_wm)

  # ── Public API: existing surface (unchanged) ────────────────────────────────

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

  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(_opts \\ []), do: GenServer.start_link(__MODULE__, :ok, name: @name)

  @doc "Merge STM into a given SI map (populates `:active_cells`, normalizes tokens)."
  @spec stm(map()) :: map()
  def stm(si) when is_map(si), do: gencall(@name, {:stm, si})

  @doc "Activate rows/ids asynchronously with a payload (e.g., `%{delta: +1}`)."
  @spec activate_cells([Row.t() | id()] | map(), map()) :: :ok
  def activate_cells(rows_or_ids, payload \\ %{delta: 1}) when is_map(payload) do
    gencast(@name, {:activate_cells, rows_or_ids, payload})
  end

  @doc "Sync call to one cell (e.g., `:status`); returns `{:ok, term}` or `{:error, :not_found}`."
  @spec cell_status(String.t()) :: {:ok, term()} | {:error, :not_found}
  def cell_status(id) when is_binary(id), do: gencall(@name, {:cell, id, :status})

  # LIFG → WM gating (fires a cast; returns :ok)
  @doc false
  def gate_from_lifg(si, opts \\ []) when is_map(si) and is_list(opts) do
    gencast(@name, {:gate_from_lifg, si, opts})
    :ok
  end

  @doc "Async cast to one cell."
  @spec cell_cast(String.t(), term()) :: :ok
  def cell_cast(id, msg) when is_binary(id), do: gencast(@name, {:cell, id, msg})

  @doc "Non-mutating snapshot for Web/debug."
  @spec snapshot() :: state()
  def snapshot, do: gencall(@name, :snapshot)

  @doc """
  Run LIFG Stage-1 (pure), then fan-out control signals; also jit-feed ATL,
  optionally consult PMTG, and persist an Episode for this pass.

  Returns `{:ok, %{choices, boosts, inhibitions, audit}}` or `{:error, reason}`.
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
    new_state = Brain.WM.Gate.ingest_from_si(state, si, opts)
    _ = safe_mood_update_wm(new_state.wm)
    {:noreply, new_state}
  end

  # ───────────────────────── Public helper: recall → WM ───────────────────────

  @spec latest_intent() :: map() | nil
  def latest_intent, do: gencall(@name, :latest_intent)

  @spec set_latest_intent(map()) :: :ok
  def set_latest_intent(m) when is_map(m), do: gencast(@name, {:set_latest_intent, m})

  @doc """
  Recall from Hippocampus and gate results into WM.

  Accepts the same `opts` you’d pass to `Hippocampus.recall/2` (e.g., `limit`, `scope`, `min_jaccard`).
  Returns the **updated WM** (newest-first).
  """
  @spec focus_from_recall(map() | list(), keyword()) :: [map()]
  def focus_from_recall(si_or_cues, recall_opts \\ []) do
    results = Brain.Hippocampus.recall(si_or_cues, recall_opts)

    cands =
      if is_list(results) and results != [] do
        Enum.map(results, fn r ->
          slate = get_in(r, [:episode, :slate]) || get_in(r, ["episode", "slate"])

          lemma =
            Map.get(r, :lemma) ||
              Map.get(r, "lemma") ||
              (slate && first_lemma_from_slate(slate)) ||
              ""

          id = Map.get(r, :id) || Map.get(r, "id") || (lemma != "" && "#{lemma}|ltm") || "ltm"

          %{
            token_index: Map.get(r, :token_index) || Map.get(r, "token_index") || 0,
            id: to_string(id),
            lemma: to_string(lemma),
            score: Map.get(r, :score) || Map.get(r, "score") || 1.0,
            source: :ltm,
            reason: :hippocampus,
            payload: r
          }
        end)
      else
        si_or_cues
        |> cues_to_candidates()
        |> Enum.map(fn c ->
          lemma = (c[:lemma] || c["lemma"] || "") |> to_string()
          id = c[:id] || c["id"] || (lemma != "" && "#{lemma}|ltm") || "ltm"

          %{
            token_index: c[:token_index] || c["token_index"] || 0,
            id: to_string(id),
            lemma: lemma,
            score: c[:score] || c["score"] || 1.0,
            source: :ltm,
            reason: :hippocampus_fallback
          }
        end)
      end

    gencall(@name, {:focus, cands, %{}})
  end

  # ───────────────────────── WM decay & eviction (τ-model) ────────────────────

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

  def coalesce_pairs(list), do: ControlSignals.coalesce_pairs(list)

  # CellRT-via delegate (keeps existing callsites stable)
  def via(id) when is_binary(id), do: CellRT.via(id)

  defp cues_to_candidates(cues) do
    winners =
      case cues do
        %{winners: ws} when is_list(ws) -> ws
        %{"winners" => ws} when is_list(ws) -> ws
        l when is_list(l) -> l
        nil -> []
        x -> [x]
      end

    Enum.map(winners, fn w ->
      cond do
        is_map(w) and (w[:id] || w["id"]) ->
          id = w[:id] || w["id"]

          %{
            token_index: w[:token_index] || w["token_index"] || 0,
            id: to_string(id),
            score: w[:score] || w["score"] || 1.0
          }

        is_map(w) and (w[:lemma] || w["lemma"] || w[:phrase] || w["phrase"]) ->
          lemma = (w[:lemma] || w["lemma"] || w[:phrase] || w["phrase"]) |> to_string()

          %{
            token_index: w[:token_index] || w["token_index"] || 0,
            lemma: lemma,
            score: w[:score] || w["score"] || 1.0
          }

        is_binary(w) and String.contains?(w, "|") ->
          %{token_index: 0, id: w, score: 1.0}

        is_binary(w) ->
          l = String.downcase(w)
          %{token_index: 0, id: "#{l}|phrase|fallback", lemma: l, score: 1.0, source: :runtime}

        true ->
          %{token_index: 0, score: 0.0}
      end
    end)
  end

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

    signals =
      boosts
      |> Kernel.++(inhibitions)
      |> then(fn pairs ->
        if coalesce?, do: ControlSignals.coalesce_pairs(pairs), else: pairs
      end)

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

