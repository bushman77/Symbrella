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

  # ───────────────────────────────────── Region Macro ─────────────────────────────────────
  @doc """
  Use inside a module to make it a region GenServer:

      defmodule Brain.LIFG do
        use Brain, region: :lifg
      end
  """
  defmacro __using__(opts) do
    region = Keyword.fetch!(opts, :region)

    quote location: :keep, bind_quoted: [region: region] do
      use GenServer
      @region region

      def start_link(opts) do
        name = Keyword.get(opts, :name, __MODULE__)
        GenServer.start_link(__MODULE__, opts, name: name)
      end

      def child_spec(opts) do
        %{
          id: __MODULE__,
          start: {__MODULE__, :start_link, [opts]},
          type: :worker,
          restart: :permanent,
          shutdown: 500
        }
      end

      @impl true
      def init(opts), do: {:ok, %{region: @region, opts: Map.new(opts), stats: %{}}}

      @impl true
      def handle_call(:status, _from, state), do: {:reply, state, state}

      @impl true
      def handle_cast(_msg, state), do: {:noreply, state}

      @impl true
      def handle_info(_msg, state), do: {:noreply, state}

      defoverridable init: 1, handle_call: 3, handle_cast: 2, handle_info: 2
    end
  end

  # ───────────────────────────────────────── Brain Server ─────────────────────────────────────────
  use GenServer
  require Logger

  alias Db.BrainCell, as: Row
  alias Brain.LIFG
  alias Brain.WorkingMemory
  alias Brain.Attention
  alias Brain.Utils.Tokens
  alias Brain.Utils.Numbers
  alias Brain.Utils.ControlSignals
  alias Brain.LIFG.Input, as: LIFGInput
  alias Brain.LIFG.Gate, as: LIFGGate
  alias Brain.Episodes.Writer, as: EpWriter

  @name __MODULE__
  @registry Brain.Registry
  @cell_sup Brain.CellSup
  @cell_timeout 2_000

  @type id :: any()
  @type wm_item :: map()
  @type wm_cfg :: %{
          capacity: pos_integer(),
          decay_ms: pos_integer(),
          gate_threshold: number(),
          merge_duplicates?: boolean()
        }
  @type state :: %{
          history: list(),
          active_cells: %{optional(String.t()) => number()},
          attention: map(),
          wm: [wm_item()],
          wm_cfg: wm_cfg(),
          activation_log: list(),
          wm_last_ms: non_neg_integer() | nil
        }

  # Small wrappers (keep the literal GenServer.* out of most call-sites)
  defp gencall(name, msg, timeout \\ 5_000), do: :gen_server.call(name, msg, timeout)
  defp gencast(name, msg), do: :gen_server.cast(name, msg)

  # ── Public API: WM/Attention ─────────────────────────────────────────────────────────────
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

  # ── Public API: existing surface (unchanged) ─────────────────────────────────────────────
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

  @doc "Activate rows/ids asynchronously with a payload (e.g., `%{delta: +0.2}`)."
  @spec activate_cells([Row.t() | id()] | map(), map()) :: :ok
  def activate_cells(rows_or_ids, payload \\ %{delta: 1}) when is_map(payload) do
    gencast(@name, {:activate_cells, rows_or_ids, payload})
  end

  @doc "Sync call to one cell (e.g., `:status`); returns `{:ok, term}` or `{:error, :not_found}`."
  @spec cell_status(String.t()) :: {:ok, term()} | {:error, :not_found}
  def cell_status(id) when is_binary(id), do: gencall(@name, {:cell, id, :status})

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

  # ── GenServer callbacks ─────────────────────────────────────────────

  @impl true
  def init(:ok) do
    {:ok,
     %{
       history: [],
       active_cells: %{},
       attention: %{},
       wm: [],
       wm_cfg: %{capacity: 7, decay_ms: 30_000, gate_threshold: 0.4, merge_duplicates?: true},
       activation_log: [],
       wm_last_ms: nil
     }}
  end

  # group all handle_call/3 together
  @impl true
  def handle_call({:configure_wm, opts}, _from, state) do
    cap = norm_pos_int(Map.get(opts, :capacity), state.wm_cfg.capacity)
    decay = norm_pos_int(Map.get(opts, :decay_ms), state.wm_cfg.decay_ms)
    thr = norm_float_01(Map.get(opts, :gate_threshold), state.wm_cfg.gate_threshold)
    md? = Map.get(opts, :merge_duplicates?, state.wm_cfg.merge_duplicates?)

    cfg2 = %{capacity: cap, decay_ms: decay, gate_threshold: thr, merge_duplicates?: !!md?}
    wm2 = WorkingMemory.trim(state.wm, cfg2.capacity)

    {:reply, :ok, %{state | wm_cfg: cfg2, wm: wm2}}
  end

  @impl true
  def handle_call({:focus, cands_or_si, opts}, _from, state) do
    {wm_next, added, removed} = do_focus(state, cands_or_si, Map.new(opts))

    :telemetry.execute(
      [:brain, :wm, :update],
      %{size: length(wm_next), added: added, removed: removed, capacity: state.wm_cfg.capacity},
      %{}
    )

    {:reply, wm_next, %{state | wm: wm_next}}
  end

  @impl true
  def handle_call({:defocus, id_or_fun}, _from, state) do
    {wm2, removed} = WorkingMemory.remove(state.wm, id_or_fun)

    :telemetry.execute(
      [:brain, :wm, :update],
      %{size: length(wm2), added: 0, removed: removed, capacity: state.wm_cfg.capacity},
      %{reason: :defocus}
    )

    {:reply, :ok, %{state | wm: wm2}}
  end

  @impl true
  def handle_call(:snapshot_wm, _from, state),
    do: {:reply, %{wm: state.wm, cfg: state.wm_cfg, attention: state.attention}, state}

  @impl true
  def handle_call({:lifg_stage1, si_or_cands, _ctx_vec, opts}, _from, state) do
    t0 = System.monotonic_time()
    now_ms = System.system_time(:millisecond)

    # Build inputs for LIFG
    candidates = LIFGInput.lifg_candidates!(si_or_cands)
    tokens0 = Tokens.extract_tokens(si_or_cands, candidates)
    slate = LIFGInput.slate_for(si_or_cands)

    lifg_opts =
      [scores: :all, normalize: :softmax, parallel: :auto, margin_threshold: 0.12]
      |> Keyword.merge(opts)

    # CRITICAL: pass `:sense_candidates` so Stage-1 can score something
    si1 =
      %{tokens: tokens0, sense_candidates: slate, trace: []}
      |> LIFG.disambiguate_stage1(lifg_opts)

    {:ok, out0} = lifg_out_from_trace(si1)

    # Optional: Hygiene pass to compute :probs, :p_top1, dedup alt_ids, etc.
    out0 =
      case Brain.LIFG.Hygiene.run(%{}, out0.choices, []) do
        {:ok, %{choices: cleaned, audit: _}} -> %{out0 | choices: cleaned}
        _ -> out0
      end

    _ = maybe_ingest_atl(out0.choices, tokens0)
    _ = maybe_consult_pmtg(out0.choices, tokens0)
    _ = maybe_store_episode(tokens0, si1, out0)

    {boosts2, inhib2} = maybe_rescale_signals(out0, lifg_opts)
    apply_control_signals(boosts2, inhib2, lifg_opts)

    state1 = apply_decay(state, now_ms)

    state2 =
      if Keyword.get(opts, :gate_into_wm, false) do
        min =
          Keyword.get(opts, :lifg_min_score, Application.get_env(:brain, :lifg_min_score, 0.6))

        lifg_cands = LIFGGate.stage1_wm_candidates(out0.choices, now_ms, min)

        if lifg_cands == [] do
          state1
        else
          {wm_next, added, removed} = do_focus(state1, lifg_cands, %{})

          :telemetry.execute(
            [:brain, :wm, :update],
            %{
              size: length(wm_next),
              added: added,
              removed: removed,
              capacity: state1.wm_cfg.capacity
            },
            %{reason: :gate_from_lifg}
          )

          %{state1 | wm: wm_next}
        end
      else
        state1
      end

    state3 = evict_if_needed(state2)

    ms = System.convert_time_unit(System.monotonic_time() - t0, :native, :millisecond)

    :telemetry.execute(
      [:brain, :pipeline, :lifg_stage1, :stop],
      %{duration_ms: ms},
      %{
        winners: length(out0.choices),
        boosts: length(out0.boosts),
        inhibitions: length(out0.inhibitions)
      }
    )

    {:reply, {:ok, out0}, state3}
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

  # group all handle_cast/2 together
  @impl true
  def handle_cast({:set_attention, ctx}, state),
    do: {:noreply, %{state | attention: Map.new(ctx)}}

  @impl true
  def handle_cast({:activate_cells, rows_or_ids, payload}, state) do
    rows_or_ids
    |> extract_items()
    |> Enum.each(fn
      %Row{} = row -> ensure_start_and_cast(row, payload)
      id when is_binary(id) -> ensure_start_and_cast(id, payload)
      other -> Logger.warning("Brain.activate_cells: unknown item #{inspect(other)}")
    end)

    {:noreply, state}
  end

  @impl true
  def handle_cast({:activation_report, id, a}, state) when is_binary(id) and is_number(a) do
    {:noreply, %{state | active_cells: Map.put(state.active_cells, id, a)}}
  end

  @impl true
  def handle_cast({:cell, id, msg}, state) when is_binary(id) do
    case Registry.lookup(@registry, id) do
      [{pid, _}] -> GenServer.cast(pid, msg)
      [] -> :ok
    end

    {:noreply, state}
  end

  # ───────────────────────── Public helper: recall → WM ─────────────────────────
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
        # Fallback: gate the cues themselves into WM as :ltm
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

  # ───────────────────────── WM decay & eviction (τ-model) ─────────────────────
  def apply_decay(%{wm: wm} = state, now_ms) when is_list(wm) and is_integer(now_ms) do
    # FIX: treat stored nil as missing; avoid now_ms - nil crash
    last_raw = Map.get(state, :wm_last_ms, nil)
    last = if is_integer(last_raw), do: last_raw, else: now_ms
    dt = max(now_ms - last, 0)
    k = Numbers.decay_factor_ms(dt)

    wm2 =
      Enum.map(wm, fn
        %{score: s} = e when is_number(s) ->
          %{e | score: Numbers.clamp01(s * k)}

        e ->
          e
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

  # ───────────────────────── Centralized WM gating logic ───────────────────────
  defp do_focus(state, cands_or_si, opts) do
    now = System.system_time(:millisecond)
    cfg = state.wm_cfg
    attn = state.attention
    cands0 = normalize_candidates(cands_or_si)

    wm_d = WorkingMemory.decay(state.wm, now, cfg.decay_ms)

    {wm_next, added, removed} =
      Enum.reduce(cands0, {wm_d, 0, 0}, fn cand0, {wm_acc, a_cnt, r_cnt} ->
        sal = Attention.salience(cand0, attn)
        {decision, gate_score} = decide_gate(wm_acc, cand0, attn, cfg, Map.new(opts), sal)

        :telemetry.execute(
          [:brain, :gate, :decision],
          %{score: gate_score},
          %{decision: decision, source: Map.get(cand0, :source)}
        )

        case decision do
          :block ->
            {wm_acc, a_cnt, r_cnt}

          :allow ->
            item = WorkingMemory.normalize(cand0, now, activation: gate_score)
            {WorkingMemory.upsert(wm_acc, item, cfg), a_cnt + 1, r_cnt}

          :boost ->
            item = WorkingMemory.normalize(cand0, now, activation: min(gate_score + 0.2, 1.0))
            {WorkingMemory.upsert(wm_acc, item, cfg), a_cnt + 1, r_cnt}
        end
      end)
      |> then(fn {wm_tmp, a_cnt, r_cnt} ->
        wm_trim = WorkingMemory.trim(wm_tmp, cfg.capacity)
        {wm_trim, a_cnt, r_cnt + (length(wm_tmp) - length(wm_trim))}
      end)

    {wm_next, added, removed}
  end

  defp decide_gate(wm, cand, attn, cfg, opts, salience) do
    if Code.ensure_loaded?(Brain.BasalGanglia) and
         function_exported?(Brain.BasalGanglia, :decide, 4) do
      Brain.BasalGanglia.decide(wm, cand, attn, Map.merge(cfg, opts))
    else
      base = (cand[:score] || cand[:activation_snapshot] || 0.0) * 1.0
      bias = salience |> Kernel.max(0.0) |> Kernel.min(1.0)
      final = min(base + 0.5 * bias, 1.0)
      prefer_source? = cand[:source] in [:runtime, :recency, :lifg, :ltm]

      cond do
        prefer_source? and base >= cfg.gate_threshold -> {:boost, final}
        prefer_source? and final >= 0.20 -> {:boost, final}
        final >= cfg.gate_threshold -> {:allow, final}
        true -> {:block, final}
      end
    end
  end

  defp normalize_candidates(list) when is_list(list), do: list

  defp normalize_candidates(%{} = si) do
    cond do
      is_list(si[:active_cells]) -> si[:active_cells]
      is_list(si[:winners]) -> si[:winners]
      is_list(si[:tokens]) -> si[:tokens]
      true -> []
    end
  end

  # ───────────────────────── LIFG/ATL/PMTG/Episodes helpers ───────────────────
  defp lifg_out_from_trace(%{trace: [ev | _]}) do
    audit = Map.drop(ev, [:choices, :boosts, :inhibitions])
    {:ok, %{choices: ev.choices, boosts: ev.boosts, inhibitions: ev.inhibitions, audit: audit}}
  end

  defp lifg_out_from_trace(%{trace: []}),
    do:
      {:ok, %{choices: [], boosts: [], inhibitions: [], audit: %{stage: :lifg_stage1, groups: 0}}}

  defp maybe_ingest_atl(choices, tokens) do
    case Process.whereis(Brain.ATL) do
      nil -> :noop
      _pid -> Brain.ATL.ingest(choices, tokens)
    end
  end

  defp confidence_from_choices(choices) when is_list(choices) do
    choices
    |> Enum.map(&Map.get(&1, :margin, 0.0))
    |> Enum.reject(&is_nil/1)
    |> case do
      [] -> 1.0
      ms -> Enum.min(ms) * 1.0
    end
  end

  defp lifg_conf_threshold, do: Application.get_env(:brain, :lifg_conf_threshold, 0.18) * 1.0

  defp maybe_consult_pmtg(choices, tokens) do
    conf = confidence_from_choices(choices)
    :telemetry.execute([:brain, :lifg, :confidence], %{value: conf}, %{})

    needy_thr = Application.get_env(:brain, :pmtg_margin_threshold, 0.18)

    needy =
      Enum.filter(choices, fn ch ->
        m = Map.get(ch, :margin, 1.0)
        alts = Map.get(ch, :alt_ids, [])
        is_number(m) and m < needy_thr and is_list(alts) and length(alts) > 0
      end)

    should_consult? = conf < lifg_conf_threshold() or needy != []

    if should_consult? and is_pid(Process.whereis(Brain.PMTG)) do
      Brain.PMTG.consult(needy, tokens, already_needy: true, limit: 5, mode: :boost)
    else
      :ok
    end
  end

  defp maybe_store_episode(tokens, si1, out0) do
    mode =
      Application.get_env(:brain, :episodes_mode, :async)
      |> EpWriter.normalize_episode_mode()

    tags = Application.get_env(:brain, :episodes_tags, ["auto", "lifg"])

    si = %{
      tokens: tokens,
      lifg_choices:
        Enum.map(out0.choices, fn ch ->
          %{
            token_index: ch[:token_index],
            lemma: ch[:lemma],
            chosen_id: ch[:chosen_id],
            alt_ids: ch[:alt_ids] || [],
            margin: ch[:margin],
            scores: ch[:scores] || %{}
          }
        end),
      trace: si1.trace
    }

    EpWriter.store(si, tags, mode)
  end

  defp maybe_rescale_signals(%{choices: choices, boosts: b, inhibitions: i}, opts) do
    case Keyword.get(opts, :delta_model, :fixed) do
      :fixed ->
        {b, i}

      :margin_scaled ->
        base_boost = Keyword.get(opts, :base_boost, 0.2)
        base_inhib = Keyword.get(opts, :base_inhib, 0.1)
        clamp = fn x -> x |> min(0.5) |> max(-0.5) end

        boosts2 =
          for ch <- choices do
            m = ch[:margin] || Map.get(ch, :margin, 0.05)
            delta = clamp.(base_boost * max(m, 0.05))
            {ch[:chosen_id] || Map.get(ch, :chosen_id), delta}
          end

        inhib2 =
          choices
          |> Enum.flat_map(fn ch ->
            alts = ch[:alt_ids] || Map.get(ch, :alt_ids, [])
            m = ch[:margin] || Map.get(ch, :margin, 0.0)

            for aid <- alts do
              delta = clamp.(-base_inhib * max(0.2, 1.0 - m))
              {aid, delta}
            end
          end)

        {boosts2, inhib2}
    end
  end

  defp apply_control_signals(boosts, inhibitions, opts) do
    coalesce? = Keyword.get(opts, :coalesce, true)
    conc = Keyword.get(opts, :signal_concurrency, System.schedulers_online())
    delta_key = Keyword.get(opts, :delta_key, :delta)

    signals =
      boosts
      |> Kernel.++(inhibitions)
      |> then(fn pairs -> if coalesce?, do: ControlSignals.coalesce_pairs(pairs), else: pairs end)

    signals
    |> Task.async_stream(
      fn {id, delta} ->
        ensure_started(id, id)
        payload = %{delta_key => delta}
        GenServer.cast(via(id), {:activate, payload})
      end,
      max_concurrency: conc,
      timeout: :infinity
    )
    |> Stream.run()

    :ok
  end

  # ───────────────────────── Misc helpers ──────────────────────────────
  def coalesce_pairs(list), do: ControlSignals.coalesce_pairs(list)

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
          %{token_index: 0, lemma: String.downcase(w), score: 1.0}

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

  # Cell helpers
  def via(id) when is_binary(id), do: {:via, Registry, {@registry, id}}

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

  defp ensure_start_and_cast(%Row{id: id} = row, payload) do
    ensure_started(id, row)
    GenServer.cast(via(id), {:activate, payload})
    :ok
  end

  defp ensure_start_and_cast(id, payload) when is_binary(id) do
    ensure_started(id, id)
    GenServer.cast(via(id), {:activate, payload})
    :ok
  end

  defp ensure_started(id, arg) when is_binary(id) do
    case Registry.lookup(@registry, id) do
      [] ->
        case DynamicSupervisor.start_child(@cell_sup, {Brain.Cell, arg}) do
          {:ok, _pid} -> :ok
          {:error, {:already_started, _pid}} -> :ok
          other -> Logger.warning("Brain: start_child(#{id}) -> #{inspect(other)}")
        end

      _ ->
        :ok
    end
  end
end
