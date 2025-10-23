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

  # ───────────────────────── Region Macro ─────────────────────────

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

  # ───────────────────────── Brain Server ─────────────────────────

  use GenServer
  require Logger

  alias Db.BrainCell, as: Row
  alias Brain.ACC
  alias Brain.Attention
  alias Brain.Episodes.Writer, as: EpWriter
  alias Brain.LIFG
  alias Brain.LIFG.Gate, as: LIFGGate
  alias Brain.LIFG.Input, as: LIFGInput
  alias Brain.Utils.ControlSignals
  alias Brain.Utils.Numbers
  alias Brain.Utils.Tokens
  alias Brain.WorkingMemory

  @name __MODULE__
  @registry Brain.Registry
  @cell_sup Brain.CellSup
  @cell_timeout 2_000

  # Telemetry constants
  @wm_update_event [:brain, :wm, :update]
  @pipeline_stop_event [:brain, :pipeline, :lifg_stage1, :stop]

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
    fallback_scale: 0.70
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
          fallback_scale: number()
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

  # ── Small wrappers (hide raw GenServer.* from call-sites) ──────────────────

  defp gencall(name, msg, timeout \\ 5_000), do: :gen_server.call(name, msg, timeout)
  defp gencast(name, msg), do: :gen_server.cast(name, msg)

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
     last_intent: nil     # ← NEW
   }}
end

  # group all handle_call/3 together

  @impl true
  def handle_call({:configure_wm, opts}, _from, state) do
    cfg2 =
      state.wm_cfg
      |> Map.merge(@wm_defaults) # guarantee missing keys are filled
      |> merge_wm_opts(opts)

    wm2 = WorkingMemory.trim(state.wm, cfg2.capacity)
    {:reply, :ok, %{state | wm_cfg: cfg2, wm: wm2}}
  end

  @impl true
  def handle_call({:focus, cands_or_si, opts}, _from, state) do
    {wm_next, added, removed} = do_focus(state, cands_or_si, Map.new(opts))
    emit_wm_update(state.wm_cfg.capacity, length(wm_next), added, removed)

    {:reply, wm_next, %{state | wm: wm_next}}
  end

  @impl true
  def handle_call({:defocus, id_or_fun}, _from, state) do
    {wm2, removed} = WorkingMemory.remove(state.wm, id_or_fun)
    emit_wm_update(state.wm_cfg.capacity, length(wm2), 0, removed, :defocus)
    {:reply, :ok, %{state | wm: wm2}}
  end

  @impl true
  def handle_call(:snapshot_wm, _from, state),
    do: {:reply, %{wm: state.wm, cfg: state.wm_cfg, attention: state.attention}, state}

  @impl true
  def handle_call({:lifg_stage1, si_or_cands, _ctx_vec, opts}, _from, state) do
    t0 = System.monotonic_time()
    now_ms = System.system_time(:millisecond)

    %{tokens: tokens0, slate: slate} = build_lifg_inputs(si_or_cands)

    lifg_opts =
      [scores: :all, normalize: :softmax, parallel: :auto, margin_threshold: 0.12]
      |> Keyword.merge(opts)

    # CRITICAL: pass `:sense_candidates` so Stage-1 can score something
    si1 =
      %{tokens: tokens0, sense_candidates: slate, trace: []}
      |> LIFG.disambiguate_stage1(lifg_opts)

    {:ok, out0} = lifg_out_from_trace(si1)

    out0 =
      case Brain.LIFG.Hygiene.run(%{}, out0.choices, []) do
        {:ok, %{choices: cleaned}} -> %{out0 | choices: cleaned}
        _ -> out0
      end

    # NEW: ACC conflict assessment (no-op if ACC region isn't started)
    _ = maybe_assess_acc(out0.choices, tokens0)

    _ = maybe_ingest_atl(out0.choices, tokens0)
    _ = maybe_consult_pmtg(out0.choices, tokens0)
    _ = maybe_store_episode(tokens0, si1, out0)

    {boosts2, inhib2} = maybe_rescale_signals(out0, lifg_opts)
    apply_control_signals(boosts2, inhib2, lifg_opts)

    state1 = apply_decay(state, now_ms)

    state2 =
      if Keyword.get(opts, :gate_into_wm, false) do
        min = Keyword.get(opts, :lifg_min_score, Application.get_env(:brain, :lifg_min_score, 0.6))
        lifg_cands = LIFGGate.stage1_wm_candidates(out0.choices, now_ms, min)

        if lifg_cands == [] do
          state1
        else
          {wm_next, added, removed} = do_focus(state1, lifg_cands, %{})
          emit_wm_update(state1.wm_cfg.capacity, length(wm_next), added, removed, :gate_from_lifg)
          %{state1 | wm: wm_next}
        end
      else
        state1
      end

    state3 = evict_if_needed(state2)

    :telemetry.execute(
      @pipeline_stop_event,
      %{duration_ms: System.convert_time_unit(System.monotonic_time() - t0, :native, :millisecond)},
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

@impl true
def handle_call(:latest_intent, _from, state),
  do: {:reply, state.last_intent, state}


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
        ensure_start_and_cast(row, payload)

      # NEW: accept plain maps from LIFG/lexicon/seed and handle smartly
      %{} = map_item ->
        handle_map_item_activation(map_item, payload)

      id when is_binary(id) ->
        ensure_start_and_cast(id, payload)

      other ->
        :telemetry.execute([:brain, :activate, :unknown_item], %{count: 1}, %{sample: other})
        Logger.debug("Brain.activate_cells: unknown item #{inspect(other)}")
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

@impl true
def handle_cast({:set_latest_intent, m}, state) do
  {:noreply, %{state | last_intent: normalize_intent_map(m)}}
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

  # ───────────────────────── WM decay & eviction (τ-model) ────────────────────

  def apply_decay(%{wm: wm} = state, now_ms) when is_list(wm) and is_integer(now_ms) do
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

  # ───────────────────────── Centralized WM gating logic ──────────────────────
defp normalize_intent_map(m) do
  intent0 = m[:intent] || m["intent"]
  intent =
    cond do
      is_atom(intent0) -> intent0
      is_binary(intent0) ->
        try do
          String.to_existing_atom(intent0)
        rescue
          _ -> :unknown
        end
      true -> :unknown
    end

  kw = m[:keyword] || m["keyword"] || ""
  conf0 = m[:confidence] || m["confidence"] || 0.0
  conf =
    cond do
      is_number(conf0) -> conf0 * 1.0
      is_binary(conf0) ->
        case Float.parse(conf0) do
          {f, _} -> f
          _ -> 0.0
        end
      true -> 0.0
    end

  at_ms = m[:at_ms] || m["at_ms"] || System.system_time(:millisecond)

  %{intent: intent, keyword: to_string(kw), confidence: conf, at_ms: at_ms}
end



  defp do_focus(state, cands_or_si, _opts) do
    now = System.system_time(:millisecond)
    cfg = state.wm_cfg
    attn = state.attention
    cands0 = normalize_candidates(cands_or_si)

    wm_d = WorkingMemory.decay(state.wm, now, cfg.decay_ms)

    {wm_next, added, removed} =
      Enum.reduce(cands0, {wm_d, 0, 0}, fn cand0, {wm_acc, a_cnt, r_cnt} ->
        if not acceptable_candidate?(cand0, cfg) do
          {wm_acc, a_cnt, r_cnt}
        else
          sal = Attention.salience(cand0, attn)
          gate_score = gate_score_for(cand0, sal, cfg)
          decision = decide_gate_policy(wm_acc, cand0, gate_score, cfg)

          :telemetry.execute(
            [:brain, :gate, :decision],
            %{score: gate_score},
            %{decision: elem(decision, 0), source: Map.get(cand0, :source)}
          )

          case decision do
            {:block, _s} ->
              {wm_acc, a_cnt, r_cnt}

            {:allow, s} ->
              item = WorkingMemory.normalize(cand0, now, activation: s)
              {WorkingMemory.upsert(wm_acc, item, cfg), a_cnt + 1, r_cnt}

            {:boost, s} ->
              item = WorkingMemory.normalize(cand0, now, activation: min(s + 0.2, 1.0))
              {WorkingMemory.upsert(wm_acc, item, cfg), a_cnt + 1, r_cnt}
          end
        end
      end)
      |> then(fn {wm_tmp, a_cnt, r_cnt} ->
        wm_trim = WorkingMemory.trim(wm_tmp, cfg.capacity)
        {wm_trim, a_cnt, r_cnt + (length(wm_tmp) - length(wm_trim))}
      end)

    {wm_next, added, removed}
  end

  # Compute a robust gate score from candidate + salience + policy scalers.
defp gate_score_for(cand, salience, cfg) do
  base =
    (cand[:score] || cand[:activation_snapshot] || 0.0)
    |> Kernel.*(1.0)

  # Prefer runtime-ish sources; keep weights gentle.
  prefer = if cand[:source] in [:runtime, :recency, :lifg, :ltm], do: 0.10, else: 0.0

  # Downweight fallback phrase candidates a touch.
  b_scaled =
    if fallback_id?(cand[:id]) do
      scale = cfg[:fallback_scale] || 0.70
      base * scale
    else
      base
    end

  # diversity penalty against items for the same lemma with same (pos, frame)
  div_pen = diversity_penalty(cand, cfg)

  # NEW: Semantic bias from Brain.Semantics (optional and bounded)
  sem_bias =
    case Brain.Semantics.bias_for(cand) do
      b when is_number(b) -> Numbers.clamp01(b)
      _ -> 0.0
    end

  sem_boost = (cfg[:semantic_boost] || 0.1) * sem_bias

  b_scaled
  |> Kernel.+(0.5 * Numbers.clamp01(salience))
  |> Kernel.+(prefer)
  |> Kernel.+(sem_boost)
  |> Kernel.-(div_pen)
  |> Numbers.clamp01()
end

  defp decide_gate_policy(wm, cand, gate_score, cfg) do
    prefer_source? = cand[:source] in [:runtime, :recency, :lifg, :ltm]
    thr = cfg.gate_threshold

    # Per-lemma budget + hysteresis (don’t churn on tiny margins)
    {within_budget?, beats_by?} = within_lemma_budget?(wm, cand, cfg)

    cond do
      not within_budget? and not beats_by? ->
        {:block, gate_score}

      prefer_source? and gate_score >= thr ->
        {:boost, gate_score}

      prefer_source? and gate_score >= 0.20 ->
        {:boost, gate_score}

      gate_score >= thr ->
        {:allow, gate_score}

      true ->
        {:block, gate_score}
    end
  end

  # ── Admission helpers (policy; no hard deletes) ─────────────────────────────

  defp acceptable_candidate?(cand, cfg) do
    id = to_string(cand[:id] || "")
    pos = to_string(get_in(cand, [:pos]) || get_in(cand, [:features, :pos]) || "")

    allow_unk? = Map.get(cfg, :allow_unk?, true)
    allow_seed? = Map.get(cfg, :allow_seed?, true)

    cond do
      not allow_seed? and String.ends_with?(id, "|seed|") -> false
      not allow_unk? and String.contains?(String.downcase(pos), "unk") -> false
      true -> true
    end
  end

  defp fallback_id?(nil), do: false
  defp fallback_id?(id) when is_binary(id), do: String.ends_with?(id, "|phrase|fallback")
  defp fallback_id?(_), do: false

  defp diversity_penalty(cand, cfg) do
    λ = Map.get(cfg, :diversity_lambda, 0.06) |> Numbers.clamp01()
    pos = to_string(get_in(cand, [:pos]) || get_in(cand, [:features, :pos]) || "")
    frame = to_string(get_in(cand, [:frame]) || get_in(cand, [:features, :frame]) || "")
    if pos == "" and frame == "", do: 0.0, else: λ * 0.5
  end

  defp within_lemma_budget?(wm, cand, cfg) do
    lemma = to_string(cand[:lemma] || guess_lemma_from_id(cand[:id]) || "")
    budget = Map.get(cfg, :lemma_budget, 2)
    margin = Map.get(cfg, :replace_margin, 0.10)

    if lemma == "" or budget <= 0 do
      {true, true}
    else
      same = Enum.filter(wm, &(to_string(&1[:lemma] || "") == lemma))

      cond do
        length(same) < budget ->
          {true, true}

        true ->
          weakest = Enum.min_by(same, &Map.get(&1, :score, 0.0), fn -> nil end)

          beat? =
            if weakest do
              (cand[:score] || 0.0) >= Map.get(weakest, :score, 0.0) + margin
            else
              true
            end

          {false, beat?}
      end
    end
  end

  defp guess_lemma_from_id(nil), do: nil

  defp guess_lemma_from_id(id) when is_binary(id) do
    case String.split(id, "|", parts: 2) do
      [w | _] -> w
      _ -> nil
    end
  end

  defp guess_lemma_from_id(_), do: nil

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

  # ───────────────────────── Misc helpers ─────────────────────────────────────

  # Small de-dup helper for WM telemetry
  defp emit_wm_update(capacity, size, added, removed, reason \\ nil) do
    meta = if reason, do: %{reason: reason}, else: %{}
    :telemetry.execute(@wm_update_event, %{size: size, added: added, removed: removed, capacity: capacity}, meta)
  end

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
          %{token_index: w[:token_index] || w["token_index"] || 0, id: to_string(id), score: w[:score] || w["score"] || 1.0}

        is_map(w) and (w[:lemma] || w["lemma"] || w[:phrase] || w["phrase"]) ->
          lemma = (w[:lemma] || w["lemma"] || w[:phrase] || w["phrase"]) |> to_string()
          %{token_index: w[:token_index] || w["token_index"] || 0, lemma: lemma, score: w[:score] || w["score"] || 1.0}

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
  defp norm_float_01_or(x, _d) when is_number(x) and x >= 0 and x <= 1, do: x * 1.0
  defp norm_float_01_or(_, d), do: d

  # Cell helpers

  def via(id) when is_binary(id), do: {:via, Registry, {@registry, id}}

  defp extract_items(list) when is_list(list), do: list

  defp extract_items(%{} = si) do
    case Map.get(si, :active_cells, []) do
      list when is_list(list) -> list
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

  # NEW: handle map items (lexicon/seed/other)
  defp handle_map_item_activation(map_item, payload) do
    id = map_item[:id] || map_item["id"]
    type = map_item[:type] || map_item["type"]

    cond do
      is_binary(id) and (type == "lexicon" or type == :lexicon) ->
        :telemetry.execute([:brain, :activate, :lexicon_autohydrate], %{count: 1}, %{id: id})
        ensure_start_and_cast(id, payload)
        :ok

      is_binary(id) and (type == "seed" or type == :seed) ->
        # Seeds are not backed by DB; don't warn-spam. Emit telemetry and skip.
        :telemetry.execute([:brain, :activate, :seed_unknown], %{count: 1}, %{id: id})
        :ok

      is_binary(id) ->
        ensure_start_and_cast(id, payload)
        :ok

      true ->
        :telemetry.execute([:brain, :activate, :unknown_map], %{count: 1}, %{sample: map_item})
        Logger.debug("Brain.activate_cells: unhandled map item #{inspect(map_item)}")
        :ok
    end
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
          norm_float_01_or(Map.get(opts, :diversity_lambda, cfg.diversity_lambda), cfg.diversity_lambda),
        allow_unk?: Map.get(opts, :allow_unk?, cfg.allow_unk?),
        allow_seed?: Map.get(opts, :allow_seed?, cfg.allow_seed?),
        fallback_scale:
          norm_float_01_or(Map.get(opts, :fallback_scale, cfg.fallback_scale), cfg.fallback_scale)
    }
  end

  # ───────────────────────── Internal: LIFG input build ───────────────────────

  defp build_lifg_inputs(si_or_candidates) do
    candidates = LIFGInput.lifg_candidates!(si_or_candidates)
    tokens0 = Tokens.extract_tokens(si_or_candidates, candidates)
    slate = LIFGInput.slate_for(si_or_candidates)
    %{tokens: tokens0, slate: slate}
  end

  # ───────────────────────── Internal: Candidate normalization ────────────────

  defp normalize_candidates(list) when is_list(list), do: list

  defp normalize_candidates(%{} = si) do
    cond do
      is_list(si[:active_cells]) -> si[:active_cells]
      is_list(si[:winners]) -> si[:winners]
      is_list(si[:tokens]) -> si[:tokens]
      true -> []
    end
  end

  # ───────────────────────── ACC hook ─────────────────────────────────────────

# Accepts *anything* and never crashes the pipeline.
defp maybe_assess_acc(si, opts) do
  # Normalize opts: only keep keyword pairs if present
  opts_kw =
    if is_list(opts) and Keyword.keyword?(opts), do: opts, else: []

  # If SI isn't a map, just pass through with zero conflict
  unless is_map(si) do
    {si, 0.0}
  else
    res =
      try do
        Brain.ACC.assess(si, opts_kw)
      rescue
        _ -> {:ok, si}
      catch
        _, _ -> {:ok, si}
      end

    case res do
      # Preferred new shape
      {:ok, %{si: si2, conflict: c}} when is_map(si2) ->
        {Map.put(si2, :acc_conflict, to_float_01(c)), to_float_01(c)}

      # Legacy 3-tuple: {:ok, si2, meta}
      {:ok, si2, meta} when is_map(si2) and is_map(meta) ->
        c = meta[:conflict] || meta["conflict"] || Map.get(si2, :acc_conflict, 0.0)
        {Map.put(si2, :acc_conflict, to_float_01(c)), to_float_01(c)}

      # Legacy 2-tuple: {:ok, si2}
      {:ok, si2} when is_map(si2) ->
        c = Map.get(si2, :acc_conflict, 0.0)
        {Map.put(si2, :acc_conflict, to_float_01(c)), to_float_01(c)}

      # Anything else (including :error tuples): pass-through
      _ ->
        {si, Map.get(si, :acc_conflict, 0.0) |> to_float_01()}
    end
  end
end

defp to_float_01(x) when is_integer(x), do: x * 1.0 |> clamp01()
defp to_float_01(x) when is_float(x),   do: clamp01(x)
defp to_float_01(_),                    do: 0.0

defp clamp01(x) when is_number(x), do: max(0.0, min(1.0, x))
defp clamp01(_), do: 0.0

end

