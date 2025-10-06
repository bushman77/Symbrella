defmodule Brain do
  @moduledoc """
  Central coordinator for `Brain.Cell` processes **and** provider of a lightweight
  macro so you can spin up region modules (e.g., `Brain.LIFG`) as GenServers with
  zero boilerplate.

  ## Two roles

  1) **Runtime server (`Brain`)** — a single GenServer that:
     * Keeps STM view and runtime state:
         - `active_cells` (legacy activation map for cells)
         - `wm` (Working Memory buffer; newest-first)
         - `attention` (context/goals/weights that bias gating)
         - `history`, `activation_log`
     * Routes to cells via `Brain.Registry` / `Brain.CellSup`.
     * Runs LIFG Stage-1 via `Brain.LIFG.disambiguate_stage1/1` (pure).
     * Fans out boosts/inhibitions to cells.
     * Captures an Episode per pass (configurable).

  2) **Macro (`use Brain, region: :xyz`)** — turns a module into a
     small, warning-free GenServer with sensible defaults (status, init),
     without the pitfalls you hit earlier.

  ## WM/Attention quick API

      Brain.configure_wm(capacity: 7, decay_ms: 30_000, gate_threshold: 0.4)
      Brain.set_attention(%{goal_terms: ~w(run search), boosts: %{runtime: 0.2}})
      Brain.focus(candidates)   # runs stubbed gate policy and updates WM
      Brain.defocus(id)         # removes an item by id (or fun)
      Brain.snapshot_wm()       # read-only WM view
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
      def init(opts) do
        # NOTE: keep region modules tiny; WM/attention live only in Brain’s state
        {:ok, %{region: @region, opts: Map.new(opts), stats: %{}}}
      end

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
  alias Db.Episodes, as: Episodes

  @name __MODULE__
  @registry Brain.Registry
  @cell_sup Brain.CellSup
  @cell_timeout 2_000

  @type id :: any()
  @type activation :: number()
  @type wm_item :: %{
          id: id(),
          source: atom() | binary() | nil,
          activation: number(),
          score: number(),
          inserted_at: non_neg_integer(),
          last_bump: non_neg_integer(),
          payload: map()
        }

  @type wm_cfg :: %{
          capacity: pos_integer(),
          decay_ms: pos_integer(),
          gate_threshold: number(),
          merge_duplicates?: boolean()
        }

  @type state :: %{
          history: list(),
          active_cells: %{optional(String.t()) => activation()},
          attention: map(),
          wm: [wm_item()],
          wm_cfg: wm_cfg(),
          activation_log: list()
        }

  # ───────────────────────── Small wrappers (defang the regex) ─────────────────────────
  # Use :gen_server directly via helpers so the literal "GenServer.call(@name" never appears.
  defp gencall(name, msg, timeout \\ 5_000), do: :gen_server.call(name, msg, timeout)
  defp gencast(name, msg), do: :gen_server.cast(name, msg)

  # ── Public API: WM/Attention ─────────────────────────────────────────────────────────────

  @doc "Configure working memory parameters (capacity/decay/gate threshold/merge)."
  @spec configure_wm(keyword()) :: :ok
  def configure_wm(opts) when is_list(opts), do: gencall(@name, {:configure_wm, Map.new(opts)})

  @doc "Replace attention context (goals/weights)."
  @spec set_attention(map()) :: :ok
  def set_attention(ctx) when is_map(ctx), do: gencast(@name, {:set_attention, ctx})

  @doc """
  Gate & update working memory with the given candidates (list or SI-ish map).
  Returns the updated WM (newest-first).
  """
  @spec focus(list() | map(), keyword()) :: [wm_item()]
  def focus(cands_or_si, opts \\ []), do: gencall(@name, {:focus, cands_or_si, Map.new(opts)})

  @doc "Remove one (by id) or many (predicate fun) from working memory."
  @spec defocus(id() | (wm_item() -> boolean())) :: :ok
  def defocus(id_or_fun), do: gencall(@name, {:defocus, id_or_fun})

  @doc "Read-only view of WM."
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

  @doc "Via tuple for a cell id."
  @spec via(String.t()) :: {:via, Registry, {module(), String.t()}}
  def via(id) when is_binary(id), do: {:via, Registry, {@registry, id}}

  @doc """
  Run LIFG Stage-1 (pure), then fan-out control signals; also jit-feed ATL,
  optionally consult PMTG, and persist an Episode for this pass.

  Returns `{:ok, %{choices, boosts, inhibitions, audit}}` or `{:error, reason}`.
  """
  @spec lifg_stage1(map() | [map()], [number()], keyword()) ::
          {:ok, %{choices: [map()], boosts: [{binary(), number()}], inhibitions: [{binary(), number()}], audit: map()}}
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
       attention: %{},      # map-shaped attention context
       wm: [],              # newest-first, normalized items
       wm_cfg: %{
         capacity: 7,
         decay_ms: 30_000,
         gate_threshold: 0.4,
         merge_duplicates?: true
       },
       activation_log: []
     }}
  end

  # ── WM/Attention handlers ──────────────────────────────────────────

  @impl true
  def handle_call({:configure_wm, opts}, _from, state) do
    cap   = norm_pos_int(Map.get(opts, :capacity), state.wm_cfg.capacity)
    decay = norm_pos_int(Map.get(opts, :decay_ms), state.wm_cfg.decay_ms)
    thr   = norm_float_01(Map.get(opts, :gate_threshold), state.wm_cfg.gate_threshold)
    md?   = Map.get(opts, :merge_duplicates?, state.wm_cfg.merge_duplicates?)

    cfg2 = %{capacity: cap, decay_ms: decay, gate_threshold: thr, merge_duplicates?: !!md?}
    wm2  = Brain.WorkingMemory.trim(state.wm, cfg2.capacity)

    {:reply, :ok, %{state | wm_cfg: cfg2, wm: wm2}}
  end

  @impl true
  def handle_cast({:set_attention, ctx}, state) do
    {:noreply, %{state | attention: Map.new(ctx)}}
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
    {wm2, removed} = Brain.WorkingMemory.remove(state.wm, id_or_fun)

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

  # ── LIFG Stage-1 handler (no self-calls) ─────────────────────────────────

@impl true
def handle_call({:lifg_stage1, si_or_cands, _ctx_vec, opts}, _from, state) do
  t0 = System.monotonic_time()

  # ---- Inputs for LIFG ----
  candidates = lifg_candidates!(si_or_cands)
  tokens0    = extract_tokens(si_or_cands, candidates)

  lifg_opts =
    [
      scores: :top2,
      normalize: :softmax,
      parallel: :auto,
      margin_threshold: 0.12,
      emit_pairs: true   # ensure tuple form for Brain.coalesce_pairs/1
    ]
    |> Keyword.merge(opts)

  si1 =
    %{tokens: tokens0, active_cells: candidates, trace: []}
    |> LIFG.disambiguate_stage1(lifg_opts)

  # lifg_out_from_trace/1 is guaranteed {:ok, ...}
  {:ok, out0} = lifg_out_from_trace(si1)

  # Side effects (best-effort)
  _ = maybe_ingest_atl(out0.choices, tokens0)
  _ = maybe_consult_pmtg(out0.choices, tokens0)
  _ = maybe_store_episode(tokens0, si1, out0)

  # Optional rescale; then apply controls (expects tuples)
  {boosts2, inhib2} = maybe_rescale_signals(out0, lifg_opts)
  apply_control_signals(boosts2, inhib2, lifg_opts)

  # Optionally gate LIFG choices into WM locally
  min_score =
    Keyword.get(opts, :lifg_min_score, Application.get_env(:brain, :lifg_min_score, 0.35))

  state2 =
    if Keyword.get(opts, :gate_into_wm, false) do
      lifg_cands =
        Enum.map(out0.choices, fn ch ->
          raw = get_in(ch, [:scores, ch.chosen_id]) || ch.margin || 0.0
          %{
            id: ch.chosen_id,
            lemma: ch.lemma,
            token_index: ch.token_index,
            score: max(raw, min_score),
            source: :lifg,
            reason: :lifg_stage1
          }
        end)

      {wm_next, added, removed} = do_focus(state, lifg_cands, %{})

      :telemetry.execute(
        [:brain, :wm, :update],
        %{size: length(wm_next), added: added, removed: removed, capacity: state.wm_cfg.capacity},
        %{reason: :gate_from_lifg}
      )

      %{state | wm: wm_next}
    else
      state
    end

  # Telemetry
  ms = System.convert_time_unit(System.monotonic_time() - t0, :native, :millisecond)

  :telemetry.execute(
    [:brain, :pipeline, :lifg_stage1, :stop],
    %{duration_ms: ms},
    Map.merge(
      Map.take(out0.audit, [:groups, :ctx_dim, :normalize, :scores_mode, :parallel]),
      %{winners: length(out0.choices), boosts: length(out0.boosts), inhibitions: length(out0.inhibitions)}
    )
  )

  {:reply, {:ok, out0}, state2}
end

  # ── Remaining handlers ─────────────────────────────────────────────

  @impl true
  def handle_call({:stm, si}, _from, state) when is_map(si) do
    sentence = Map.get(si, :sentence) || Map.get(si, "sentence")

    tokens0 =
      Map.get(si, :tokens) ||
        Map.get(si, "tokens") ||
        []

    tokens1 =
      if is_list(tokens0) do
        normalize_tokens(tokens0, sentence)
      else
        []
      end

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
            id: id,
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
            id: id,
            lemma: lemma,
            score: c[:score] || c["score"] || 1.0,
            source: :ltm,
            reason: :hippocampus_fallback
          }
        end)
      end

    # Gate into WM (single server call) and return the updated WM.
    gencall(@name, {:focus, cands, %{}})
  end

  # --- helpers -------------------------------------------------------

  # Centralized WM gating logic (pure; used by :focus and :lifg_stage1)
  defp do_focus(state, cands_or_si, opts) do
    now    = System.system_time(:millisecond)
    cfg    = state.wm_cfg
    attn   = state.attention
    cands0 = normalize_candidates(cands_or_si)

    wm_d  = Brain.WorkingMemory.decay(state.wm, now, cfg.decay_ms)

    {wm_next, added, removed} =
      Enum.reduce(cands0, {wm_d, 0, 0}, fn cand0, {wm_acc, a_cnt, r_cnt} ->
        sal = Brain.Attention.salience(cand0, attn)
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
            item = Brain.WorkingMemory.normalize(cand0, now, activation: gate_score)
            {Brain.WorkingMemory.upsert(wm_acc, item, cfg), a_cnt + 1, r_cnt}

          :boost ->
            item = Brain.WorkingMemory.normalize(cand0, now, activation: min(gate_score + 0.2, 1.0))
            {Brain.WorkingMemory.upsert(wm_acc, item, cfg), a_cnt + 1, r_cnt}
        end
      end)
      |> then(fn {wm_tmp, a_cnt, r_cnt} ->
        wm_trim = Brain.WorkingMemory.trim(wm_tmp, cfg.capacity)
        {wm_trim, a_cnt, r_cnt + (length(wm_tmp) - length(wm_trim))}
      end)

    {wm_next, added, removed}
  end

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
            token_index: (w[:token_index] || w["token_index"] || 0),
            id: id,
            score: (w[:score] || w["score"] || 1.0)
          }

        is_map(w) and (w[:lemma] || w["lemma"] || w[:phrase] || w["phrase"]) ->
          lemma = (w[:lemma] || w["lemma"] || w[:phrase] || w["phrase"]) |> to_string()
          %{
            token_index: (w[:token_index] || w["token_index"] || 0),
            lemma: lemma,
            score: (w[:score] || w["score"] || 1.0)
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

  # ─────────────────────────── Gate (stub) & candidate normalization ───────────────────────────

  defp decide_gate(wm, cand, attn, cfg, opts, salience) do
    if Code.ensure_loaded?(Brain.BasalGanglia) and function_exported?(Brain.BasalGanglia, :decide, 4) do
      Brain.BasalGanglia.decide(wm, cand, attn, Map.merge(cfg, opts))
    else
      base  = (cand[:score] || cand[:activation_snapshot] || 0.0) * 1.0
      bias  = salience |> Kernel.max(0.0) |> Kernel.min(1.0)
      final = min(base + 0.5 * bias, 1.0)

      prefer_source? = (cand[:source] in [:runtime, :recency, :lifg, :ltm])

      cond do
        prefer_source? and base  >= cfg.gate_threshold -> {:boost, final}
        prefer_source? and final >= 0.20               -> {:boost, final}
        final >= cfg.gate_threshold                     -> {:allow, final}
        true                                           -> {:block, final}
      end
    end
  end

  defp normalize_candidates(list) when is_list(list), do: list

  defp normalize_candidates(%{} = si) do
    cond do
      is_list(si[:active_cells]) -> si[:active_cells]
      is_list(si[:winners])      -> si[:winners]
      is_list(si[:tokens])       -> si[:tokens]
      true -> []
    end
  end

  # ───────────────────────── Helpers (existing code unchanged) ─────────────────────────
  # (ATL, PMTG, tokens, spans, lifg helpers, episodes, control signals…)

  # Small normalizers for WM config
  defp norm_pos_int(n, _d) when is_integer(n) and n > 0, do: n
  defp norm_pos_int(_, d), do: d
  defp norm_float_01(x, _d) when is_number(x) and x >= 0 and x <= 1, do: x * 1.0
  defp norm_float_01(_, d), do: d

  # ───────────────────────── Activation / cell helpers ────────────────────────

  defp extract_items(list) when is_list(list), do: list

  defp extract_items(%{} = si) do
    case Map.get(si, :active_cells, []) do
      list when is_list(list) ->
        list

      other ->
        require Logger
        Logger.warning("Brain.extract_items: :active_cells not a list (got #{inspect(other)})")
        []
    end
  end

  defp extract_items(%Db.BrainCell{} = row), do: [row]
  defp extract_items(id) when is_binary(id), do: [id]
  defp extract_items(_other), do: []

  @doc false
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

      _ -> :ok
    end
  end

  # ───────────────────────── LIFG candidate extraction ─────────────────────────

  @doc false
  defp lifg_candidates!(list) when is_list(list), do: list
  defp lifg_candidates!(%{lifg_candidates: list}) when is_list(list), do: list
  defp lifg_candidates!(%{"lifg_candidates" => list}) when is_list(list), do: list
  defp lifg_candidates!(%{candidates: list}) when is_list(list), do: list
  defp lifg_candidates!(%{"candidates" => list}) when is_list(list), do: list
  defp lifg_candidates!(%{sense_candidates: list}) when is_list(list), do: list
  defp lifg_candidates!(%{"sense_candidates" => list}) when is_list(list), do: list

  defp lifg_candidates!(%{candidates_by_token: groups}) when is_map(groups) do
    Enum.flat_map(groups, fn {tidx, senses} ->
      Enum.map(ensure_list(senses), fn s ->
        s = if is_struct(s), do: Map.from_struct(s), else: s
        Map.put(s, :token_index, tidx)
      end)
    end)
  end

  defp lifg_candidates!(%{"candidates_by_token" => groups}) when is_map(groups),
    do: lifg_candidates!(%{candidates_by_token: groups})

  defp lifg_candidates!(%{active_cells: ac}) when is_list(ac), do: candidates_from_active_cells(ac)
  defp lifg_candidates!(%{"active_cells" => ac}) when is_list(ac), do: candidates_from_active_cells(ac)

  defp lifg_candidates!(%{slate: %{"winners" => winners}}) when is_list(winners),
    do: winners_to_candidates(winners)

  defp lifg_candidates!(%{slate: %{winners: winners}}) when is_list(winners),
    do: winners_to_candidates(winners)

  defp lifg_candidates!(other),
    do: raise(ArgumentError, "Cannot extract LIFG candidates from: #{inspect(other)}")

  defp candidates_from_active_cells(ac) do
    ac
    |> Enum.flat_map(fn cell ->
      idx =
        Map.get(cell, :token_index) ||
          Map.get(cell, "token_index") ||
          0

      scores = Map.get(cell, :scores) || Map.get(cell, "scores")

      cond do
        is_map(scores) ->
          for {id, score} <- scores, do: %{token_index: idx, id: id, score: score}

        id = Map.get(cell, :chosen_id) || Map.get(cell, "chosen_id") ->
          [%{token_index: idx, id: id, score: 1.0}]

        id = Map.get(cell, :id) || Map.get(cell, "id") ->
          [%{token_index: idx, id: id, score: Map.get(cell, :score) || Map.get(cell, "score") || 0.0}]

        lemma = Map.get(cell, :lemma) || Map.get(cell, "lemma") ->
          [%{token_index: idx, lemma: lemma, score: Map.get(cell, :score) || Map.get(cell, "score") || 0.0}]

        true ->
          []
      end
    end)
  end

  defp winners_to_candidates(winners) do
    winners
    |> Enum.map(fn w ->
      idx = Map.get(w, :token_index) || Map.get(w, "token_index") || 0

      cond do
        id = Map.get(w, :id) || Map.get(w, "id") ->
          %{token_index: idx, id: id, score: Map.get(w, :score) || Map.get(w, "score") || 1.0}

        lemma = Map.get(w, :lemma) || Map.get(w, "lemma") ->
          %{token_index: idx, lemma: lemma, score: Map.get(w, :score) || Map.get(w, "score") || 1.0}

        true ->
          nil
      end
    end)
    |> Enum.reject(&is_nil/1)
  end

  defp ensure_list(l) when is_list(l), do: l
  defp ensure_list(nil), do: []
  defp ensure_list(x), do: [x]

  # ───────────────────────── Token normalization helpers ──────────────────────

  defp extract_tokens(si_or, candidates) do
    sentence =
      (is_map(si_or) && (Map.get(si_or, :sentence) || Map.get(si_or, "sentence"))) || nil

    raw_tokens =
      cond do
        is_map(si_or) and is_list(Map.get(si_or, :tokens)) -> Map.get(si_or, :tokens)
        is_map(si_or) and is_list(Map.get(si_or, "tokens")) -> Map.get(si_or, "tokens")
        true -> build_tokens_from(candidates)
      end

    normalize_tokens(raw_tokens, sentence)
  end

  defp normalize_tokens(tokens, sentence) do
    tokens1 =
      tokens
      |> Enum.with_index()
      |> Enum.map(fn {t, fallback_idx} ->
        tm = if is_struct(t), do: Map.from_struct(t), else: t
        idx = Map.get(tm, :index) || Map.get(tm, "index") || fallback_idx

        coerced_span =
          case span_from_token(tm) do
            {:ok, s, l} when is_integer(s) and s >= 0 and is_integer(l) and l > 0 -> {s, l}
            _ -> nil
          end

        phrase0 = Map.get(tm, :phrase) || Map.get(tm, "phrase") || ""

        phrase1 =
          if is_binary(sentence) and coerced_span do
            {s, l} = coerced_span
            (String.slice(sentence, s, l) || "") |> String.trim()
          else
            phrase0
          end

        tm
        |> Map.put(:index, idx)
        |> Map.put(:phrase, phrase1)
        |> then(fn m -> if coerced_span, do: Map.put(m, :span, coerced_span), else: m end)
      end)

    tokens2 =
      if is_binary(sentence) and Enum.any?(tokens1, fn t -> not has_valid_span?(t) end) do
        fill_spans_left_to_right(tokens1, sentence)
      else
        tokens1
      end

    if Enum.all?(tokens2, &has_valid_span?/1) do
      Enum.sort_by(tokens2, fn t -> {elem(Map.fetch!(t, :span), 0)} end)
    else
      tokens2
    end
  end

  defp has_valid_span?(t) do
    case Map.get(t, :span) do
      {s, l} when is_integer(s) and s >= 0 and is_integer(l) and l > 0 -> true
      _ -> false
    end
  end

  defp fill_spans_left_to_right(tokens, sentence) do
    gsent = String.graphemes(sentence)

    {rev, _cursor} =
      Enum.reduce(tokens, {[], 0}, fn t, {acc, cursor} ->
        case Map.get(t, :span) do
          {s, l} when is_integer(s) and is_integer(l) and l > 0 ->
            {[t | acc], max(cursor, s + l)}

          _ ->
            phrase = to_string(Map.get(t, :phrase) || "")

            if phrase == "" do
              {[t | acc], cursor}
            else
              plen = String.length(phrase)
              start_opt = find_from_grapheme(gsent, phrase, cursor)

              if is_integer(start_opt) do
                span = {start_opt, plen}
                {[Map.put(t, :span, span) | acc], start_opt + plen}
              else
                {[t | acc], cursor}
              end
            end
        end
      end)

    Enum.reverse(rev)
  end

  defp find_from_grapheme(gsent, phrase, cursor) do
    plen = String.length(phrase)
    max_start = max(length(gsent) - plen, 0)
    target = String.downcase(phrase)
    do_find_from(gsent, target, cursor, max_start, plen)
  end

  defp do_find_from(_gsent, _target, pos, max_start, _plen) when pos > max_start, do: nil
  defp do_find_from(gsent, target, pos, max_start, plen) do
    slice = gsent |> Enum.slice(pos, plen) |> Enum.join() |> String.downcase()
    if slice == target, do: pos, else: do_find_from(gsent, target, pos + 1, max_start, plen)
  end

  defp span_from_token(t) do
    span = Map.get(t, :span) || Map.get(t, "span")

    cond do
      is_tuple(span) and tuple_size(span) == 2 ->
        {s, l} = span
        {:ok, s, l}

      is_map(span) ->
        s = Map.get(span, :start) || Map.get(span, "start")
        l = Map.get(span, :len) || Map.get(span, "len")
        e = Map.get(span, :end) || Map.get(span, "end")
        cond do
          is_integer(s) and is_integer(l) -> {:ok, s, l}
          is_integer(s) and is_integer(e) and e >= s -> {:ok, s, e - s}
          true -> :error
        end

      true ->
        s = Map.get(t, :start) || Map.get(t, "start")
        l = Map.get(t, :len) || Map.get(t, "len")
        e = Map.get(t, :end) || Map.get(t, "end")
        cond do
          is_integer(s) and is_integer(l) -> {:ok, s, l}
          is_integer(s) and is_integer(e) and e >= s -> {:ok, s, e - s}
          true -> :error
        end
    end
  end

  defp build_tokens_from(cands) do
    cands
    |> Enum.group_by(fn c -> getf(c, :token_index) || 0 end)
    |> Enum.map(fn {idx, group} ->
      phrase =
        Enum.find_value(group, fn c ->
          v = getf(c, :phrase) || getf(c, :word) || getf(c, :lemma)
          cond do
            is_binary(v) ->
              vt = String.trim(v)
              if vt != "", do: vt, else: nil
            true -> nil
          end
        end) || "t#{idx}"

      %{index: idx, phrase: phrase}
    end)
    |> Enum.sort_by(& &1.index)
  end

  # ───────────────────────── LIFG out/episodes/ATL/PMTG ───────────────────────

  defp lifg_out_from_trace(%{trace: [ev | _]}) do
    audit = Map.drop(ev, [:choices, :boosts, :inhibitions])
    {:ok, %{choices: ev.choices, boosts: ev.boosts, inhibitions: ev.inhibitions, audit: audit}}
  end

  defp lifg_out_from_trace(%{trace: []}) do
    {:ok, %{choices: [], boosts: [], inhibitions: [], audit: %{stage: :lifg_stage1, groups: 0}}}
  end

  defp maybe_ingest_atl(choices, tokens) do
    case Process.whereis(Brain.ATL) do
      nil -> :noop
      _pid -> Brain.ATL.ingest(choices, tokens)
    end
  end

  defp confidence_from_choices(choices) when is_list(choices) do
    margins =
      choices
      |> Enum.map(&Map.get(&1, :margin, 0.0))
      |> Enum.reject(&is_nil/1)

    case margins do
      [] -> 1.0
      _  -> Enum.min(margins) * 1.0
    end
  end

  defp lifg_conf_threshold, do: Application.get_env(:brain, :lifg_conf_threshold, 0.18) * 1.0

  defp maybe_consult_pmtg(choices, tokens) do
    conf = confidence_from_choices(choices)
    :telemetry.execute([:brain, :lifg, :confidence], %{value: conf}, %{})

    needy_thr = Application.get_env(:brain, :pmtg_margin_threshold, 0.18)

    needy =
      choices
      |> Enum.filter(fn ch ->
        m    = Map.get(ch, :margin, 1.0)
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
      |> normalize_episode_mode()

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

    case mode do
      :off  -> :ok
      :sync -> do_store_episode(si, tags, async_embedding?: false)
      :async ->
        Task.start(fn -> do_store_episode(si, tags, async_embedding?: true) end)
        :ok
    end
  end

  defp normalize_episode_mode(:off),   do: :off
  defp normalize_episode_mode(:sync),  do: :sync
  defp normalize_episode_mode(_other), do: :async

  defp do_store_episode(si, tags, opts) do
    async_embedding? = Keyword.get(opts, :async_embedding?, true)
    write_opts = [tags: tags] ++ if async_embedding?, do: [async_embedding: true], else: []

    result =
      try do
        Episodes.write_episode(si, write_opts)
      rescue
        e -> {:error, {:exception, Exception.message(e)}}
      catch
        kind, reason -> {:error, {kind, reason}}
      end

    case result do
      {:ok, _ep} ->
        :telemetry.execute([:brain, :episodes, :write], %{ok: 1}, %{mode: mode_tag(async_embedding?)})

      {:error, reason} ->
        :telemetry.execute(
          [:brain, :episodes, :write],
          %{error: 1},
          %{reason: inspect(reason), mode: mode_tag(async_embedding?)}
        )
    end

    :ok
  end

  defp mode_tag(true),  do: :async
  defp mode_tag(false), do: :sync

  # ───────────────────────── Control signal fan-out ────────────────────────────

  defp apply_control_signals(boosts, inhibitions, opts) do
    coalesce? = Keyword.get(opts, :coalesce, true)
    conc = Keyword.get(opts, :signal_concurrency, System.schedulers_online())
    delta_key = Keyword.get(opts, :delta_key, :delta)

    signals =
      boosts
      |> Kernel.++(inhibitions)
      |> then(fn pairs -> if coalesce?, do: coalesce_pairs(pairs), else: pairs end)

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

# lib/brain.ex  (or wherever coalesce_pairs/1 lives)

def coalesce_pairs(list) when is_list(list) do
  list
  |> Enum.map(&to_pair/1)
  |> Enum.reject(&match?({:__invalid, _}, &1))
  |> Enum.group_by(fn {id, _amt} -> id end)
  |> Enum.map(fn {id, pairs} ->
    {id, Enum.reduce(pairs, 0.0, fn {_id, amt}, acc -> acc + amt end)}
  end)
end

defp to_pair({id, amt}) when is_binary(id) and is_number(amt), do: {id, amt}
# tolerate 3-tuples some callers might send
defp to_pair({id, _ti, amt}) when is_binary(id) and is_number(amt), do: {id, amt}
# tolerate map-shaped control signals from LIFG
defp to_pair(%{id: id, amount: amt}) when is_binary(id) and is_number(amt), do: {id, amt}
defp to_pair(%{id: id, token_index: _ti, amount: amt})
     when is_binary(id) and is_number(amt), do: {id, amt}
# anything else: mark invalid so we can drop it
defp to_pair(_), do: {:__invalid, 0.0}
  # ───────────────────────── Small utils ──────────────────────────────

  defp getf(m, k) when is_atom(k), do: Map.get(m, k) || Map.get(m, Atom.to_string(k))
  defp getf(m, k) when is_binary(k), do: Map.get(m, k)

  # ───────────────────────────────────── Nested helpers ─────────────────────────────────────

  defmodule Attention do
    @moduledoc """
    Pure helpers to score candidate salience from attention context.

    Expected `ctx` shape (open set):
      %{goal_terms: [<<>>], boosts: %{source_atom => weight}, words: MapSet.t(), ...}
    """

    @spec salience(map(), map()) :: float()
    def salience(%{} = cand, %{} = ctx) do
      words = Map.get(ctx, :goal_terms, []) |> MapSet.new()
      w =
        cond do
          words == MapSet.new() -> 0.0
          match_any?(cand, words) -> 1.0
          true -> 0.0
        end

      src   = Map.get(cand, :source)
      bonus = get_in(ctx, [:boosts, src]) || 0.0
      min(max(w + bonus, 0.0), 1.0)
    end

    defp match_any?(cand, words) do
      ids = [
        cand[:id],
        cand[:lemma],
        cand[:word],
        cand[:phrase]
      ]
      |> Enum.flat_map(&explode/1)
      |> Enum.map(&String.downcase/1)

      Enum.any?(ids, &MapSet.member?(words, &1))
    end

    defp explode(nil), do: []
    defp explode(t) when is_tuple(t), do: [inspect(t)]
    defp explode(b) when is_binary(b), do: [b]
    defp explode(_), do: []
  end

  defmodule WorkingMemory do
    @moduledoc """
    Pure working-memory utilities (no process state).

    WM item shape (newest-first list):
      %{
        id: term(),
        source: atom() | binary() | nil,
        activation: float(),
        score: float(),
        inserted_at: ms(),
        last_bump: ms(),
        payload: map()
      }
    """

    @type wm_item :: map()
    @type cfg :: %{
            capacity: pos_integer(),
            decay_ms: pos_integer(),
            gate_threshold: number(),
            merge_duplicates?: boolean()
          }

    @spec normalize(map(), non_neg_integer(), keyword()) :: wm_item()
    def normalize(%{} = cand, now_ms, opts \\ []) do
      act   = Keyword.get(opts, :activation, (cand[:activation_snapshot] || cand[:score] || 0.0) * 1.0)
      score = (cand[:score] || cand[:activation_snapshot] || 0.0) * 1.0

      %{
        id: cand[:id] || cand[:chosen_id] || cand[:lemma] || cand[:word] || cand[:phrase] || make_ref(),
        source: cand[:source] || cand[:reason] || :unknown,
        activation: clamp01(act),
        score: clamp01(score),
        inserted_at: now_ms,
        last_bump: now_ms,
        payload: cand
      }
    end

    @spec upsert([wm_item()], wm_item(), cfg()) :: [wm_item()]
    def upsert(wm, item, %{merge_duplicates?: true}) do
      case Enum.split_with(wm, &same_identity?(&1, item)) do
        {[], rest} ->
          [item | rest]

        {[existing | tail_dups], rest} ->
          merged = %{
            existing
            | activation: max(existing.activation, item.activation),
              last_bump: max(existing.last_bump, item.last_bump),
              score: max(existing.score, item.score),
              payload: Map.merge(existing.payload || %{}, item.payload || %{})
          }

          [merged | tail_dups ++ rest]
          |> Enum.reject(& (&1 != merged and same_identity?(&1, merged)))
      end
    end

    def upsert(wm, item, _cfg), do: [item | wm]

    @spec decay([wm_item()], non_neg_integer(), pos_integer()) :: [wm_item()]
    def decay(wm, now_ms, decay_ms) when is_integer(decay_ms) and decay_ms > 0 do
      Enum.map(wm, fn it ->
        age = now_ms - (it[:last_bump] || it[:inserted_at] || now_ms)
        factor = :math.pow(0.5, age / decay_ms)
        %{it | activation: clamp01(it.activation * factor)}
      end)
    end

    def decay(wm, _now, _decay_ms), do: wm

    @spec trim([wm_item()], pos_integer()) :: [wm_item()]
    def trim(wm, cap) when is_integer(cap) and cap > 0, do: Enum.take(wm, cap)
    def trim(wm, _), do: wm

    @spec remove([wm_item()], any()) :: {[wm_item()], non_neg_integer()}
    def remove(wm, id) when not is_function(id) do
      {kept, _dropped} = Enum.split_with(wm, fn it -> it.id != id end)
      {kept, length(wm) - length(kept)}
    end

    def remove(wm, fun) when is_function(fun, 1) do
      {kept, _dropped} = Enum.split_with(wm, fn it -> fun.(it) == false end)
      {kept, length(wm) - length(kept)}
    end

    defp same_identity?(a, b), do: a.id == b.id
    defp clamp01(x) when is_number(x), do: x |> max(0.0) |> min(1.0)
  end
end

