defmodule Brain do
  @moduledoc """
  Central coordinator for `Brain.Cell` processes **and** provider of a lightweight
  macro so you can spin up region modules (e.g., `Brain.LIFG`) as GenServers with
  zero boilerplate.

  ## Two roles

  1) **Runtime server (`Brain`)** — a single GenServer that:
     * Keeps STM view (`active_cells`, `attention`, `history`).
     * Routes to cells via `Brain.Registry` / `Brain.CellSup`.
     * Runs LIFG Stage-1 via `Brain.LIFG.disambiguate_stage1/1` (pure).
     * Fans out boosts/inhibitions to cells.
     * Captures an Episode per pass (configurable).

  2) **Macro (`use Brain, region: :xyz`)** — turns a module into a
     small, warning-free GenServer with sensible defaults (status, init),
     without the pitfalls you hit earlier (no bad `@impl`, no duplicated defaults).
  """

  # ───────────────────────── Region Macro (used by Brain.LIFG, Brain.PMTG, etc.) ─────────────────────────

  @doc """
  Use inside a module to make it a region GenServer:

      defmodule Brain.LIFG do
        use Brain, region: :lifg
        # optional overrides of init/1 or handlers, and public API
      end
  """
  defmacro __using__(opts) do
    region = Keyword.fetch!(opts, :region)

    quote location: :keep, bind_quoted: [region: region] do
      use GenServer

      @region region

      # Avoid default args here; prevents duplicate-default warnings downstream.
      def start_link(opts) do
        name = Keyword.get(opts, :name, __MODULE__)
        GenServer.start_link(__MODULE__, opts, name: name)
      end

      @doc false
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
      defoverridable init: 1

      @impl true
      def handle_call(:status, _from, state), do: {:reply, state, state}
      defoverridable handle_call: 3

      @impl true
      def handle_cast(_msg, state), do: {:noreply, state}
      defoverridable handle_cast: 2

      @impl true
      def handle_info(_msg, state), do: {:noreply, state}
      defoverridable handle_info: 2
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

  @type id :: String.t()
  @type activation :: number()
  @type state :: %{
          history: list(),
          active_cells: %{optional(id()) => activation()},
          attention: MapSet.t(),
          activation_log: list()
        }

  # ── Public API ──────────────────────────────────────────────────────

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
  def stm(si) when is_map(si), do: GenServer.call(@name, {:stm, si})

  @doc "Activate rows/ids asynchronously with a payload (e.g., `%{delta: +0.2}`)."
  @spec activate_cells([Row.t() | id()] | map(), map()) :: :ok
  def activate_cells(rows_or_ids, payload \\ %{delta: 1}) when is_map(payload) do
    GenServer.cast(@name, {:activate_cells, rows_or_ids, payload})
  end

  @doc "Sync call to one cell (e.g., `:status`); returns `{:ok, term}` or `{:error, :not_found}`."
  @spec cell_status(id()) :: {:ok, term()} | {:error, :not_found}
  def cell_status(id) when is_binary(id), do: GenServer.call(@name, {:cell, id, :status})

  @doc "Async cast to one cell."
  @spec cell_cast(id(), term()) :: :ok
  def cell_cast(id, msg) when is_binary(id), do: GenServer.cast(@name, {:cell, id, msg})

  @doc "Non-mutating snapshot for Web/debug."
  @spec snapshot() :: state()
  def snapshot, do: GenServer.call(@name, :snapshot)

  @doc "Via tuple for a cell id."
  @spec via(id()) :: {:via, Registry, {module(), id()}}
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
    GenServer.call(@name, {:lifg_stage1, si_or_candidates, context_vec, opts}, :infinity)
  end

  # ── GenServer callbacks ─────────────────────────────────────────────

  @impl true
  @spec init(:ok) :: {:ok, state()}
  def init(:ok) do
    {:ok,
     %{
       history: [],
       active_cells: %{},
       attention: MapSet.new(),
       activation_log: []
     }}
  end

  @impl true
  def handle_call({:lifg_stage1, si_or_cands, _context_vec, opts}, _from, state) do
    candidates = lifg_candidates!(si_or_cands)

    lifg_opts =
      [scores: :top2, normalize: :softmax, parallel: :auto, margin_threshold: 0.12]
      |> Keyword.merge(opts)

    t0 = System.monotonic_time()
    tokens0 = extract_tokens(si_or_cands, candidates)

    si0 = %{tokens: tokens0, active_cells: candidates, trace: []}
    si1 = LIFG.disambiguate_stage1(si0)

    with {:ok, out0} <- lifg_out_from_trace(si1) do
      # Feed ATL (best-effort) & gate PMTG by confidence + per-token margin
      _ = maybe_ingest_atl(out0.choices, tokens0)
      _ = maybe_consult_pmtg(out0.choices, tokens0)

      # Persist an episode for this pass (configurable: :async | :sync | :off)
      _ = maybe_store_episode(tokens0, si1, out0)

      {boosts2, inhib2} = maybe_rescale_signals(out0, lifg_opts)
      apply_control_signals(boosts2, inhib2, lifg_opts)

      ms = System.convert_time_unit(System.monotonic_time() - t0, :native, :millisecond)

      :telemetry.execute(
        [:brain, :pipeline, :lifg_stage1, :stop],
        %{duration_ms: ms},
        Map.merge(
          Map.take(out0.audit, [:groups, :ctx_dim, :normalize, :scores_mode, :parallel]),
          %{
            winners: length(out0.choices),
            boosts: length(out0.boosts),
            inhibitions: length(out0.inhibitions)
          }
        )
      )

      {:reply, {:ok, out0}, state}
    else
      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  @impl true
  def handle_call({:stm, si}, _from, state) when is_map(si) do
    sentence = Map.get(si, :sentence) || Map.get(si, "sentence")

    tokens0 =
      Map.get(si, :tokens) ||
        Map.get(si, "tokens") ||
        []

    tokens1 =
      if is_list(tokens0) do
        normalize_tokens(tokens0, sentence) # ensure phrase/span; sort by start when spans exist
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

  # ─────────────────────────── Pipeline helpers ───────────────────────────────

  # Best-effort ATL ingest (if ATL is running)
  defp maybe_ingest_atl(choices, tokens) do
    case Process.whereis(Brain.ATL) do
      nil -> :noop
      _pid -> Brain.ATL.ingest(choices, tokens)
    end
  end

  # Compute an utterance-level confidence from LIFG choices.
  # Strategy: use the minimum margin across tokens that had a choice.
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

  # Pull threshold from config or default.
  defp lifg_conf_threshold, do: Application.get_env(:brain, :lifg_conf_threshold, 0.18) * 1.0

  # If confidence is low and PMTG is running, consult asynchronously.
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
      # pass only the needy set; mark as already_needy to skip re-filtering
      Brain.PMTG.consult(needy, tokens, already_needy: true, limit: 5, mode: :boost)
    else
      :ok
    end
  end

  # ───────────────────────── Token normalization helpers ──────────────────────

  # Prefer caller tokens; otherwise build from candidates.
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

  # Canonicalize tokens to include :index, :phrase, and (when derivable) :span.
  # Preserve existing fields (e.g., :mw), only augment.
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
      Enum.sort_by(tokens2, fn t ->
        {s, _} = Map.fetch!(t, :span)
        s
      end)
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

  # Fill missing spans from sentence, scanning left→right (case-insensitive, grapheme-aware).
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

  # ───────────────────────── LIFG candidate extraction ─────────────────────────

  @doc false
  @spec lifg_candidates!(map() | [map()]) :: [map()]
  defp lifg_candidates!(list) when is_list(list), do: list
  defp lifg_candidates!(%{lifg_candidates: list}) when is_list(list), do: list
  defp lifg_candidates!(%{candidates: list}) when is_list(list), do: list

  defp lifg_candidates!(%{candidates_by_token: groups}) when is_map(groups) do
    Enum.flat_map(groups, fn {tidx, senses} ->
      Enum.map(senses, fn s ->
        s = if is_struct(s), do: Map.from_struct(s), else: s
        Map.put(s, :token_index, tidx)
      end)
    end)
  end

  defp lifg_candidates!(other),
    do: raise(ArgumentError, "Cannot extract LIFG candidates from: #{inspect(other)}")

  # ───────────────────────── Control signal fan-out ─────────────────────────

  @doc false
  @spec apply_control_signals([{binary(), number()}], [{binary(), number()}], keyword()) :: :ok
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

  @doc false
  @spec coalesce_pairs([{binary(), number()}]) :: [{binary(), number()}]
  defp coalesce_pairs(pairs) do
    pairs
    |> Enum.group_by(fn {id, _} -> id end, fn {_, d} -> d end)
    |> Enum.map(fn {id, ds} -> {id, Enum.sum(ds)} end)
  end

  # ───────────────────────── Activation / cell helpers ────────────────────────

  defp extract_items(list) when is_list(list), do: list

  defp extract_items(%{} = si) do
    case Map.get(si, :active_cells, []) do
      list when is_list(list) -> list
      other ->
        Logger.warning("Brain.extract_items: :active_cells not a list (got #{inspect(other)})")
        []
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

  # ───────────────────────── LIFG /1 compat helpers ───────────────────────────

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

  defp lifg_out_from_trace(%{trace: [ev | _]}) do
    audit = Map.drop(ev, [:choices, :boosts, :inhibitions])
    {:ok, %{choices: ev.choices, boosts: ev.boosts, inhibitions: ev.inhibitions, audit: audit}}
  end

  defp lifg_out_from_trace(%{trace: []}) do
    {:ok, %{choices: [], boosts: [], inhibitions: [], audit: %{stage: :lifg_stage1, groups: 0}}}
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
            delta = clamp.(base_boost * max(ch.margin, 0.05))
            {ch.chosen_id, delta}
          end

        inhib2 =
          choices
          |> Enum.flat_map(fn ch ->
            for aid <- ch.alt_ids do
              delta = clamp.(-base_inhib * max(0.2, 1.0 - ch.margin))
              {aid, delta}
            end
          end)

        {boosts2, inhib2}
    end
  end

  # ───────────────────────── Span helpers ───────────────────────────

  # Be liberal about span shape:
  #   %{span: %{start: s, len: l}} | %{span: %{start: s, end: e}} | %{span: {s, l}} |
  #   %{start: s, len: l} | %{start: s, end: e}
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

  # ───────────────────────── Episode capture ───────────────────────────

  # Build a minimal SI for storage. Keep it small & stable so historical rows are easy to use.
  defp episode_si(tokens, si1, out0) do
    %{
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
  end

  # Async by default, configurable. Does nothing if disabled.
  defp maybe_store_episode(tokens, si1, out0) do
    mode =
      Application.get_env(:brain, :episodes_mode, :async)
      |> normalize_episode_mode()

    tags = Application.get_env(:brain, :episodes_tags, ["auto", "lifg"])

    si = episode_si(tokens, si1, out0)

    case mode do
      :off ->
        :ok

      :sync ->
        do_store_episode(si, tags, async_embedding?: false)

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

  # ───────────────────────── Small utils ──────────────────────────────

  defp getf(m, k) when is_atom(k), do: Map.get(m, k) || Map.get(m, Atom.to_string(k))
  defp getf(m, k) when is_binary(k), do: Map.get(m, k)
end

