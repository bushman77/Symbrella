defmodule Brain do
  @moduledoc """
  Central coordinator for `Brain.Cell` processes.

  Responsibilities:
    * Keep a lightweight STM view (`active_cells`, `attention`, `history`).
    * Provide a small API for activating cells in bulk or per-cell routing.
    * Lazily start `Brain.Cell` processes behind a `DynamicSupervisor`.
    * Surface `active_cells` into an incoming SI (Semantic Input) snapshot.
    * Run LIFG Stage-1 and fan-out boosts/inhibitions as side effects.

  Intentionally thin so downstream stages (e.g., LIFG orchestration) can depend on it without surprises.
  """

  use GenServer
  require Logger

  alias Db.BrainCell, as: Row
  alias Brain.LIFG

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

  # ─────────────────────────── Public API ───────────────────────────

  @spec start_link(term()) :: GenServer.on_start()
  def start_link(_args), do: GenServer.start_link(__MODULE__, :ok, name: @name)

  @doc """
  Blocking: merge a short-term memory snapshot into the given SI map.
  Returns the SI with `:active_cells` populated from Brain's current state.
  """
  @spec stm(map()) :: map()
  def stm(si) when is_map(si), do: GenServer.call(@name, {:stm, si})

  @doc """
  Non-blocking: activate a batch of rows or ids with a given payload.

      Brain.activate_cells(["id1","id2"], %{delta: +1})
      Brain.activate_cells(si, %{delta: -0.25})  # when si has :active_cells
  """
  @spec activate_cells([Row.t() | id()] | map(), map()) :: :ok
  def activate_cells(rows_or_ids, payload \\ %{delta: 1}) when is_map(payload) do
    GenServer.cast(@name, {:activate_cells, rows_or_ids, payload})
  end

  @doc "Route a synchronous call to a single cell (e.g., `:status`)."
  @spec cell_status(id()) :: {:ok, term()} | {:error, :not_found}
  def cell_status(id) when is_binary(id),
    do: GenServer.call(@name, {:cell, id, :status})

  @doc "Route an async cast to a single cell."
  @spec cell_cast(id(), term()) :: :ok
  def cell_cast(id, msg) when is_binary(id),
    do: GenServer.cast(@name, {:cell, id, msg})

  @doc "State snapshot (debug/testing)."
  @spec snapshot() :: state()
  def snapshot, do: GenServer.call(@name, :snapshot)

  @doc "Via tuple for a cell id, using `Brain.Registry`."
  @spec via(id()) :: {:via, Registry, {module(), id()}}
  def via(id) when is_binary(id), do: {:via, Registry, {@registry, id}}

  @doc """
  Run LIFG Stage-1 and apply control signals as a side effect.

  `si_or_candidates` may be:
    * a flat list of candidate maps
    * a map with `:lifg_candidates` or `:candidates`
    * a map with `:candidates_by_token` (token_index => [cands])

  Returns `{:ok, lifg_result}` with `:choices`, `:boosts`, `:inhibitions`, `:audit`.
  """
  @spec lifg_stage1(map() | [map()], [number()], keyword()) ::
          {:ok,
           %{
             choices: [map()],
             boosts: [{binary(), number()}],
             inhibitions: [{binary(), number()}],
             audit: map()
           }}
  def lifg_stage1(si_or_candidates, context_vec, opts \\ [])
      when is_list(context_vec) do
    GenServer.call(@name, {:lifg_stage1, si_or_candidates, context_vec, opts}, :infinity)
  end

  # ─────────────────────────── GenServer ────────────────────────────

  @impl true
  @spec init(:ok) :: {:ok, state()}
  def init(:ok) do
    {:ok, %{history: [], active_cells: %{}, attention: MapSet.new(), activation_log: []}}
  end

  @impl true
  def handle_call({:lifg_stage1, si_or_cands, _context_vec, opts}, _from, state) do
    candidates = lifg_candidates!(si_or_cands)

    lifg_opts =
      [scores: :top2, normalize: :softmax, parallel: :auto, margin_threshold: 0.12]
      |> Keyword.merge(opts)

    t0 = System.monotonic_time()

    # Prefer caller-provided tokens (keeps punctuation/emoji), else rebuild.
    tokens0 = extract_tokens(si_or_cands, candidates)

    # Minimal SI for the new LIFG /1 API (no :context_vec field on the struct)
    si0 = %{tokens: tokens0, active_cells: candidates, trace: []}
    si1 = LIFG.disambiguate_stage1(si0)

    with {:ok, out0} <- lifg_out_from_trace(si1) do
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

  # ─── handle_call :stm ───────────────────────────────────────────────

  @impl true
  def handle_call({:stm, si}, _from, state) when is_map(si) do
    sentence = Map.get(si, :sentence) || Map.get(si, "sentence")

    tokens0 =
      Map.get(si, :tokens) ||
        Map.get(si, "tokens") ||
        []

    tokens1 =
      if is_list(tokens0) do
        # ensure phrase + span; sort by start
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

  # ——— handle_cast ———

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

  # ─────────────────────────── Helpers ──────────────────────────────

  # Prefer the caller's tokens so token.phrase keeps punctuation/emoji intact.
  # If phrase is missing/placeholder, or if we *can* slice, we slice from sentence+span.
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
  # Preserve ALL existing fields (e.g., :mw), only augment.
  defp normalize_tokens(tokens, sentence) do
    # 1) Coerce shape, (re)hydrate phrase from sentence+span if present
    tokens1 =
      tokens
      |> Enum.with_index()
      |> Enum.map(fn {t, fallback_idx} ->
        tm = if is_struct(t), do: Map.from_struct(t), else: t

        idx =
          Map.get(tm, :index) ||
            Map.get(tm, "index") ||
            fallback_idx

        # Try to coerce span first (from many shapes)
        coerced_span =
          case span_from_token(tm) do
            {:ok, s, l} when is_integer(s) and s >= 0 and is_integer(l) and l > 0 -> {s, l}
            _ -> nil
          end

        phrase0 =
          Map.get(tm, :phrase) ||
            Map.get(tm, "phrase") ||
            ""

        # If we have a sentence and a valid span, slice phrase from it; otherwise keep provided phrase.
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

    # 2) If some spans are missing but we have sentence+phrase, derive spans left→right (grapheme-aware)
    tokens2 =
      if is_binary(sentence) and Enum.any?(tokens1, fn t -> not has_valid_span?(t) end) do
        fill_spans_left_to_right(tokens1, sentence)
      else
        tokens1
      end

    # 3) If all tokens now have spans, sort by start; otherwise keep original order
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

  # Return grapheme index of phrase within sentence graphemes at/after cursor; nil if not found.
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

  # Tiny field getter that works for maps & structs, atom or string keys.
  defp getf(m, k) when is_atom(k), do: Map.get(m, k) || Map.get(m, Atom.to_string(k))
  defp getf(m, k) when is_binary(k), do: Map.get(m, k)

  # ----- LIFG candidate extraction -----

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

  # ----- Control signal fan-out -----

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

  # ----- Activation utilities -----

  @doc false
  @spec extract_items([Row.t() | id()] | map()) :: [Row.t() | id()]
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

  @doc false
  @spec ensure_start_and_cast(Row.t(), map()) :: :ok
  defp ensure_start_and_cast(%Row{id: id} = row, payload) do
    ensure_started(id, row)
    GenServer.cast(via(id), {:activate, payload})
    :ok
  end

  @doc false
  @spec ensure_start_and_cast(id(), map()) :: :ok
  defp ensure_start_and_cast(id, payload) when is_binary(id) do
    ensure_started(id, id)
    GenServer.cast(via(id), {:activate, payload})
    :ok
  end

  @doc false
  @spec ensure_started(id(), Row.t() | id()) :: :ok
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

  # ----- LIFG /1 compat helpers -----

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

            true ->
              nil
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

  # Be liberal about span shape:
  #   %{span: %{start: s, len: l}} | %{span: %{start: s, end: e}} | %{span: {s, l}} |
  #   %{start: s, len: l} | %{start: s, end: e}
  defp span_from_token(t) do
    span = Map.get(t, :span) || Map.get(t, "span")

    cond do
      # tuple form {start, len}
      is_tuple(span) and tuple_size(span) == 2 ->
        {s, l} = span
        {:ok, s, l}

      # nested map: %{start:, len:} or %{start:, end:}
      is_map(span) ->
        s = Map.get(span, :start) || Map.get(span, "start")
        l = Map.get(span, :len) || Map.get(span, "len")
        e = Map.get(span, :end) || Map.get(span, "end")

        cond do
          is_integer(s) and is_integer(l) -> {:ok, s, l}
          is_integer(s) and is_integer(e) and e >= s -> {:ok, s, e - s}
          true -> :error
        end

      # flat keys on the token itself
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
end
