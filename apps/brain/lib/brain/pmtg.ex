# apps/brain/lib/brain/pmtg.ex
defmodule Brain.PMTG do
  @moduledoc """
  Posterior Middle Temporal Gyrus (pMTG) — controlled semantic retrieval.

  Role (modeled): when LIFG returns low-confidence choices (`needy`), pMTG plans
  retrieval queries to fetch clarifying evidence (e.g., DB rows, episodic context),
  then optionally boosts the current winners or re-runs LIFG with enriched evidence.

  Modes:
    • `:boost` (default) — bump `chosen_id` if any evidence found; softly inhibit alts
    • `:rerun`          — re-run `LIFG.Stage1` with a slate built from evidence
    • `:none`           — plan/record only (no side effects)

  Optional runtime config (override per-call via opts):

      config :brain,
        pmtg_mode: :boost,
        pmtg_margin_threshold: 0.15,
        pmtg_window_keep: 50

  Additional signals considered for "needy":
    • p(top1) < `:acc_p_min` (defaults to 0.65) when available
    • presence of alternatives (alt_ids != [])
  """

  use Brain, region: :pmtg

  @name __MODULE__

  @type si :: map()
  @type choice :: map()

  @typedoc "Retrieval query planned per needy choice"
  @type query :: %{
          required(:stage) => :pmtg,
          required(:token_index) => non_neg_integer(),
          required(:lemma) => String.t(),
          required(:chosen_id) => String.t() | nil,
          required(:alt_ids) => [String.t()],
          required(:limit) => pos_integer()
        }

  @typedoc "Evidence bundle returned by Episodes/Lexicon backends"
  @type evidence_item :: %{
          required(:token_index) => non_neg_integer(),
          required(:lemma) => String.t(),
          required(:chosen_id) => String.t() | nil,
          required(:alt_ids) => [String.t()],
          required(:episodes) => list(),
          required(:lexicon) => list()
        }

  # ───────────────────────────── Public API ─────────────────────────────

  @doc """
  Non-blocking consult. Pass raw LIFG `choices` or prefiltered `needy` (set `already_needy: true`).

  Options (selected):
    * `:already_needy` (bool) — if true, `list` is used as-is
    * `:limit` (pos_integer) — max episodes/lexicon entries per token (default: 5)
    * `:margin_threshold` (float) — low-margin threshold (default: app env)
    * `:p_min` (float) — p(top1) threshold (default: `:acc_p_min`)
    * `:mode` (:boost | :rerun | :none) — override server mode
    * `:boost`, `:inhib` — deltas for :boost mode (defaults 0.15 / -0.05)
  """
  @spec consult([choice()], [map()], keyword()) :: :ok
  def consult(list, tokens, opts \\ [])
      when is_list(list) and is_list(tokens) and is_list(opts) do
    GenServer.cast(@name, {:consult, list, tokens, opts})
  end

  @doc """
  Synchronous consult; returns result payload. Useful for tests or blocking flows.

  See `consult/3` for supported options.
  """
  @spec consult_sync([choice()], [map()], keyword()) ::
          {:ok,
           %{
             si: si(),
             queries: [query()],
             evidence: [evidence_item()],
             choices: [choice()],
             mode: atom(),
             rerun?: boolean()
           }}
  def consult_sync(list, tokens, opts \\ [])
      when is_list(list) and is_list(tokens) and is_list(opts) do
    GenServer.call(@name, {:consult_sync, list, tokens, opts})
  end

  @doc "Server status (state includes :last and rolling :window)."
  @spec status(pid() | module()) :: map()
  def status(server \\ @name), do: GenServer.call(server, :status)

  @doc "Update server-level defaults at runtime (e.g., mode/window_keep)."
  @spec configure(keyword()) :: :ok
  def configure(opts) when is_list(opts), do: GenServer.cast(@name, {:configure, opts})

  @doc "Clear rolling window and last snapshot."
  @spec reset() :: :ok
  def reset, do: GenServer.call(@name, :reset)

  # ─────────────────────── GenServer overrides/state ───────────────────────

  @impl true
  def init(opts) do
    keep = Keyword.get(opts, :window_keep, Application.get_env(:brain, :pmtg_window_keep, 50))
    mode = Keyword.get(opts, :mode, Application.get_env(:brain, :pmtg_mode, :boost))

    {:ok,
     %{
       region: :pmtg,
       opts: %{mode: mode, window_keep: keep} |> Map.merge(Map.new(opts)),
       window_keep: keep,
       # [{ts_ms, %{needy_count: n, queries: [...]}}]
       window: [],
       # %{si, queries, evidence, audit}
       last: nil
     }}
  end

  @impl true
  def handle_cast({:configure, opts}, state) do
    opts_map = Map.new(opts)
    keep = Map.get(opts_map, :window_keep, state.window_keep)
    mode = Map.get(opts_map, :mode, state.opts[:mode])

    {:noreply,
     state
     |> Map.put(:window_keep, keep)
     |> Map.put(:opts, Map.merge(state.opts, %{mode: mode} |> Map.merge(opts_map)))}
  end

  @impl true
  def handle_cast({:consult, list, tokens, opts}, state) do
    t0 = System.monotonic_time()

    %{need: need, si: si1, queries: queries, evidence: evidence, thr: thr, pmin: pmin, mode: mode} =
      plan_and_collect(list, tokens, opts, state)

    # Apply side-effects for async mode (no return payload)
    apply_mode(si1, need, list, evidence, mode, opts, emit_mode: :async)

    timing_ms = System.convert_time_unit(System.monotonic_time() - t0, :native, :millisecond)

    {last, window} = build_last_and_window(si1, queries, evidence, need, state, timing_ms)

    :telemetry.execute(
      [:brain, :pmtg, :consult],
      %{needy: length(need)},
      %{thr: thr, p_min: pmin, mode: mode}
    )

    {:noreply, %{state | last: last, window: window}}
  end

  @impl true
  def handle_call({:consult_sync, list, tokens, opts}, _from, state) do
    t0 = System.monotonic_time()

    %{need: need, si: si1, queries: queries, evidence: evidence, thr: thr, pmin: pmin, mode: mode} =
      plan_and_collect(list, tokens, opts, state)

    {choices, rerun?, si_final} =
      apply_mode(si1, need, list, evidence, mode, opts, emit_mode: :sync)

    timing_ms = System.convert_time_unit(System.monotonic_time() - t0, :native, :millisecond)

    {last, window} = build_last_and_window(si1, queries, evidence, need, state, timing_ms)

    :telemetry.execute(
      [:brain, :pmtg, :consult],
      %{needy: length(need)},
      %{thr: thr, p_min: pmin, mode: mode}
    )

    {:reply,
     {:ok,
      %{
        si: si_final,
        queries: queries,
        evidence: evidence,
        choices: choices,
        mode: mode,
        rerun?: rerun?
      }}, %{state | last: last, window: window}}
  end

  @impl true
  def handle_call(:reset, _from, state) do
    {:reply, :ok, %{state | last: nil, window: []}}
  end

  # Fallback to the macro’s default clauses (e.g., :status)
  @impl true
  def handle_call(other, from, state), do: super(other, from, state)

  # ─────────────────────────── Pure planning stage ───────────────────────────

  @doc """
  Plan retrieval for `needy` LIFG choices. Appends a `:pmtg` trace event.
  Returns `{:ok, %{si, queries, audit}}`.
  """
  @spec resolve(si(), [choice()], keyword()) ::
          {:ok, %{si: si(), queries: [query()], audit: map()}}
  def resolve(si, needy, opts \\ []) when is_map(si) and is_list(needy) do
    t0 = System.monotonic_time()

    queries =
      Enum.map(needy, fn ch ->
        %{
          stage: :pmtg,
          token_index: ch[:token_index],
          lemma: lemma_from_choice(ch, si),
          chosen_id: ch[:chosen_id],
          alt_ids: List.wrap(ch[:alt_ids] || []),
          limit: Keyword.get(opts, :limit, 5)
        }
      end)

    ev = %{
      stage: :pmtg,
      needy_count: length(needy),
      planned_queries: length(queries)
    }

    si2 = Map.update(si, :trace, [ev], fn tr -> [ev | tr] end)

    audit = %{
      stage: :pmtg,
      needy_count: ev.needy_count
    }

    timing_ms = System.convert_time_unit(System.monotonic_time() - t0, :native, :millisecond)
    {:ok, %{si: si2, queries: queries, audit: Map.put(audit, :timing_ms, timing_ms)}}
  end

  # ───────────────────── Evidence gathering (best-effort) ───────────────────

  @spec fetch_evidence([query()], [map()], keyword()) :: [evidence_item()]
  def fetch_evidence(queries, _tokens, _opts) when is_list(queries) do
    Enum.map(queries, fn q ->
      hits_eps = episodes_hits(q.lemma, q.limit)
      hits_lex = lexicon_hits(q.lemma, q.limit)

      %{
        token_index: q.token_index,
        lemma: q.lemma,
        chosen_id: q.chosen_id,
        alt_ids: q.alt_ids,
        episodes: hits_eps,
        lexicon: hits_lex
      }
    end)
  end

  # Backward-compat (arity-2)
  @spec fetch_evidence([query()], keyword()) :: [evidence_item()]
  def fetch_evidence(queries, opts) when is_list(queries) and is_list(opts) do
    fetch_evidence(queries, [], opts)
  end

  defp episodes_hits(nil, _limit), do: []
  defp episodes_hits("", _limit), do: []

  defp episodes_hits(lemma, limit) do
    mod = Module.concat([Db, Episodes])

    cond do
      Code.ensure_loaded?(mod) and function_exported?(mod, :find_by_lemma, 2) ->
        safe_call(fn -> mod.find_by_lemma(lemma, limit) end)

      Code.ensure_loaded?(mod) and function_exported?(mod, :search, 1) ->
        safe_call(fn -> mod.search(lemma) end) |> Enum.take(limit)

      true ->
        []
    end
  end

  defp lexicon_hits(nil, _limit), do: []
  defp lexicon_hits("", _limit), do: []

  defp lexicon_hits(lemma, limit) do
    mods = [Module.concat([Db, Lexicon]), Lexicon]

    case Enum.find(mods, &Code.ensure_loaded?/1) do
      nil ->
        []

      mod ->
        cond do
          function_exported?(mod, :lookup, 2) ->
            safe_call(fn -> mod.lookup(lemma, limit) end)

          function_exported?(mod, :definitions_for, 2) ->
            safe_call(fn -> mod.definitions_for(lemma, limit) end)

          true ->
            []
        end
    end
  end

  defp safe_call(fun) do
    try do
      case fun.() do
        list when is_list(list) -> list
        other -> List.wrap(other)
      end
    rescue
      _ -> []
    catch
      _, _ -> []
    end
  end

  # ─────────────── Sense compatibility (MWE vs unigram) ───────────────

  @doc """
  Filter lexicon candidates by token granularity.

  Ensures:
    * MWEs (`token.n > 1`) keep space-containing lemmas; unigrams prefer non-space lemmas.
    * Emits `[:brain, :pmtg, :no_mwe_senses]` when an MWE has no MWE senses (kept=0).
  """
  @spec enforce_sense_compatibility([evidence_item()], [map()]) :: [evidence_item()]
  def enforce_sense_compatibility(evidence, tokens)
      when is_list(evidence) and is_list(tokens) do
    tmap =
      Map.new(tokens, fn t ->
        {Map.get(t, :index) || Map.get(t, "index"), t}
      end)

    Enum.map(evidence, fn ev ->
      idx = Map.get(ev, :token_index) || Map.get(ev, "token_index")
      token = Map.get(tmap, idx, %{})
      lex = Map.get(ev, :lexicon) || Map.get(ev, "lexicon") || []

      {filtered, fallback_used?} = filter_lexicon_for_token(lex, token)

      ev
      |> Map.put(:lexicon, filtered)
      |> maybe_emit_no_mwe_senses(token, lex, filtered, fallback_used?)
    end)
  end

  defp filter_lexicon_for_token(lexicon, %{n: n}) when is_list(lexicon) and is_integer(n) do
    filtered =
      Enum.filter(lexicon, fn sense ->
        lemma = (Map.get(sense, :lemma) || Map.get(sense, "lemma") || "") |> to_string()
        has_space = String.contains?(lemma, " ")
        (n > 1 and has_space) or (n == 1 and not has_space)
      end)

    if n > 1 and filtered == [] do
      # No MWE senses available: keep originals but flag via telemetry as kept=0.
      {lexicon, true}
    else
      {filtered, false}
    end
  end

  defp filter_lexicon_for_token(lexicon, _token), do: {lexicon, false}

  defp maybe_emit_no_mwe_senses(ev, %{n: n} = token, orig, filtered, fallback?)
       when is_integer(n) and n > 1 and (filtered == [] or fallback?) do
    kept_count = if fallback?, do: 0, else: length(filtered)

    safe_exec_telemetry([:brain, :pmtg, :no_mwe_senses], %{
      orig: length(orig),
      kept: kept_count,
      token_index: Map.get(token, :index),
      phrase: Map.get(token, :phrase)
    })

    ev
  end

  defp maybe_emit_no_mwe_senses(ev, _token, _orig, _filtered, _fallback?), do: ev

  # ───────────────────────── Actions on evidence ─────────────────────────────

  defp do_boost(evidence, opts) do
    boost = Keyword.get(opts, :boost, 0.15) * 1.0
    inhib = Keyword.get(opts, :inhib, -0.05) * 1.0

    evidence
    |> Enum.each(fn ev ->
      has_hits = ev.episodes != [] or ev.lexicon != []

      if has_hits and is_binary(ev.chosen_id) do
        # boost winner
        Brain.cell_cast(ev.chosen_id, {:activate, %{delta: boost}})

        # softly inhibit alts (unique)
        ev.alt_ids
        |> List.wrap()
        |> Enum.uniq()
        |> Enum.each(fn aid ->
          Brain.cell_cast(aid, {:activate, %{delta: inhib}})
        end)
      end
    end)

    :ok
  end

  @doc false
  @spec do_rerun(si(), [evidence_item()], keyword()) ::
          {:ok, %{si: si(), choices: [choice()]}}
  defp do_rerun(si, evidence, opts) do
    # Build a slate from lexicon evidence with semantic fields preserved
    slate_from_ev =
      evidence
      |> Enum.group_by(& &1.token_index)
      |> Enum.into(%{}, fn {tidx, evs} ->
        senses =
          evs
          |> Enum.flat_map(fn ev -> List.wrap(ev.lexicon) end)
          |> Enum.uniq_by(fn s -> Map.get(s, :id) || Map.get(s, "id") || sense_key_fallback(s) end)
          |> Enum.with_index()
          |> Enum.map(fn {s, i} ->
            id = Map.get(s, :id) || Map.get(s, "id") || sense_key_fallback(s)
            lemma = Map.get(s, :lemma) || Map.get(s, "lemma") || ""
            pos = Map.get(s, :pos) || Map.get(s, "pos") || ""

            defn =
              Map.get(s, :gloss) || Map.get(s, "gloss") ||
                Map.get(s, :definition) || Map.get(s, "definition") || ""

            syns = Map.get(s, :synonyms) || Map.get(s, "synonyms") || []

            feats_from_sense = Map.get(s, :features) || %{}

            # tiny deterministic tiebreaker so top-1 has non-zero margin
            eps = 1.0e-6 * (1000 - i)

            base_feats = %{
              lex_fit: 0.60,
              # will get +eps below
              rel_prior: 0.50,
              activation: 0.0,
              intent_bias: 0.0,
              # carry semantics so Stage1 heuristics can work
              pos: pos,
              lemma: lemma,
              definition: defn,
              synonyms: syns
            }

            feats =
              base_feats
              # let explicit features from the sense win over base defaults
              |> Map.merge(feats_from_sense, fn _k, _base, from_sense -> from_sense end)
              # always add the epsilon even if rel_prior came from the sense
              |> Map.update(:rel_prior, 0.50 + eps, &(&1 + eps))
              # salutation nudge for interjection senses (hello/hi/hey)
              |> maybe_salutation_nudge(si, tidx)

            %{id: id, features: feats}
          end)

        {tidx, senses}
      end)

    si2 = Map.put(si, :sense_candidates, slate_from_ev)

    # Weights: Stage1 env/defaults + optional bump
    env =
      Application.get_env(:brain, :lifg_stage1_weights, %{
        lex_fit: 0.4,
        rel_prior: 0.3,
        activation: 0.2,
        intent_bias: 0.1
      })

    base = Map.merge(env, Map.new(Keyword.get(opts, :weights, [])))
    bump = Map.get(Map.new(opts), :rerun_weights_bump, %{})
    weights = Map.merge(base, bump, fn _k, a, b -> a + b end)

    {:ok, %{choices: choices}} =
      Brain.LIFG.Stage1.run(
        si2,
        weights: weights,
        chargram_event: [:brain, :pmtg, :chargram_violation],
        boundary_event: [:brain, :pmtg, :boundary_drop]
      )

    {:ok, %{si: si2, choices: choices}}
  end

  defp maybe_salutation_nudge(feats, si, tidx) when is_map(feats) do
    phrase =
      si
      |> Map.get(:tokens, [])
      |> Enum.at(tidx, %{})
      |> Map.get(:phrase, "")
      |> to_string()

    pos = (feats[:pos] || feats["pos"] || "") |> to_string() |> String.downcase()

    if Regex.match?(~r/^(hello|hi|hey)\b/i, phrase) and String.contains?(pos, "interjection") do
      Map.update(feats, :lex_fit, 0.63, &(&1 + 0.03))
    else
      feats
    end
  end

  # ─────────────────────────────── Helpers ───────────────────────────────

  # Shared consult logic for async/sync paths
  defp plan_and_collect(list, tokens, opts, state) do
    thr = margin_threshold(opts)
    pmin = p_min(opts)

    need =
      if Keyword.get(opts, :already_needy, false) do
        list
      else
        needy_from_choices(list, thr, pmin)
      end

    si0 = %{tokens: tokens, trace: []}

    {:ok, %{si: si1, queries: queries}} =
      resolve(si0, need, limit: Keyword.get(opts, :limit, 5))

    evidence =
      fetch_evidence(queries, si1.tokens, opts)
      |> enforce_sense_compatibility(si1.tokens)

    mode = opts[:mode] || state.opts[:mode] || Application.get_env(:brain, :pmtg_mode, :boost)

    %{
      need: need,
      si: si1,
      queries: queries,
      evidence: evidence,
      thr: thr,
      pmin: pmin,
      mode: mode
    }
  end

  defp apply_mode(si1, need, list, evidence, mode, opts, emit_mode: emit_mode) do
    case mode do
      :boost ->
        do_boost(evidence, opts)
        {need, false, si1}

      :rerun ->
        case do_rerun(si1, evidence, opts) do
          {:ok, %{si: si2, choices: rerun_choices}} ->
            emit_rerun_event(rerun_choices, emit_mode)
            {merge_choices(list, rerun_choices), true, si2}

          _ ->
            {need, false, si1}
        end

      :none ->
        {need, false, si1}

      _ ->
        {need, false, si1}
    end
  end

  defp build_last_and_window(si1, queries, evidence, need, state, timing_ms) do
    last = %{
      si: si1,
      queries: queries,
      evidence: evidence,
      audit: %{stage: :pmtg, timing_ms: timing_ms, needy_count: length(need)}
    }

    window =
      [
        {System.system_time(:millisecond), %{needy_count: length(need), queries: queries}}
        | state.window
      ]
      |> Enum.take(state.window_keep)

    {last, window}
  end

  # Determine needy items using margin, p(top1), and presence of alts
  defp needy_from_choices(choices, thr, pmin) do
    Enum.filter(choices, fn ch ->
      m = Map.get(ch, :margin, 1.0) * 1.0
      alts = Map.get(ch, :alt_ids, [])
      p1 = p_top1(ch)

      low_margin? = is_number(m) and m < thr * 1.0
      low_prob? = is_number(p1) and p1 < pmin * 1.0
      has_alts? = is_list(alts) and length(alts) > 0

      (low_margin? or low_prob?) and has_alts?
    end)
  end

  defp p_top1(ch) do
    chosen_id = Map.get(ch, :chosen_id)
    probs = Map.get(ch, :probs, %{})

    cond do
      is_binary(chosen_id) and is_map(probs) and is_number(Map.get(probs, chosen_id)) ->
        Map.get(probs, chosen_id) * 1.0

      is_number(Map.get(ch, :p_top1)) ->
        Map.get(ch, :p_top1) * 1.0

      is_map(Map.get(ch, :scores)) ->
        Map.get(ch, :scores)
        |> Map.values()
        |> Enum.max(fn -> 0.0 end)
        |> Kernel.*(1.0)

      true ->
        1.0
    end
  end

  defp margin_threshold(opts) do
    Keyword.get(
      opts,
      :margin_threshold,
      Application.get_env(:brain, :pmtg_margin_threshold, 0.15)
    )
    |> Kernel.*(1.0)
  end

  defp p_min(opts) do
    Keyword.get(opts, :p_min, Application.get_env(:brain, :acc_p_min, 0.65))
    |> Kernel.*(1.0)
  end

  defp lemma_from_choice(ch, si) do
    idx = ch[:token_index]

    with true <- is_integer(idx),
         tokens when is_list(tokens) <- Map.get(si, :tokens),
         %{} = tok <- Enum.at(tokens, idx),
         phrase when is_binary(phrase) <- Map.get(tok, :phrase) || Map.get(tok, "phrase") do
      phrase
    else
      _ -> ch[:lemma] || "?"
    end
  end

  # Merge rerun winners over a baseline (typically the original or needy list)
  defp merge_choices(baseline, rerun) do
    by_tok = Map.new(baseline, fn ch -> {ch.token_index, ch} end)

    Enum.reduce(rerun, by_tok, fn ch, acc -> Map.put(acc, ch.token_index, ch) end)
    |> Map.values()
    |> Enum.sort_by(& &1.token_index)
  end

  # Emit a pMTG :rerun event (2-arity only; no 1-arity wrapper)
  defp emit_rerun_event(choices, mode) do
    groups =
      choices
      |> Enum.map(& &1.token_index)
      |> MapSet.new()
      |> MapSet.size()

    # Telemetry: keep numeric fields in measurements; atoms/labels in meta
    :telemetry.execute(
      [:brain, :pmtg, :rerun],
      %{groups: groups, choices: length(choices)},
      %{mode: mode}
    )
  end

  # Ensure sense has a reproducible key when id is missing
  defp sense_key_fallback(s) do
    "#{Map.get(s, :lemma) || Map.get(s, "lemma") || "?"}|#{Map.get(s, :pos) || Map.get(s, "pos") || "?"}"
  end

  # --- Telemetry helpers (grouped; no duplicates, no unused 1-arity) ---

  # Some call sites pass only event + measurements
  defp safe_exec_telemetry(event, measurements),
    do: safe_exec_telemetry(event, measurements, %{})

  # Single implementation
  defp safe_exec_telemetry(event, measurements, meta) do
    if Code.ensure_loaded?(:telemetry) and function_exported?(:telemetry, :execute, 3) do
      :telemetry.execute(event, measurements, meta)
    else
      :ok
    end
  end
end

