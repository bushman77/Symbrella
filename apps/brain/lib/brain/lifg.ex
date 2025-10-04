defmodule Brain.LIFG do
  @moduledoc """
  **Left Inferior Frontal Gyrus (LIFG) — Competitive Sense Selection**

  Two entry points:
  • Legacy pure stage (kept for back-compat):
      `disambiguate_stage1/1,2` — scores candidates from slate or active_cells with optional context similarity.

  • New fast pipeline stage:
      `Brain.LIFG.Stage1.run/2` — per-token disambiguation from `si.sense_candidates` with guards and tiny heuristics.

  Central config (umbrella root `config/config.exs`):

      config :brain,
        lifg_stage1_weights: %{lex_fit: 0.40, rel_prior: 0.30, activation: 0.20, intent_bias: 0.10},
        lifg_stage1_scores_mode: :all,  # or :top2 | :none
        pmtg_mode: :boost,
        pmtg_margin_threshold: 0.15,
        pmtg_window_keep: 50
  """

  require Logger

  ## Region GenServer for status/config/telemetry (stage stays pure)
  use Brain, region: :lifg

  # ── Optional server API (convenience) ─────────────────────────────────
  def status(server \\ __MODULE__), do: GenServer.call(server, :status)

  def reload_config(new_opts, server \\ __MODULE__) when is_list(new_opts) or is_map(new_opts) do
    GenServer.cast(server, {:reload_config, Map.new(new_opts)})
  end

  @impl true
  def handle_call(:status, _from, state) do
    {:reply, state.opts || %{}, state}
  end

  @impl true
  def handle_cast({:reload_config, new_opts}, state) do
    {:noreply, %{state | opts: Map.merge(state.opts || %{}, new_opts)}}
  end

  # ── Legacy pure Stage-1 API ──────────────────────────────────────────

  @doc "Pure, stateless stage-1. Appends a trace event with choices/boosts/inhibitions."
  @spec disambiguate_stage1(map()) :: map()
  def disambiguate_stage1(si) when is_map(si), do: disambiguate_stage1(si, [])

  @doc """
  Pure stage-1 with options:
    * `:weights`           — override weights map (outer {lex, sim, rel, prag, act, prime})
    * `:margin_threshold`  — default 0.12
    * `:scores`            — :all | :top2 | :none (default :all)
    * `:fit?`              — (cand, si) -> boolean (default fn -> true end)
    * `:max_flips`         — reanalysis cap (default 1) [stubbed]
    * `:prime`             — priming options (stubbed; no-op)
  """
  @spec disambiguate_stage1(map(), keyword()) :: map()
  def disambiguate_stage1(%{} = si, opts) do
    try do
      # 1) Tripwire (stub)
      tokens =
        si
        |> Map.get(:tokens, [])
        |> ensure_list()
        |> tripwire_check_and_filter(on_leak: :drop)

      ctx = Map.get(si, :context_vec)
      ctx = if is_list(ctx), do: ctx, else: nil
      ctxn = if ctx, do: l2(ctx), else: 0.0

      # 2) Candidate source: prefer slate ; else from active_cells
      candidates =
        case Map.get(si, :sense_candidates) do
          %{} = slate -> candidates_from_slate(si, slate, tokens)
          _ -> candidates_from_cells(si, tokens)
        end

      # 3) Score + normalize per token group, pick winners (+ minimal reanalysis)
      weights = resolved_weights(opts)
      {choices, boosts, inhibitions} =
        disambiguate_groups(tokens, candidates, ctx, ctxn, weights, opts)

      # 4) Build trace event
      ev = %{
        stage: :lifg_stage1,
        ts_ms: System.system_time(:millisecond),
        normalize: :softmax,
        scores_mode: Keyword.get(opts, :scores, :all),
        ctx_dim: (ctx && length(ctx)) || 0,
        groups: candidates |> Enum.map(& &1.token_index) |> MapSet.new() |> MapSet.size(),
        choices: choices,
        boosts: boosts,
        inhibitions: inhibitions
      }

      trace = [ev | Map.get(si, :trace, [])]
      Map.put(si, :trace, trace)
    rescue
      e ->
        Logger.error("LIFG disambiguate_stage1 failed: #{inspect(e)}")
        ts = System.system_time(:millisecond)
        ev = %{stage: :lifg_stage1, error: Exception.format(:error, e, __STACKTRACE__), ts_ms: ts}
        trace = [ev | Map.get(si, :trace, [])]
        Map.put(si, :trace, trace)
    end
  end

  # Back-compat shim if callers pass (si, ctx, opts)
  @spec disambiguate_stage1(map(), any(), keyword()) :: map()
  def disambiguate_stage1(si, _ctx, opts), do: disambiguate_stage1(si, opts)

  # ── Candidate sources (legacy pure stage) ────────────────────────────

  defp candidates_from_slate(si, slate, tokens) when is_map(slate) and is_list(tokens) do
    Enum.flat_map(slate, fn {idx, list} ->
      lemma = lemma_for(tokens, idx)

      List.wrap(list)
      |> Enum.map(fn c ->
        feats = Map.get(c, :features, %{}) || %{}

        %{
          id: Map.get(c, :id),
          token_index: idx,
          lemma: lemma || "",
          pos: Map.get(feats, :pos),
          lex_fit: Map.get(feats, :lex_fit, 0.6),
          rel_prior: Map.get(c, :prior, 0.5),
          intent_bias: intent_bias_from(si, idx, feats),
          activation: Map.get(feats, :activation, 0.0),
          embedding: Map.get(feats, :embedding)
        }
      end)
      |> Enum.reject(&is_nil/1)
    end)
  end

  defp candidates_from_slate(_si, _slate, _tokens), do: []

  # ── FIXED: Strict MWE matching to prevent cross-token contamination ──
  defp candidates_from_cells(si, tokens) when is_map(si) and is_list(tokens) do
    cells = Map.get(si, :active_cells, []) |> ensure_list()

    # Pre-normalize cell info; no token index here yet.
    cells_normed =
      Enum.map(cells, fn m ->
        word  = getf(m, :word) || ""
        lemma = getf(m, :lemma) || word
        nrm   = getf(m, :norm)  || word

        %{
          id:           getf(m, :id),
          pos:          getf(m, :pos),
          lex_fit:      getf(m, :lex_fit),
          rel_prior:    getf(m, :rel_prior),
          intent_bias:  getf(m, :intent_bias),
          activation:   getf(m, :activation),
          embedding:    getf(m, :embedding),
          definition:   getf(m, :definition),
          __norm__:     norm_phrase(nrm),
          __lemma__:    lemma
        }
      end)

    tokens
    |> Enum.with_index()
    |> Enum.flat_map(fn {t, idx} ->
      tnorm = norm_phrase(Map.get(t, :phrase, ""))
      is_mwe = Map.get(t, :n, 1) > 1 or Map.get(t, :mw) in [true, "true"]

      cells_normed
      |> Enum.filter(fn c ->
        # STRICT matching for MWEs to prevent cross-token contamination
        if is_mwe do
          # For MWE tokens, ONLY exact match
          c.__norm__ == tnorm
        else
          # For single-word tokens, allow fuzzy matching
          c.__norm__ == tnorm or
            String.contains?(tnorm, c.__norm__) or
            String.contains?(c.__norm__, tnorm)
        end
      end)
      |> Enum.map(fn c ->
        %{
          id:          c.id,
          token_index: idx,
          lemma:       c.__lemma__ || "",
          pos:         c.pos,
          lex_fit:     c.lex_fit || 0.6,
          rel_prior:   c.rel_prior || 0.5,
          intent_bias: (c.intent_bias || 0.0) +
                         intent_bias_from(si, idx, %{pos: c.pos, definition: c.definition}),
          activation:  c.activation || 0.0,
          embedding:   c.embedding
        }
      end)
    end)
  end

  defp candidates_from_cells(_si, _tokens), do: []

  defp intent_bias_from(si, idx, feats_or_map) do
    tokens = Map.get(si, :tokens, [])
    cur    = Enum.at(tokens, idx) || %{}
    prev   = if idx > 0, do: Enum.at(tokens, idx - 1), else: %{}

    phrase = (cur[:phrase] || cur["phrase"] || "") |> to_b()
    prev_w = (prev[:phrase] || prev["phrase"] || "") |> to_b() |> String.downcase()

    pos  =
      (getf(feats_or_map, :pos) || getf(feats_or_map, "pos") || "")
      |> to_b() |> String.downcase()

    defn =
      (getf(feats_or_map, :definition) || getf(feats_or_map, "definition") || "")
      |> to_b() |> String.downcase()

    at_start? = match?(%{span: {0, _}}, cur) or (cur[:index] || cur["index"] || 0) == 0

    bias = 0.0
    # Salutation: "hello/hi/hey …" → favor interjection at start
    bias =
      if at_start? and Regex.match?(~r/^(hello|hi|hey)\b/i, phrase) and String.contains?(pos, "interjection"),
        do: bias + 0.20, else: bias

    # Definitions gated by "(with "the")" → slightly penalize if not preceded by "the"
    bias =
      if String.contains?(defn, ~s|(with "the")|) and prev_w != "the",
        do: bias - 0.15, else: bias

    bias
  end

  # ── Group scoring (legacy pure stage) ───────────────────────────────

  defp disambiguate_groups(tokens, candidates, ctx, ctxn, weights, opts) do
    groups = Enum.group_by(candidates, & &1.token_index)

    scores_mode      = Keyword.get(opts, :scores, :all)
    margin_threshold = Keyword.get(opts, :margin_threshold, 0.12)
    fit?             = Keyword.get(opts, :fit?, fn _cand, _si -> true end)
    prime_opts       = Keyword.get(opts, :prime, [])

    {choices, losers} =
      Enum.map_reduce(groups, MapSet.new(), fn {tidx, cands0}, acc_losers ->
        tok =
          case Enum.at(tokens, tidx) do
            %{} = t -> t
            _ -> %{}
          end

        # sanitize: drop nil/id-less, de-dup by id, then apply MWE↔sense compatibility
        cands =
          cands0
          |> Enum.reject(&is_nil/1)
          |> Enum.reject(&(is_nil(&1.id) or not is_binary(&1.id)))
          |> Enum.uniq_by(& &1.id)
          |> filter_candidates_for_token(tok)

        # If no candidates remain after filtering, skip this token
        if cands == [] do
          {nil, acc_losers}
        else
          try do
            {_cand_feats, normed} = score_and_normalize(cands, ctx, ctxn, weights, prime_opts)

            {best_c, best_f} = best_of_normed(normed)

            {winner_c, winner_f} =
              case best_c do
                nil -> {nil, %{score_norm: 0.0, sim: 0.0, score_raw: 0.0}}
                bc  ->
                  if fit?.(bc, %{}) do
                    {bc, best_f}
                  else
                    best_of_normed(Enum.drop(normed, 1))
                  end
              end

            {_ru_c, ru_f} =
              case normed do
                [_only]           -> {nil, %{score_norm: 0.0}}
                [_, {c2, f2} | _] -> {c2, f2}
                _                 -> {nil, %{score_norm: 0.0}}
              end

            margin = (winner_f[:score_norm] || 0.0) - (ru_f[:score_norm] || 0.0)

            # build alt ids from top-2 if margin below threshold
            alt_ids =
              if margin < margin_threshold and length(normed) > 1 do
                case Enum.sort_by(normed, fn {_c, f} -> -f.score_norm end) do
                  [_first, {c2, _} | _] -> [c2.id]
                  _ -> []
                end
              else
                []
              end

            if winner_c, do: priming_bump(winner_c.id, prime_opts)

            lemma = lemma_for(tokens, tidx) || (winner_c && (winner_c[:lemma] || "")) || ""

            # cache sorted once for :top2 to avoid double sorts
            sorted_normed = if scores_mode == :all, do: [], else: Enum.sort_by(normed, fn {_c, f} -> -f.score_norm end)

            scores =
              case scores_mode do
                :all ->
                  normed |> Map.new(fn {c, f} -> {c.id, f.score_norm} end)

                :top2 ->
                  case sorted_normed do
                    []                       -> %{}
                    [{c1, f1}]               -> %{c1.id => f1.score_norm}
                    [{c1, f1}, {c2, f2} | _] -> %{c1.id => f1.score_norm, c2.id => f2.score_norm}
                  end

                _ ->
                  %{}
              end

            choice = %{
              token_index: tidx,
              lemma: lemma,
              chosen_id: winner_c && winner_c.id,
              alt_ids: alt_ids,
              scores: scores,
              margin: margin,
              features:
                winner_c && %{
                  sim: winner_f.sim,
                  score_raw: winner_f.score_raw,
                  score_norm: winner_f.score_norm,
                  pos: winner_c[:pos],
                  lex_fit: winner_c[:lex_fit],
                  rel_prior: winner_c[:rel_prior],
                  intent_bias: winner_c[:intent_bias],
                  activation: winner_c[:activation]
                } || %{}
            }

            losers_here =
              case winner_c do
                nil -> MapSet.new()
                wc  -> cands |> Enum.filter(&(&1.id != wc.id)) |> Enum.map(& &1.id) |> MapSet.new()
              end

            {choice, MapSet.union(acc_losers, losers_here)}
          rescue
            _ -> {nil, acc_losers}
          end
        end
      end)
      |> then(fn {choices, losers} ->
        # Filter out nil choices from tokens with no valid candidates
        {Enum.reject(choices, &is_nil/1), losers}
      end)

    choices = Enum.sort_by(choices, & &1.token_index)

    boosts      = for ch <- choices, ch.chosen_id, do: {ch.chosen_id, +0.5}
    inhibitions = Enum.map(losers, &{&1, -0.25})

    {choices, boosts, inhibitions}
  end

  defp score_and_normalize(cands, ctx, ctxn, weights, prime_opts) when is_list(cands) do
    w_prime = Map.get(weights, :prime, 0.0) * 1.0

    cand_feats =
      Enum.map(cands, fn cand ->
        sim =
          case cand.embedding do
            nil -> 0.0
            vec when is_list(vec) -> cosine_with_ctxnorm(vec, ctx, ctxn)
            _ -> 0.0
          end

        prime =
          if w_prime == 0.0, do: 0.0, else: priming_boost_for(to_string(Map.get(cand, :id, "")), prime_opts)

        score =
          weights.lex * safe_num(cand.lex_fit) +
            weights.sim * sim +
            weights.rel * safe_num(cand.rel_prior) +
            weights.prag * safe_num(cand.intent_bias) +
            weights.act * safe_num(cand.activation) +
            w_prime * prime

        {cand, %{sim: sim, score_raw: score}}
      end)

    normed_vals = normalize_scores(Enum.map(cand_feats, fn {_c, f} -> f.score_raw end))

    normed =
      Enum.zip(cand_feats, normed_vals)
      |> Enum.map(fn {{cand, feats}, s_norm} -> {cand, Map.put(feats, :score_norm, s_norm)} end)

    {cand_feats, normed}
  end

  defp best_of_normed([]), do: {nil, %{score_norm: 0.0}}
  defp best_of_normed(list), do: Enum.max_by(list, fn {_c, f} -> f.score_norm end, fn -> hd(list) end)

  defp phrase_for(tokens, idx) do
    case Enum.at(tokens, idx) do
      %{phrase: p} when is_binary(p) -> p
      _ -> nil
    end
  end

  defp lemma_for(tokens, idx), do: phrase_for(tokens, idx)

  # ── Tripwire/Reanalysis/Priming — stubs ─────────────────────────────
  defp tripwire_check_and_filter(tokens, _opts), do: tokens
  defp priming_bump(_id, _opts), do: :ok
  defp priming_boost_for(_id, _opts), do: 0.0

  # ── Math & utils (legacy pure) ──────────────────────────────────────

  @abs_eps 1.0e-15
  @rel_eps 1.0e-12

  defp zeroish?(x, scale) when is_float(x) do
    ax = abs(x)
    ax <= @abs_eps or ax <= @rel_eps * max(1.0, abs(scale))
  end

  @doc "Cosine similarity; 0.0 on nil/zero-norm."
  @spec cosine([number()] | nil, [number()] | nil) :: float()
  def cosine(nil, _), do: 0.0
  def cosine(_, nil), do: 0.0
  def cosine(a, b) when is_list(a) and is_list(b) and length(a) == length(b) do
    num = dot(a, b)
    den = l2(a) * l2(b)
    if zeroish?(den, den), do: 0.0, else: num / den
  end
  def cosine(_a, _b), do: 0.0

  @spec normalize_scores([number()]) :: [float()]
  def normalize_scores(scores) when is_list(scores) do
    m = Enum.max([0.0 | scores], fn -> 0.0 end)
    exps = Enum.map(scores, fn s -> :math.exp(s - m) end)
    z = Enum.sum(exps)
    if zeroish?(z, z), do: List.duplicate(0.0, length(scores)), else: Enum.map(exps, &(&1 / z))
  end
  def normalize_scores(_), do: []

  defp cosine_with_ctxnorm(vec, ctx, ctxn) do
    cond do
      is_nil(vec) or is_nil(ctx) -> 0.0
      zeroish?(ctxn, ctxn) -> 0.0
      true ->
        na = l2(vec)
        den = na * ctxn
        if zeroish?(den, den), do: 0.0, else: dot(vec, ctx) / den
    end
  end

  defp dot(a, b) when is_list(a) and is_list(b), do: Enum.zip(a, b) |> Enum.reduce(0.0, fn {x, y}, acc -> acc + safe_num(x) * safe_num(y) end)
  defp dot(_a, _b), do: 0.0
  defp l2(v) when is_list(v), do: v |> Enum.reduce(0.0, fn x, acc -> acc + safe_num(x) * safe_num(x) end) |> :math.sqrt()
  defp l2(_), do: 0.0
  defp safe_num(nil), do: 0.0
  defp safe_num(x) when is_number(x), do: x * 1.0
  defp safe_num(_), do: 0.0
  defp getf(m, k) when is_atom(k), do: Map.get(m, k) || Map.get(m, Atom.to_string(k))
  defp getf(m, k) when is_binary(k), do: Map.get(m, k)
  defp getf(_m, _k), do: nil
  defp to_b(nil), do: ""
  defp to_b(v) when is_binary(v), do: v
  defp to_b(v), do: Kernel.to_string(v)

  # ── NEW: Shared normalization helper ─────────────────────────────────
  defp norm_phrase(phrase) do
    phrase
    |> to_b()
    |> String.downcase()
    |> String.replace(~r/\s+/, " ")
    |> String.trim()
  end

  # --- Confidence helpers (pure) ---
  @doc "Return p(top1) from a LIFG choice (max over normalized scores)."
  def top1_prob(choice) when is_map(choice) do
    choice |> Map.get(:scores, %{}) |> Map.values() |> Enum.max(fn -> 0.0 end)
  end
  def top1_prob(_), do: 0.0

  @doc """
  Low-confidence predicate used by pMTG gate.
  Defaults: tau_confident=0.20, p_min=0.65. Returns true when pMTG should fire.
  """
  def low_confidence?(choice, opts) when is_map(choice) do
    tau   = Keyword.get(opts, :tau_confident, 0.20)
    p_min = Keyword.get(opts, :p_min, 0.65)
    m     = Map.get(choice, :margin, 0.0)
    p1    = top1_prob(choice)
    alts? = (choice[:alt_ids] || []) != []
    (m < tau) or (p1 < p_min) or alts?
  end
  def low_confidence?(_choice, _opts), do: true

  defp sense_lemma(c) when is_map(c) do
    (Map.get(c, :lemma) ||
       get_in(c, [:features, :lemma]) ||
       get_in(c, ["features", "lemma"]) ||
       Map.get(c, :word))
    |> case do
      nil -> nil
      ""  -> nil
      v   -> to_string(v)
    end
  end
  defp sense_lemma(_), do: nil

  defp filter_candidates_for_token(list, tok) when is_list(list) and is_map(tok) do
    # MWE token if n>1 or explicit :mw flag
    mwe? = (Map.get(tok, :n, 1) > 1) or (Map.get(tok, :mw) in [true, "true"])

    have_lemma? = Enum.any?(list, &(!is_nil(sense_lemma(&1))))

    kept =
      Enum.filter(list, fn c ->
        case sense_lemma(c) do
          nil     -> true            # no lemma info? keep to be safe
          lemma   ->
            has_space = String.contains?(lemma, " ")
            if mwe?, do: has_space, else: not has_space
        end
      end)

    # safe fallback: if the filter nuked everything but we had lemmas, keep original list
    if have_lemma? and kept == [], do: list, else: kept
  end

  defp filter_candidates_for_token(list, _tok), do: list

  defp ensure_list(l) when is_list(l), do: l
  defp ensure_list(_), do: []

  ##===================== HYGIENE (optional helper) =====================
  defmodule Hygiene do
    @moduledoc """
    Post-Stage-1 hygiene pass:
      • Sanitizes score maps (drops non-numeric/NaN)
      • Computes softmax probabilities as `:probs`
      • Adds `:p_top1`
      • Dedups `:alt_ids`
      • Flags `:needs_rerun` (by margin/p_top1/alt_ids)
      • Emits telemetry: [:brain, :hygiene, :wipe]
    """

    require Logger

    @type choice :: map()

    @spec run(map(), [choice()], keyword()) ::
            {:ok, %{si: map(), choices: [choice()], audit: map()}} | {:error, term()}
    def run(si, choices, opts \\ []) when is_map(si) and is_list(choices) do
      try do
        min_margin =
          Keyword.get(opts, :min_margin, Application.get_env(:brain, :lifg_min_margin, 0.12))

        p_min =
          Keyword.get(opts, :p_min, Application.get_env(:brain, :lifg_min_p_top1, 0.65))

        event = Keyword.get(opts, :event, [:brain, :hygiene, :wipe])
        now_ms = System.system_time(:millisecond)

        {cleaned, stats} =
          Enum.map_reduce(choices, %{sanitized: 0, needs_rerun: 0}, fn ch, acc ->
            {probs, dropped} = normalize_scores_map(Map.get(ch, :scores, %{}))
            p1 = max_prob(probs)

            alt_ids2 =
              (ch[:alt_ids] || [])
              |> Enum.reject(&(&1 == ch[:chosen_id]))
              |> Enum.uniq()

            needs? =
              (ch[:margin] || 0.0) < min_margin or
                p1 < p_min or
                alt_ids2 != []

            ch2 =
              ch
              |> Map.put(:alt_ids, alt_ids2)
              |> Map.put(:probs, probs)
              |> Map.put(:p_top1, p1)
              |> Map.put(:needs_rerun, needs?)

            acc2 =
              acc
              |> Map.update!(:sanitized, &(&1 + dropped))
              |> Map.update!(:needs_rerun, &(&1 + if(needs?, do: 1, else: 0)))

            {ch2, acc2}
          end)

        meas = %{total: length(choices), sanitized: stats.sanitized, needs_rerun: stats.needs_rerun, ts_ms: now_ms}
        emit(event, meas, %{min_margin: min_margin, p_min: p_min})
        {:ok, %{si: si, choices: cleaned, audit: meas}}
      rescue
        e ->
          Logger.error("LIFG Hygiene run failed: #{inspect(e)}")
          {:error, e}
      end
    end

    defp normalize_scores_map(map) when is_map(map) do
      pairs =
        map
        |> Enum.filter(fn {_k, v} -> is_number(v) and v == v end) # (v==v) filters NaN

      dropped = map_size(map) - length(pairs)
      vals = Enum.map(pairs, fn {_, v} -> v end)

      probs =
        case vals do
          [] -> %{}
          _ ->
            m = Enum.max([0.0 | vals])
            exps = Enum.map(vals, fn s -> :math.exp(s - m) end)
            z = Enum.sum(exps)

            if z == 0.0 do
              Map.new(pairs, fn {k, _} -> {k, 0.0} end)
            else
              pairs |> Enum.zip(exps) |> Map.new(fn {{k, _}, e} -> {k, e / z} end)
            end
        end

      {probs, dropped}
    end

    defp max_prob(%{} = probs), do: probs |> Map.values() |> Enum.max(fn -> 0.0 end)
    defp max_prob(_), do: 0.0

    defp emit(ev, meas, meta) do
      if Code.ensure_loaded?(:telemetry) and function_exported?(:telemetry, :execute, 3) do
        :telemetry.execute(ev, meas, meta)
      else
        :ok
      end
    end
  end

  ##===================== TOKEN GUARD (compat) ==========================
  defmodule Guard do
    @moduledoc """
    Compatibility shim for Core.LIFG.Input.
    Ensures maps, index, and optional span-sort.
    """
    @spec sanitize(list()) :: list()
    def sanitize(tokens) when is_list(tokens) do
      tokens
      |> Enum.map(&mapify/1)
      |> ensure_indexed()
      |> sort_by_span_if_present()
    end

    defp mapify(t) when is_map(t), do: t
    defp mapify(%_{} = s), do: Map.from_struct(s)
    defp mapify(other), do: %{phrase: to_string(other)}

    defp ensure_indexed(list) when is_list(list) do
      list
      |> Enum.with_index()
      |> Enum.map(fn {t, i} ->
        Map.put_new(t, :index, Map.get(t, :index) || Map.get(t, "index") || i)
      end)
    end

    defp sort_by_span_if_present(list) when is_list(list) do
      if Enum.all?(list, &valid_span?/1), do: Enum.sort_by(list, &elem(Map.fetch!(&1, :span), 0)), else: list
    end

    defp valid_span?(%{span: {s, l}}) when is_integer(s) and is_integer(l) and l > 0, do: true
    defp valid_span?(_), do: false
  end

  ##================== TOKEN BOUNDARY GUARD (compat) ====================
  defmodule BoundaryGuard do
    @moduledoc """
    Minimal boundary + char-gram guard (keeps MWEs).
    """
    @type token :: map()
    @spec sanitize([token()]) :: [token()]
    def sanitize(tokens), do: sanitize(tokens, nil)

    @spec sanitize([token()], String.t() | nil) :: [token()]
    def sanitize(tokens, sentence) when is_list(tokens) do
      tokens
      |> Enum.map(&mapify/1)
      |> Enum.reject(&chargram?/1)
      |> Enum.filter(fn t ->
        mw? = truthy(Map.get(t, :mw) || Map.get(t, "mw"))
        case {Map.get(t, :span) || Map.get(t, "span"), sentence} do
          {{s, l}, snt} when is_integer(s) and is_integer(l) and l > 0 and is_binary(snt) ->
            boundary_aligned?(snt, s, l) or mw?
          _ -> true
        end
      end)
      |> sort_by_span_if_present()
    end

    defp mapify(%_{} = s), do: Map.from_struct(s)
    defp mapify(%{} = m), do: m
    defp mapify(other), do: %{phrase: to_string(other)}

    defp chargram?(t) when is_map(t),
      do:
        (Map.get(t, :chargram) in [true, "true"]) or
        (Map.get(t, :kind) in [:chargram, "chargram"]) or
        (Map.get(t, :source) in [:chargram, "chargram"])
    defp chargram?(_), do: false

    defp sort_by_span_if_present(list) when is_list(list) do
      if Enum.all?(list, &valid_span?/1), do: Enum.sort_by(list, &elem(Map.fetch!(&1, :span), 0)), else: list
    end

    defp valid_span?(%{span: {s, l}}) when is_integer(s) and is_integer(l) and l > 0, do: true
    defp valid_span?(_), do: false

    defp boundary_aligned?(snt, s, l) when is_binary(snt) and is_integer(s) and is_integer(l) do
      left = s - 1
      right = s + l
      left_ok = s == 0 or not word_char?(String.slice(snt, left, 1))
      right_ok = right >= String.length(snt) or not word_char?(String.slice(snt, right, 1))
      left_ok and right_ok
    end
    defp boundary_aligned?(_snt, _s, _l), do: false

    defp word_char?(""), do: false
    defp word_char?(ch), do: Regex.match?(~r/^[\p{L}\p{N}]$/u, ch)
    defp truthy(true), do: true
    defp truthy("true"), do: true
    defp truthy(_), do: false
  end

  ##========================= STAGE 1 (new) ============================
  defmodule Stage1 do
    @moduledoc """
    Stage 1 — fast per-token disambiguation from si.sense_candidates.
    Options:
      :weights          — %{lex_fit, rel_prior, activation, intent_bias}
      :scores           — :all | :top2 | :none (default from `:lifg_stage1_scores_mode`)
      :chargram_event   — telemetry (default [:brain, :lifg, :chargram_violation])
      :boundary_event   — telemetry (default [:brain, :lifg, :boundary_drop])
      :margin_threshold — default 0.15 (for alt_id emission)
    """

    require Logger

    @type si :: map()
    @type choice :: %{
            required(:token_index) => non_neg_integer(),
            required(:chosen_id)   => String.t(),
            required(:alt_ids)     => [String.t()],
            required(:margin)      => float(),
            optional(:scores)      => map()
          }

    @spec run(si(), keyword()) :: {:ok, %{si: si(), choices: [choice()], audit: map()}} | {:error, term()}
    def run(si, opts \\ []) when is_map(si) and is_list(opts) do
      try do
        t0 = System.monotonic_time()

        tokens = Map.get(si, :tokens, [])# |> ensure_list()
        slate  = case Map.get(si, :sense_candidates, Map.get(si, :candidates_by_token, %{})) do
          %{} = s -> s
          _ -> %{}
        end

        {kept_tokens, dropped} = guard_tokens(tokens, si, opts)

        scores_mode =
          Keyword.get(opts, :scores, Application.get_env(:brain, :lifg_stage1_scores_mode, :all))

        margin_threshold = Keyword.get(opts, :margin_threshold, 0.15)

        choices =
          kept_tokens
          |> Enum.map(fn tok ->
            disambiguate_token(
              tok,
              Map.get(slate, tok.index, []),
              scores: scores_mode,
              margin_threshold: margin_threshold,
              weights: Keyword.get(opts, :weights, %{})
            )
          end)
          |> Enum.reject(&is_nil/1)

        timing_ms =
          System.convert_time_unit(System.monotonic_time() - t0, :native, :millisecond)

        audit = %{
          stage: :lifg_stage1,
          token_count: length(tokens),
          kept_tokens: length(kept_tokens),
          dropped_tokens: length(dropped),
          timing_ms: timing_ms
        }

        {:ok, %{si: si, choices: choices, audit: audit}}
      rescue
        e ->
          Logger.error("LIFG Stage1 run failed: #{inspect(e)}")
          {:error, e}
      end
    end

    # Back-compat shim (si, ctx, opts)
    @spec run(si(), map(), keyword()) :: {:ok, %{si: si(), choices: [choice()], audit: map()}} | {:error, term()}
    def run(si, _ctx, opts), do: run(si, opts)

    # ── Guards ─────────────────────────────────────────────────────────

    defp guard_tokens(tokens, si, opts) when is_list(tokens) and is_map(si) do
      char_ev = Keyword.get(opts, :chargram_event, [:brain, :lifg, :chargram_violation])
      bnd_ev  = Keyword.get(opts, :boundary_event,  [:brain, :lifg, :boundary_drop])

      Enum.reduce(tokens, {[], []}, fn tok, {kept, dropped} ->
        cond do
          is_chargram?(tok) ->
            emit(char_ev, %{token_index: tok[:index], phrase: tok[:phrase]}, %{})
            {kept, [tok | dropped]}

          not boundary_ok?(tok, si) ->
            emit(bnd_ev, %{token_index: tok[:index], phrase: tok[:phrase]}, %{})
            {kept, [tok | dropped]}

          true ->
            {[tok | kept], dropped}
        end
      end)
      |> (fn {k, d} -> {Enum.reverse(k), Enum.reverse(d)} end).()
    end
    defp guard_tokens(_tokens, _si, _opts), do: {[], []}

    defp is_chargram?(tok) when is_map(tok) do
      (Map.get(tok, :source) in [:chargram, "chargram"]) or
        (Map.get(tok, :kind)   in [:chargram, "chargram"]) or
        (Map.get(tok, :chargram) in [true, "true"])
    end
    defp is_chargram?(_), do: false

    defp boundary_ok?(tok, si) when is_map(tok) and is_map(si) do
      # MWEs bypass strictness
      mw? = Map.get(tok, :mw) || Map.get(tok, "mw") || false
      if mw?, do: true, else: do_boundary_check(tok, si)
    end
    defp boundary_ok?(_tok, _si), do: false

    defp do_boundary_check(tok, si) when is_map(tok) and is_map(si) do
      sent = Map.get(si, :sentence)
      span = Map.get(tok, :span) || Map.get(tok, "span")

      cond do
        not is_binary(sent) -> true
        not (is_tuple(span) and tuple_size(span) == 2) -> true
        true ->
          {start, len} = span
          start = as_int(start, 0)
          len   = as_int(len, 0)
          stop  = start + len
          s_len = byte_size(sent)

          left_ok?  = start <= 0 or not letter?(String.at(sent, start - 1))
          right_ok? = stop >= s_len or not letter?(String.at(sent, stop))

          left_ok? and right_ok?
      end
    end
    defp do_boundary_check(_tok, _si), do: true

    defp letter?(<<c::utf8>>), do: unicode_letter?(c)
    defp unicode_letter?(cp),
      do: (cp >= ?A and cp <= ?Z) or (cp >= ?a and cp <= ?z) or (cp >= ?À and cp <= 0x024F)

    # ── Disambiguation per token (hybrid) ──────────────────────────────

    defp disambiguate_token(%{} = tok, cand_list, opts) when is_list(cand_list) do
      idx         = tok.index || 0
      thr         = Keyword.get(opts, :margin_threshold, 0.15)
      scores_mode = Keyword.get(opts, :scores, :all)
      w           = weights(opts)

      # sanitize input candidates (nil/id-less/dupes) and apply tiny heuristics
      cand_list =
        cand_list
        |> Enum.reject(&is_nil/1)
        |> Enum.reject(&(is_nil(&1.id) or not is_binary(&1.id)))
        |> Enum.uniq_by(& &1.id)
        |> prefer_salutation_interjection(tok)
        |> prefer_pronoun_for_I(tok)
        |> compat_filter_for_token(tok)

      score_cand = fn c when is_map(c) ->
        f = Map.get(c, :features, %{})
        %{
          id: c.id,
          score:
            w.lex_fit     * as_float(f[:lex_fit]     || f["lex_fit"]) +
            w.rel_prior   * as_float(f[:rel_prior]   || f["rel_prior"]) +
            w.activation  * as_float(f[:activation]  || f["activation"]) +
            w.intent_bias * as_float(f[:intent_bias] || f["intent_bias"])
        }
      end

      scored = cand_list |> Enum.map(&score_cand.(&1))

      if length(scored) == 0 do
        nil
      else
        sorted_scored = Enum.sort_by(scored, &(-&1.score))

        [%{id: top_id, score: top_s} | rest] = sorted_scored

        rest_unique =
          rest
          |> Enum.reject(&(&1.id == top_id))
          |> Enum.uniq_by(& &1.id)

        second_s =
          case rest_unique do
            [%{score: s2} | _] -> s2
            _ -> 0.0
          end

        alt_ids =
          if max(top_s - second_s, 0.0) < thr and rest_unique != [] do
            [hd(rest_unique).id]
          else
            []
          end

        %{
          token_index: idx,
          chosen_id: top_id,
          alt_ids: alt_ids,
          margin: max(top_s - second_s, 0.0),
          scores: build_scores_map(sorted_scored, scores_mode)
        }
      end
    end
    defp disambiguate_token(_tok, _cand_list, _opts), do: nil

    # Prefer interjection "greeting" senses for salutations ("hello/hi/hey"), esp. at start
    defp prefer_salutation_interjection(list, tok) when is_list(list) and is_map(tok) do
      phrase = (tok[:phrase] || "") |> to_string()
      at_start =
        case tok[:span] do
          {0, _} -> true
          _ -> (tok[:index] || 0) == 0
        end

      sal? = Regex.match?(~r/^(hello|hi|hey)\b/i, phrase) or at_start

      if sal? do
        greet =
          Enum.filter(list, fn c when is_map(c) ->
            pos =
              (c[:pos] || get_in(c, [:features, :pos]) || "")
              |> to_string() |> String.downcase()

            defn =
              (c[:definition] || get_in(c, [:features, :definition]) || "")
              |> to_string() |> String.downcase()

            syns =
              (c[:synonyms] || get_in(c, [:features, :synonyms]) || [])
              |> Enum.map(&String.downcase/1)

            String.contains?(pos, "interjection") and
              (String.contains?(defn, "greet") or Enum.any?(syns, &(&1 == "greeting")))
          end)

        if greet != [], do: greet, else: list
      else
        list
      end
    end
    defp prefer_salutation_interjection(list, _tok), do: list

    # Pronoun preference for the token exactly "I"
    defp prefer_pronoun_for_I(list, tok) when is_list(list) and is_map(tok) do
      phrase = (Map.get(tok, :phrase) || Map.get(tok, "phrase") || "") |> to_string()
      if phrase == "I" do
        pronounish = Enum.filter(list, fn c when is_map(c) -> pos_pronoun?(c) or id_pronounish?(c) end)
        if pronounish == [], do: list, else: pronounish
      else
        list
      end
    end
    defp prefer_pronoun_for_I(list, _tok), do: list

    defp pos_pronoun?(c) when is_map(c) do
      p =
        Map.get(c, :pos) ||
          get_in(c, [:features, :pos]) ||
          get_in(c, ["features", "pos"])

      cond do
        is_nil(p) -> false
        is_atom(p) -> p |> Atom.to_string() |> String.downcase() |> String.contains?("pron")
        is_binary(p) -> p |> String.downcase() |> String.contains?("pron")
        true -> false
      end
    end
    defp pos_pronoun?(_), do: false

    defp id_pronounish?(c) when is_map(c) do
      id = Map.get(c, :id) |> to_string()
      String.contains?(String.downcase(id), "pron")
    end
    defp id_pronounish?(_), do: false

    # Prefer MWE senses for MWE tokens (and vice versa), with safe fallback
    defp compat_filter_for_token(list, %{n: n} = tok) when is_list(list) and is_map(tok) and is_integer(n) do
      mwe? = n > 1 or (Map.get(tok, :mw) || Map.get(tok, "mw") || false)
      have_lemma? = Enum.any?(list, &has_readable_lemma?/1)

      kept =
        Enum.filter(list, fn c when is_map(c) ->
          case sense_lemma(c) do
            nil -> true  # if unknown, keep
            lemma ->
              has_space = String.contains?(lemma, " ")
              if mwe?, do: has_space, else: not has_space
          end
        end)

      if have_lemma? and kept == [], do: list, else: kept
    end
    defp compat_filter_for_token(list, _tok), do: list

    defp has_readable_lemma?(c), do: not is_nil(sense_lemma(c))

    defp sense_lemma(c) when is_map(c) do
      (Map.get(c, :lemma) ||
         get_in(c, [:features, :lemma]) ||
         get_in(c, ["features", "lemma"]) ||
         Map.get(c, :word))
      |> case do
        nil -> nil
        ""  -> nil
        v   -> to_string(v)
      end
    end
    defp sense_lemma(_), do: nil

    # Build score map per requested mode; default :all for back-compat
    defp build_scores_map(scored, :all) when is_list(scored),
      do: Map.new(scored, fn %{id: i, score: s} -> {i, s} end)

    defp build_scores_map(scored, :top2) when is_list(scored) do
      case scored do
        [] -> %{}
        [a] -> %{a.id => a.score}
        [a, b | _] -> %{a.id => a.score, b.id => b.score}
      end
    end

    defp build_scores_map(_scored, _), do: %{}

    # ── Utils ──────────────────────────────────────────────────────────

    defp weights(opts) when is_list(opts) do
      env = Application.get_env(:brain, :lifg_stage1_weights, %{})
      base = %{lex_fit: 0.4, rel_prior: 0.3, activation: 0.2, intent_bias: 0.1}

      base
      |> Map.merge(env || %{})
      |> Map.merge(Map.new(Keyword.get(opts, :weights, [])))
    end

# === in apps/brain/lib/brain/lifg_stage1.ex (inside defmodule Brain.LIFG.Stage1) ===

    defp emit(ev, meas, meta) do
      if Code.ensure_loaded?(:telemetry) and function_exported?(:telemetry, :execute, 3) do
        :telemetry.execute(ev, meas, meta)
      else
        :ok
      end
    end

    defp as_int(nil, d), do: d
    defp as_int(i, _d) when is_integer(i), do: i
    defp as_int(b, d) when is_binary(b) do
      case Integer.parse(b) do
        {n, _} -> n
        _ -> d
      end
    end
    defp as_int(_, d), do: d

    defp as_float(nil), do: 0.0
    defp as_float(n) when is_number(n), do: n * 1.0
    defp as_float(b) when is_binary(b) do
      case Float.parse(b) do
        {f, _} -> f
        _ -> 0.0
      end
    end
    defp as_float(_), do: 0.0
  end

  @doc """
  Full LIFG pipeline:
  1) ATL finalize → slate (if available)
  2) Attach si.sense_candidates
  3) Stage-1 disambiguation
  4) pMTG consult (sync rerun or async boost/none)
  Returns {:ok, %{si, choices, slate}}.
  """
@spec run(map(), keyword()) :: {:ok, %{si: map(), choices: list(), slate: map()}} | {:error, term()}
def run(si, opts \\ []) when is_map(si) and is_list(opts) do
  require Logger

  try do
    # 1) ATL finalize (optional): expect {si, slate}; fallback to {si, %{}}
    {si1, slate} =
      if Code.ensure_loaded?(Brain.ATL) and function_exported?(Brain.ATL, :finalize, 2) do
        case Brain.ATL.finalize(si, opts) do
          {s, sl} when is_map(s) and is_map(sl) -> {s, sl}
          _ -> {si, %{}}
        end
      else
        {si, %{}}
      end

    # 2) Attach slate → sense_candidates (optional)
    si2 =
      if Code.ensure_loaded?(Brain.ATL) and function_exported?(Brain.ATL, :attach_sense_candidates, 3) do
        case Brain.ATL.attach_sense_candidates(
               si1,
               slate,
               top_k: Keyword.get(opts, :top_k, 3),
               margin_window: Keyword.get(opts, :margin_window, 0.05)
             ) do
          %{} = s -> s
          _ -> si1
        end
      else
        si1
      end

    # 3) Stage-1 scoring
    scores_mode = Keyword.get(opts, :scores, Application.get_env(:brain, :lifg_stage1_scores_mode, :all))
    margin_thr  = Keyword.get(opts, :margin_threshold, 0.15)

    case Stage1.run(
           si2,
           weights: Map.get(opts, :weights, %{
             lex_fit: 0.5, rel_prior: 0.25, activation: 0.15, intent_bias: 0.10
           }),
           scores: scores_mode,
           margin_threshold: margin_thr,
           chargram_event: [:brain, :lifg, :chargram_violation],
           boundary_event: [:brain, :lifg, :boundary_drop]
         ) do
      {:ok, %{si: si3, choices: choices, audit: _a}} ->
        # 4) pMTG integration
        pmtg_mode = Keyword.get(opts, :pmtg_mode, Application.get_env(:brain, :pmtg_mode, :boost))

        {final_si, final_choices} =
          if Code.ensure_loaded?(Brain.PMTG) do
            case {pmtg_mode, Keyword.get(opts, :pmtg_apply?, true)} do
              {:rerun, true} ->
                case Brain.PMTG.consult_sync(
                       choices,
                       si3.tokens,
                       already_needy: false,
                       margin_threshold: margin_thr,
                       limit: Keyword.get(opts, :limit, 5),
                       mode: :rerun,
                       rerun_only_if_hits: Keyword.get(opts, :rerun_only_if_hits, true),
                       rerun_weights_bump: Keyword.get(opts, :rerun_weights_bump, %{lex_fit: 0.05, rel_prior: 0.05})
                     ) do
                  {:ok, %{si: si_after, choices: merged}} -> {si_after, merged}
                  _ -> {si3, choices}
                end

              _other_mode ->
                # Async consult; keep current choices
                _ = Brain.PMTG.consult(
                      choices,
                      si3.tokens,
                      margin_threshold: margin_thr,
                      limit: Keyword.get(opts, :limit, 5),
                      mode: pmtg_mode
                    )
                {si3, choices}
            end
          else
            {si3, choices}
          end

        {:ok, %{si: final_si, choices: final_choices, slate: slate}}

      {:error, reason} ->
        {:error, {:stage1, reason}}
    end
  rescue
    e ->
      Logger.error("LIFG full run failed: #{inspect(e)}")
      {:error, e}
  end
end

  # ── weights (outer) ─────────────────────────────────────────────────
  defp to_map(nil), do: %{}
  defp to_map(m) when is_map(m), do: m
  defp to_map(kv) when is_list(kv), do: Map.new(kv)
  defp to_map(_), do: %{}

  @type lifg_outer_weights ::
          %{
            required(:lex) => float(),
            required(:sim) => float(),
            required(:rel) => float(),
            required(:prag) => float(),
            required(:act) => float(),
            optional(:prime) => float()
          }

  @doc "Default outer LIFG weights (lex/sim/rel/prag/act + optional :prime)."
  @spec default_weights() :: lifg_outer_weights()
  def default_weights, do: %{lex: 0.25, sim: 0.40, rel: 0.15, prag: 0.10, act: 0.10, prime: 0.0}

  # Merge order: defaults <- env (lifg_weights OR mapped stage1) <- opts[:weights]
  defp resolved_weights(opts) when is_list(opts) do
    outer_env = to_map(Application.get_env(:brain, :lifg_weights))
    mapped_from_stage1 =
      case to_map(Application.get_env(:brain, :lifg_stage1_weights)) do
        %{} = w1 when map_size(w1) > 0 ->
          %{
            lex:  Map.get(w1, :lex_fit,     default_weights().lex),
            sim:  default_weights().sim, # stage-1 has no :sim
            rel:  Map.get(w1, :rel_prior,   default_weights().rel),
            prag: Map.get(w1, :intent_bias, default_weights().prag),
            act:  Map.get(w1, :activation,  default_weights().act)
          }
        _ -> %{}
      end

    env_weights = if outer_env == %{}, do: mapped_from_stage1, else: outer_env
    opt_w      = to_map(Keyword.get(opts, :weights, %{}))

    default_weights()
    |> Map.merge(env_weights)
    |> Map.merge(opt_w)
  end
end

