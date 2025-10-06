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


@default_weights %{lex_fit: 0.40, rel_prior: 0.30, activation: 0.20, intent_bias: 0.10}

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

# ---------------------------------------------
# Public API (back-compat heads, no wrappers)
# ---------------------------------------------
# In apps/brain/lib/brain/lifg.ex

# ---- LIFG Stage-1: struct-safe, honors si.lifg_opts, works with active_cells ----

@spec disambiguate_stage1(map(), keyword()) :: map()
def disambiguate_stage1(%{} = si, opts) do
  # Merge opts: call-time opts override si.lifg_opts
  si_opts =
    case Map.get(si, :lifg_opts) do
      kw when is_list(kw) -> kw
      m when is_map(m) -> Map.to_list(m)
      _ -> []
    end

  eff_opts       = Keyword.merge(si_opts, opts)
  fit?           = Keyword.get(eff_opts, :fit?, fn _cand, _si -> true end)
  normalize_mode = Keyword.get(eff_opts, :normalize, :none)   # :none | :softmax
  margin_thr     = Keyword.get(eff_opts, :margin_threshold, 0.15)
  scores_mode    = Keyword.get(eff_opts, :scores, :all)       # :all | :top2 | :none
  weights        = lifg_weights()

  sc_map = Map.get(si, :sense_candidates) || Map.get(si, "sense_candidates")
  ac     = Map.get(si, :active_cells)

  # Normalize to per-token candidate groups
  token_groups =
    cond do
      is_map(sc_map) and map_size(sc_map) > 0 ->
        sc_map
        |> Enum.sort_by(fn {ti, _} -> ti end)
        |> Enum.map(fn {ti, list} ->
          {ti, Enum.map(list || [], &normalize_cand/1)}
        end)

      is_list(ac) ->
        ac
        |> Enum.map(&normalize_cand/1)
        |> Enum.group_by(& &1.token_index)
        |> Enum.sort_by(fn {ti, _} -> ti end)

      true ->
        []
    end

  eps = 1.0e-12

  {choices, boosts, inhibitions} =
    Enum.reduce(token_groups, {[], [], []}, fn {ti, group}, {chs, boos, inhs} ->
      ranked_raw =
        group
        |> Enum.map(&score_candidate(&1, weights))
        |> Enum.sort_by(fn c -> {-c.score, c.id} end)

      case ranked_raw do
        [] ->
          {chs, boos, inhs}

        _ ->
          # Display ranking (if softmax requested); selection & gating use RAW
          ranked_disp =
            case normalize_mode do
              :softmax ->
                ps = softmax(Enum.map(ranked_raw, & &1.score))
                Enum.zip_with(ranked_raw, ps, fn c, p -> %{c | score: p} end)
              _ ->
                ranked_raw
            end

          chosen     = Enum.find(ranked_raw, fn c -> fit?.(c, si) end) || hd(ranked_raw)
          losers_raw = Enum.reject(ranked_raw, &(&1.id == chosen.id))

          # Margin = top score gap (so inhibitions can widen it across rounds)
          margin_raw =
            case ranked_raw do
              [_a]       -> 1.0
              [a, b | _] -> max(a.score - b.score, 0.0)
              _          -> 0.0
            end

          # Emit scores per requested mode (display only)
          scores_map =
            case scores_mode do
              :none -> %{}
              :top2 -> ranked_disp |> Enum.take(2) |> Map.new(&{&1.id, &1.score})
              _     -> Map.new(ranked_disp, &{&1.id, &1.score})
            end

          choice = %{
            token_index: ti,
            lemma: Map.get(chosen, :lemma),
            chosen_id: chosen.id,
            alt_ids: Enum.map(losers_raw, & &1.id),
            margin: margin_raw,
            scores: scores_map,
            features: %{
              score_raw:   chosen.score,
              score_norm:  Map.get(scores_map, chosen.id, chosen.score),
              pos: nil, sim: 0.0,
              rel_prior:   feat(chosen.features, :rel_prior),
              lex_fit:     feat(chosen.features, :lex_fit),
              activation:  feat(chosen.features, :activation),
              intent_bias: feat(chosen.features, :intent_bias)
            }
          }

          boosts_here =
            if margin_raw + eps >= margin_thr do
              [%{token_index: ti, id: chosen.id, amount: Float.round(margin_raw, 6)}]
            else
              []
            end

          inhibitions_here =
            if margin_raw + eps < margin_thr and losers_raw != [] do
              Enum.map(losers_raw, fn l ->
                gap = max(chosen.score - l.score, 0.0)
                amt = Float.round(max(margin_thr - gap, 0.0), 6)
                %{token_index: ti, id: l.id, amount: amt}
              end)
            else
              []
            end

          {[choice | chs], boosts_here ++ boos, inhibitions_here ++ inhs}
      end
    end)

  # Option A: convert map-shaped signals to tuple form when gating/WM is involved
  pairs_mode? =
    Keyword.has_key?(eff_opts, :gate_into_wm) or
    Keyword.has_key?(eff_opts, :lifg_min_score) or
    Keyword.get(eff_opts, :emit_pairs, false)

  {boosts_out, inhibitions_out} =
    if pairs_mode? do
      {
        Enum.map(boosts, fn
          %{token_index: _ti, id: id, amount: amt} -> {id, +amt}
          %{id: id, amount: amt} -> {id, +amt}
        end),
        Enum.map(inhibitions, fn
          %{token_index: _ti, id: id, amount: amt} -> {id, -amt}
          %{id: id, amount: amt} -> {id, -amt}
        end)
      }
    else
      {boosts, inhibitions}
    end

  evt = %{
    stage: :lifg_stage1,
    choices: Enum.reverse(choices),
    boosts: Enum.reverse(boosts_out),
    inhibitions: Enum.reverse(inhibitions_out),
    opts: Enum.into(eff_opts, %{})
  }

  trace = [evt | (Map.get(si, :trace) || [])]
  Map.put(si, :trace, trace)
end

# (/1) pure; uses si.lifg_opts only
@doc "Pure, stateless stage-1 for SemanticInput. Appends a trace event (:choices/:boosts/:inhibitions)."
@spec disambiguate_stage1(map()) :: map()
def disambiguate_stage1(%{} = si), do: disambiguate_stage1(si, [])

# --- helper: numerically stable softmax over a list of floats ---
defp softmax([]), do: []

defp softmax(xs) when is_list(xs) do
  m   = Enum.max(xs)
  exs = Enum.map(xs, & :math.exp(&1 - m))
  sum = Enum.sum(exs)

  cond do
    sum == 0.0 ->
      n = length(xs)
      if n == 0, do: [], else: List.duplicate(1.0 / n, n)

    true ->
      Enum.map(exs, & &1 / sum)
  end
end


defp lifg_weights do
  env = Application.get_env(:brain, :lifg_stage1_weights, %{})
  Map.merge(@default_weights, env || %{})
end

# ---------------------------------------------
# Scoring & selection
# ---------------------------------------------
  # ── Candidate sources (legacy pure stage) ────────────────────────────

# Preserve fields; only ensure :features exists

# accessor used everywher
defp feat(map, k) when is_map(map) do
  Map.get(map, k) || Map.get(map, to_string(k)) || 0.0
end

# single source of truth for scoring
defp score_candidate(%{} = cand, w) do
  s =
    case cand[:score] do
      n when is_number(n) -> n * 1.0
      _ ->
        f = cand[:features] || %{}
        feat(f, :lex_fit)     * feat(w, :lex_fit) +
        feat(f, :rel_prior)   * feat(w, :rel_prior) +
        feat(f, :activation)  * feat(w, :activation) +
        feat(f, :intent_bias) * feat(w, :intent_bias)
    end
  Map.put(cand, :score, s)
end

# Promote legacy top-level fields into :features, but let existing :features win.
# Promote legacy top-level fields into :features, but preserve token_index/score.
defp normalize_cand(%{} = cand) do
  id =
    cand[:id] || cand[:chosen_id] ||
      cand[:lemma] || cand[:word] || cand[:phrase] || "<?>"

  top_as_features =
    [:lex_fit, :rel_prior, :activation, :intent_bias, :pos, :lemma, :definition, :synonyms]
    |> Enum.reduce(%{}, fn k, acc ->
      case Map.fetch(cand, k) do
        {:ok, v} when not is_nil(v) -> Map.put(acc, k, v)
        _ -> acc
      end
    end)

  merged_features =
    (cand[:features] || %{})
    |> Map.merge(top_as_features, fn _k, from_features, _from_top -> from_features end)

  out =
    cand
    |> Map.put(:id, id)
    |> Map.put(:features, merged_features)
    |> Map.put_new(:lemma, cand[:lemma] || cand[:word] || cand[:phrase] || "")

  # Keep token_index if present
  out =
    case Map.fetch(cand, :token_index) do
      {:ok, ti} -> Map.put(out, :token_index, ti)
      :error    -> out
    end

  # Keep precomputed score if present (lets score_candidate/2 use it)
  case Map.fetch(cand, :score) do
    {:ok, s} -> Map.put(out, :score, s)
    :error   -> out
  end
end

# If the “candidate” is just an ID, lift it.
defp normalize_cand(id) when is_binary(id), do: %{id: id, features: %{}}

 # ── Math & utils (legacy pure) ──────────────────────────────────────
  
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

        tokens = Map.get(si, :tokens, [])
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
end

