defmodule Brain.LIFG do
  @moduledoc ~S"""
  Stage-1 Disambiguation (LIFG). Pure, stateless, fast.
  Public API:
    • `disambiguate_stage1/1` or `disambiguate_stage1/2` — SI → SI
      - consumes `si.sense_candidates` if present (from Core.SenseSlate)
      - else falls back to legacy matching against `si.active_cells`
      - selects per-token winner with Reanalysis fallback
      - appends a trace event and stores winners & traces on SI

  Scoring recipe (weights tunable):
      score = w_lex*lex_fit
            + w_sim*cosine(context_vec, embedding)
            + w_rel*rel_prior
            + w_prag*intent_bias
            + w_act*activation

  Defaults: w_lex=0.25, w_sim=0.40, w_rel=0.15, w_prag=0.10, w_act=0.10.
  Softmax per token group → probabilities sum ≈ 1.0.
  """

  alias Brain.LIFG.{Reanalysis, Tripwire, Priming}
  alias Core.SemanticInput

  # ---------- Numerical stability (epsilon guard) ----------
  @abs_eps 1.0e-15
  @rel_eps 1.0e-12

  # Treat very small magnitudes as zero (absolute OR relative to a scale).
  defp zeroish?(x, scale) when is_float(x) do
    ax = abs(x)
    ax <= @abs_eps or ax <= @rel_eps * max(1.0, abs(scale))
  end

  # ---- Define SenseChoice first so specs can see it ----
  defmodule SenseChoice do
    @enforce_keys [:token_index, :lemma, :chosen_id, :scores, :margin, :features]
    defstruct token_index: 0,
              lemma: "",
              chosen_id: "",
              alt_ids: [],
              scores: %{},
              margin: 0.0,
              features: %{}

    @behaviour Access
    @impl Access
    def fetch(%__MODULE__{} = s, key), do: Map.fetch(Map.from_struct(s), key)
    @impl Access
    def get_and_update(%__MODULE__{} = s, key, fun) do
      {get, map2} = Map.get_and_update(Map.from_struct(s), key, fun)
      {get, struct(s, map2)}
    end

    @impl Access
    def pop(%__MODULE__{} = s, key) do
      {val, map2} = Map.pop(Map.from_struct(s), key)
      {val, struct(s, map2)}
    end
  end

  @typedoc "Per-token choice."
  @type t_choice :: %__MODULE__.SenseChoice{
          token_index: non_neg_integer(),
          lemma: binary(),
          chosen_id: binary(),
          alt_ids: [binary()],
          scores: %{optional(binary()) => number()},
          margin: number(),
          features: map()
        }

  # ─────────────────────────────
  # SI → SI (pipeline-safe)
  # ─────────────────────────────
  @doc "Consume SI, compute LIFG choices with reanalysis, append a trace event, return SI."
  @spec disambiguate_stage1(SemanticInput.t()) :: SemanticInput.t()
  def disambiguate_stage1(si), do: disambiguate_stage1(si, [])

  # New arity-3 wrapper (kept for compatibility; funnels to /2 through opts)
  @spec disambiguate_stage1(map(), any(), keyword()) :: {:ok, map()} | {:error, term()}
  def disambiguate_stage1(input, ctx, opts) do
    disambiguate_stage1(input, Keyword.put(opts, :ctx, ctx))
  end

  @doc """
  Same as `disambiguate_stage1/1` with options:
    * `:weights`         — override weight map
    * `:margin_threshold` — default 0.12
    * `:scores`          — :all (default) | :top2 | :none
    * `:fit?`            — (cand, si) -> boolean (default `&default_fit?/2`)
    * `:max_flips`       — integer (default 1)
    * `:prime`           — priming options (bump winners, etc.)
  """
  @spec disambiguate_stage1(SemanticInput.t(), keyword()) :: SemanticInput.t()
  def disambiguate_stage1(%{} = si, opts) do
    # Tripwire in front of LIFG: report and drop any char-gram leaks
    tokens =
      si
      |> Map.get(:tokens, [])
      |> Tripwire.check_and_filter(on_leak: :drop)

    context_vec = Map.get(si, :context_vec, [0.0])
    ctx_norm = l2(context_vec)

    candidates =
      case Map.get(si, :sense_candidates) do
        slate when is_map(slate) and map_size(slate) > 0 ->
          candidates_from_slate(si, slate, tokens)

        _ ->
          candidates_from_cells(si, tokens)
      end

    {:ok, out} =
      do_disambiguate_with_reanalysis(si, tokens, candidates, context_vec, ctx_norm, opts)

    ev = %{
      stage: :lifg_stage1,
      normalize: :softmax,
      groups: candidates |> Enum.map(& &1.token_index) |> MapSet.new() |> MapSet.size(),
      scores_mode: out.audit.scores_mode,
      ctx_dim: length(context_vec),
      timing_ms: out.audit.timing_ms,
      choices: out.choices,
      boosts: out.boosts,
      inhibitions: out.inhibitions,
      reanalysis: %{applied: out.audit.reanalysis_applied, max_flips: out.audit.max_flips}
    }

    si
    |> Map.put(:sense_winners, out.winners)
    |> Map.put(:lifg_traces, out.traces)
    |> Map.put(:trace, [ev | Map.get(si, :trace, [])])
    # persist post-tripwire tokens if desired
    |> Map.put(:tokens, tokens)
  end

  @spec do_disambiguate_with_reanalysis(map(), list(), list(), [number()], float(), keyword()) ::
          {:ok,
           %{
             winners: %{optional(non_neg_integer) => map()},
             traces: %{optional(non_neg_integer) => list()},
             choices: [t_choice()],
             boosts: [{binary(), number()}],
             inhibitions: [{binary(), number()}],
             audit: map()
           }}
  defp do_disambiguate_with_reanalysis(si, tokens, candidates, context_vec, ctx_norm, opts) do
    t0 = System.monotonic_time()

    weights = resolved_weights(opts)
    margin_threshold = Keyword.get(opts, :margin_threshold, 0.12)
    scores_mode = Keyword.get(opts, :scores, :all)
    fit? = Keyword.get(opts, :fit?, &__MODULE__.default_fit?/2)
    max_flips = Keyword.get(opts, :max_flips, 1)

    # priming config (opt-in)
    prime_opts = Keyword.get(opts, :prime, [])
    w_prime = Map.get(weights, :prime, 0.0) * 1.0
    bump_after = Keyword.get(prime_opts, :bump_winners, true)

    groups = Enum.group_by(candidates, & &1.token_index)

    {choices, winners, traces, re_count} =
      Enum.reduce(groups, {[], %{}, %{}, 0}, fn {token_idx, cands}, {chs_acc, w_acc, t_acc, rc} ->
        {_cand_feats, normed} =
          score_and_normalize(cands, context_vec, ctx_norm, weights, prime_opts)

        # Attach normalized :score for Reanalysis ordering (using raw score)
        cands_scored =
          Enum.map(normed, fn {cand, f} -> Map.put(cand, :score, f.score_raw) end)

        {result, did_flip} =
          case Reanalysis.pick(cands_scored, si,
                 fit?: fit?,
                 max_flips: max_flips,
                 order: :score_desc
               ) do
            {:ok, winner, trace} -> {{:ok, winner, trace}, trace_has_reject?(trace)}
            {:error, reason, trace} -> {{:error, reason, trace}, trace_has_reject?(trace)}
          end

        rc2 = rc + if(did_flip, do: 1, else: 0)

        # Build choice and optionally bump priming for the accepted winner
        {choice, w_acc2, t_acc2} =
          case result do
            {:ok, winner, trace} ->
              if w_prime > 0.0 and bump_after, do: Priming.bump(winner.id, prime_opts)

              ch =
                build_choice_from_result(
                  token_idx,
                  tokens,
                  normed,
                  winner,
                  margin_threshold,
                  scores_mode
                )

              {ch, Map.put(w_acc, token_idx, winner), Map.put(t_acc, token_idx, trace)}

            {:error, :no_fit, trace} ->
              {best_c, best_f} = best_of_normed(normed)

              ch =
                build_choice_from_pairs(
                  token_idx,
                  tokens,
                  normed,
                  {best_c, best_f},
                  margin_threshold,
                  scores_mode
                )

              {ch, w_acc, Map.put(t_acc, token_idx, trace)}

            {:error, :no_candidates, trace} ->
              {nil, w_acc, Map.put(t_acc, token_idx, trace)}
          end

        chs_acc2 = if choice, do: [choice | chs_acc], else: chs_acc
        {chs_acc2, w_acc2, t_acc2, rc2}
      end)

    choices = Enum.reverse(choices)

    losers =
      for {tidx, cands} <- groups,
          ch = Enum.find(choices, &(&1.token_index == tidx)),
          ch != nil,
          c <- cands,
          c.id != ch.chosen_id,
          uniq: true do
        c.id
      end

    boosts = Enum.map(choices, &{&1.chosen_id, +0.5})
    inhibitions = Enum.map(losers, &{&1, -0.25})

    ms = System.convert_time_unit(System.monotonic_time() - t0, :native, :millisecond)

    audit = %{
      stage: :lifg_stage1,
      normalize: :softmax,
      margin_threshold: margin_threshold,
      weights: weights,
      timing_ms: ms,
      groups: map_size(groups),
      scores_mode: scores_mode,
      ctx_dim: length(context_vec),
      reanalysis_applied: re_count,
      max_flips: max_flips
    }

    {:ok,
     %{
       choices: choices,
       winners: winners,
       traces: traces,
       boosts: boosts,
       inhibitions: inhibitions,
       audit: audit
     }}
  end

  # ----- candidates from SenseSlate (preferred) -----
  defp candidates_from_slate(si, slate, tokens) do
    Enum.flat_map(slate, fn {idx, list} ->
      lemma = lemma_for(tokens, idx)

      Enum.map(List.wrap(list), fn c ->
        feats = Map.get(c, :features, %{}) || %{}

        %{
          id: Map.get(c, :id),
          token_index: idx,
          lemma: lemma || "",
          pos: Map.get(feats, :pos),
          lex_fit: Map.get(feats, :lex_fit, 0.6),
          rel_prior: Map.get(c, :prior, 0.5),
          intent_bias: intent_bias_from(si, c, feats),
          activation: Map.get(feats, :activation, 0.0),
          embedding: Map.get(feats, :embedding)
        }
      end)
    end)
  end

  # ----- legacy: derive candidates by matching tokens to active_cells -----
  defp candidates_from_cells(si, tokens) do
    cells = Map.get(si, :active_cells, [])

    norm = fn s ->
      s |> to_b() |> String.downcase() |> String.replace(~r/\s+/, " ") |> String.trim()
    end

    cells_normed =
      Enum.map(cells, fn m ->
        word = getf(m, :word) || ""
        lemma = getf(m, :lemma) || word
        nrm = getf(m, :norm) || word

        %{
          id: getf(m, :id),
          pos: getf(m, :pos),
          lex_fit: getf(m, :lex_fit),
          rel_prior: getf(m, :rel_prior),
          intent_bias: getf(m, :intent_bias),
          activation: getf(m, :activation),
          embedding: getf(m, :embedding),
          __norm__: norm.(nrm),
          __lemma__: lemma
        }
      end)

    tokens
    |> Enum.with_index()
    |> Enum.flat_map(fn {t, idx} ->
      tnorm = norm.(Map.get(t, :phrase, ""))

      Enum.filter(cells_normed, fn c ->
        c.__norm__ == tnorm or String.contains?(tnorm, c.__norm__) or
          String.contains?(c.__norm__, tnorm)
      end)
      |> Enum.map(fn c ->
        %{
          id: c.id,
          token_index: idx,
          lemma: c.__lemma__ || "",
          pos: c.pos,
          lex_fit: c.lex_fit || 0.6,
          rel_prior: c.rel_prior || 0.5,
          intent_bias: c.intent_bias || 0.0,
          activation: c.activation || 0.0,
          embedding: c.embedding
        }
      end)
    end)
  end

  # Intent bias hook (simple placeholder)
  defp intent_bias_from(si, _cand, _feats) do
    case Map.get(si, :intent) do
      nil -> 0.0
      _ -> 0.0
    end
  end

  # ---- scoring / normalization helpers ----
  @doc "Default weights."
  @spec default_weights() :: %{
          lex: float(),
          sim: float(),
          rel: float(),
          prag: float(),
          act: float()
        }
  def default_weights, do: %{lex: 0.25, sim: 0.40, rel: 0.15, prag: 0.10, act: 0.10}

  @doc "Cosine similarity; 0.0 on nil/zero-norm."
  @spec cosine([number()] | nil, [number()] | nil) :: float()
  def cosine(nil, _), do: 0.0
  def cosine(_, nil), do: 0.0

  def cosine(a, b) when is_list(a) and is_list(b) do
    num = dot(a, b)
    den = l2(a) * l2(b)
    if zeroish?(den, den), do: 0.0, else: num / den
  end

  @spec normalize_scores([number()]) :: [float()]
  def normalize_scores(scores) do
    m = Enum.max([0.0 | scores])
    exps = Enum.map(scores, fn s -> :math.exp(s - m) end)
    z = Enum.sum(exps)
    if zeroish?(z, z), do: List.duplicate(0.0, length(scores)), else: Enum.map(exps, &(&1 / z))
  end

  defp score_and_normalize(cands, ctx, ctx_norm, weights, prime_opts) do
    w_prime = Map.get(weights, :prime, 0.0) * 1.0

    cand_feats =
      Enum.map(cands, fn cand ->
        sim =
          case cand.embedding do
            nil -> 0.0
            vec -> cosine_with_ctxnorm(vec, ctx, ctx_norm)
          end

        prime =
          if w_prime == 0.0 do
            0.0
          else
            Brain.LIFG.Priming.boost_for(to_string(Map.get(cand, :id, "")), prime_opts)
          end

        score =
          weights.lex * safe_num(cand.lex_fit) +
            weights.sim * sim +
            weights.rel * safe_num(cand.rel_prior) +
            weights.prag * safe_num(cand.intent_bias) +
            weights.act * safe_num(cand.activation) +
            w_prime * prime

        {cand, %{sim: sim, prime: prime, score_raw: score}}
      end)

    normed_vals = normalize_scores(Enum.map(cand_feats, fn {_c, f} -> f.score_raw end))

    normed =
      Enum.zip(cand_feats, normed_vals)
      |> Enum.map(fn {{cand, feats}, s_norm} ->
        {cand, Map.put(feats, :score_norm, s_norm)}
      end)

    {cand_feats, normed}
  end

  # ---- choice builders (winner-aware) ----
  defp build_choice_from_result(token_idx, tokens, normed, winner, margin_threshold, scores_mode) do
    sorted = Enum.sort_by(normed, fn {_c, f} -> -f.score_norm end)

    {chosen_c, chosen_f} =
      Enum.find_value(sorted, fn {c, f} -> if c.id == winner.id, do: {c, f}, else: nil end) ||
        best_of_normed(sorted)

    {_ru_c, ru_f} =
      Enum.find_value(sorted, fn {c, f} -> if c.id != winner.id, do: {c, f}, else: nil end) ||
        {nil, %{score_norm: 0.0}}

    margin = chosen_f.score_norm - (ru_f[:score_norm] || 0.0)

    alt_ids =
      if margin < margin_threshold do
        case Enum.find_value(sorted, fn {c, _f} -> if c.id != winner.id, do: c.id, else: nil end) do
          nil -> []
          id -> [id]
        end
      else
        []
      end

    lemma = lemma_for(tokens, token_idx) || (chosen_c[:lemma] || "")

    %__MODULE__.SenseChoice{
      token_index: token_idx,
      lemma: lemma,
      chosen_id: winner.id,
      alt_ids: alt_ids,
      scores: scores_map_from_normed(normed, scores_mode, nil),
      margin: margin,
      features: base_features(chosen_c, chosen_f)
    }
  end

  defp build_choice_from_pairs(
         token_idx,
         tokens,
         normed,
         {best_c, best_f},
         margin_threshold,
         scores_mode
       ) do
    sorted = Enum.sort_by(normed, fn {_c, f} -> -f.score_norm end)

    {_ru_c, ru_f} =
      case sorted do
        [_only] -> {nil, %{score_norm: 0.0}}
        [_, {c2, f2} | _] -> {c2, f2}
        _ -> {nil, %{score_norm: 0.0}}
      end

    margin = best_f.score_norm - (ru_f[:score_norm] || 0.0)

    alt_ids =
      if margin < margin_threshold and length(sorted) > 1 do
        [elem(Enum.at(sorted, 1), 0).id]
      else
        []
      end

    lemma = lemma_for(tokens, token_idx) || (best_c[:lemma] || "")

    %__MODULE__.SenseChoice{
      token_index: token_idx,
      lemma: lemma,
      chosen_id: best_c.id,
      alt_ids: alt_ids,
      scores: scores_map_from_normed(normed, scores_mode, [{best_c, best_f}]),
      margin: margin,
      features: base_features(best_c, best_f)
    }
  end

  defp best_of_normed([]), do: {nil, %{score_norm: 0.0}}

  defp best_of_normed(list),
    do: Enum.max_by(list, fn {_c, f} -> f.score_norm end, fn -> hd(list) end)

  defp scores_map_from_normed(normed, :all, _pairs),
    do: Enum.map(normed, fn {c, f} -> {c.id, f.score_norm} end) |> Map.new()

  defp scores_map_from_normed(_normed, :top2, pairs) when is_list(pairs),
    do: Enum.map(pairs, fn {c, f} -> {c.id, f.score_norm} end) |> Map.new()

  defp scores_map_from_normed(_normed, _mode, _pairs), do: %{}

  defp base_features(cand, feats) do
    %{
      sim: feats.sim,
      score_raw: feats.score_raw,
      score_norm: feats.score_norm,
      pos: cand[:pos],
      lex_fit: cand[:lex_fit],
      rel_prior: cand[:rel_prior],
      intent_bias: cand[:intent_bias],
      activation: cand[:activation]
    }
  end

  # ---- small utils ----
  def default_fit?(_candidate, _si), do: true
  defp trace_has_reject?(trace), do: Enum.any?(trace, fn {tag, _} -> tag == :reject end)

  defp lemma_for(tokens, idx) do
    case Enum.at(tokens, idx) do
      %{phrase: p} when is_binary(p) -> p
      _ -> nil
    end
  end

  # Single, epsilon-safe version. Avoids head pattern-matching on 0.0.
  defp cosine_with_ctxnorm(vec, ctx, ctxn) do
    cond do
      is_nil(vec) or is_nil(ctx) ->
        0.0

      zeroish?(ctxn, ctxn) ->
        0.0

      true ->
        na = l2(vec)
        den = na * ctxn
        if zeroish?(den, den), do: 0.0, else: dot(vec, ctx) / den
    end
  end

  defp dot(a, b), do: Enum.zip(a, b) |> Enum.reduce(0.0, fn {x, y}, acc -> acc + x * y end)
  defp l2(v), do: v |> Enum.reduce(0.0, fn x, acc -> acc + x * x end) |> :math.sqrt()
  defp safe_num(nil), do: 0.0
  defp safe_num(x) when is_number(x), do: x * 1.0
  defp getf(m, k) when is_atom(k), do: Map.get(m, k) || Map.get(m, Atom.to_string(k))
  defp getf(m, k) when is_binary(k), do: Map.get(m, k)
  defp to_b(nil), do: ""
  defp to_b(v) when is_binary(v), do: v
  defp to_b(v), do: Kernel.to_string(v)

  # Reads defaults, then merges config, then per-call overrides.
  defp resolved_weights(opts) do
    default_weights()
    |> Map.merge(Map.new(Application.get_env(:brain, :lifg_weights, %{})))
    |> Map.merge(Map.new(Keyword.get(opts, :weights, %{})))
  end
end
