defmodule Brain.LIFG do
  @moduledoc """
  Stage-1 Disambiguation (LIFG). Pure, stateless, fast.

  Public API:
    • `disambiguate_stage1/1` — pipeline entry: SI → SI
      (results are attached as a trace event that includes `choices`, `boosts`, `inhibitions`)

  Scoring recipe (weights tunable):
      score = w_lex*lex_fit
            + w_sim*cosine(context_vec, embedding)
            + w_rel*rel_prior
            + w_prag*intent_bias
            + w_act*activation

  Defaults: w_lex=0.25, w_sim=0.40, w_rel=0.15, w_prag=0.10, w_act=0.10.
  Softmax per token group → probabilities sum ≈ 1.0.
  """

  alias Core.SemanticInput
  alias __MODULE__.SenseChoice

  @typedoc "Per-token choice."
  @type t_choice :: %SenseChoice{
          token_index: non_neg_integer(),
          lemma: binary(),
          chosen_id: binary(),
          alt_ids: [binary()],
          scores: %{optional(binary()) => number()},
          margin: number(),
          features: map()
        }

  defmodule SenseChoice do
    @enforce_keys [:token_index, :lemma, :chosen_id, :scores, :margin, :features]
    defstruct token_index: 0,
              lemma: "",
              chosen_id: "",
              alt_ids: [],
              scores: %{},
              margin: 0.0,
              features: %{}
  end

  # ─────────────────────────────
  # SI → SI (pipeline-safe)
  # ─────────────────────────────
  @doc "Consume SI, compute LIFG choices, append a trace event, return SI."
  @spec disambiguate_stage1(SemanticInput.t()) :: SemanticInput.t()
  def disambiguate_stage1(%SemanticInput{} = si) do
    tokens = si.tokens || []
    cells  = si.active_cells || []
    ctx    = Map.get(si, :context_vec, [0.0])

    norm = fn s ->
      s
      |> to_string()
      |> String.downcase()
      |> String.replace(~r/\s+/, " ")
      |> String.trim()
    end

    cells_normed =
      Enum.map(cells, fn m ->
        word = m[:word] || m["word"] || ""
        n    = m[:norm] || m["norm"] || word
        Map.merge(m, %{__norm__: norm.(n), __lemma__: word})
      end)

    candidates =
      tokens
      |> Enum.with_index()
      |> Enum.flat_map(fn {t, idx} ->
        tnorm = norm.(Map.get(t, :phrase, ""))

        cells_normed
        |> Enum.filter(fn c ->
          c.__norm__ == tnorm or
            String.contains?(tnorm, c.__norm__) or
            String.contains?(c.__norm__, tnorm)
        end)
        |> Enum.map(fn c ->
          %{
            id: c[:id] || c["id"],
            token_index: idx,
            lemma: c.__lemma__ || "",
            pos: c[:pos] || c["pos"],
            lex_fit: c[:lex_fit] || 0.6,
            rel_prior: c[:rel_prior] || 0.5,
            intent_bias: c[:intent_bias] || 0.0,
            activation: c[:activation] || 0.0,
            embedding: c[:embedding] # may be nil
          }
        end)
      end)

    # Use private scorer (no public /3)
    {:ok, out} = do_disambiguate(candidates, ctx, [])

    ev = %{
      stage: :lifg_stage1,
      normalize: :softmax,
      groups: candidates |> Enum.map(& &1.token_index) |> MapSet.new() |> MapSet.size(),
      scores_mode: :all,
      ctx_dim: length(ctx),
      timing_ms: out.audit.timing_ms,
      choices: out.choices,
      boosts: out.boosts,
      inhibitions: out.inhibitions
    }

    %SemanticInput{si | trace: [ev | (si.trace || [])]}
  end

  # ─────────────────────────────
  # Private scorer
  # ─────────────────────────────
  @spec do_disambiguate([map()], [number()], keyword()) ::
          {:ok,
           %{
             choices: [t_choice()],
             boosts: [{binary(), number()}],
             inhibitions: [{binary(), number()}],
             audit: map()
           }}
  defp do_disambiguate(candidates, context_vec, opts) when is_list(candidates) do
    t0 = System.monotonic_time()

    weights          = Map.merge(default_weights(), Map.new(Keyword.get(opts, :weights, %{})))
    margin_threshold = Keyword.get(opts, :margin_threshold, 0.12)
    scores_mode      = Keyword.get(opts, :scores, :all)

    groups   = Enum.group_by(candidates, & &1.token_index)
    ctx_norm = l2(context_vec)

    choices =
      groups
      |> Enum.map(fn {token_idx, cands} ->
        {_feats, normed} = score_and_normalize(cands, context_vec, ctx_norm, weights)
        build_choice(token_idx, normed, margin_threshold, scores_mode)
      end)
      |> Enum.reject(&is_nil/1)

    choices_by_token = Map.new(choices, &{&1.token_index, &1})

    losers =
      for {tidx, cands} <- groups,
          ch = Map.get(choices_by_token, tidx),
          ch != nil,
          c <- cands,
          c.id != ch.chosen_id,
          uniq: true do
        c.id
      end

    boosts      = Enum.map(choices, &{&1.chosen_id, +0.5})
    inhibitions = Enum.map(losers,  &{&1, -0.25})

    ms = System.convert_time_unit(System.monotonic_time() - t0, :native, :millisecond)

    audit = %{
      stage: :lifg_stage1,
      normalize: :softmax,
      margin_threshold: margin_threshold,
      weights: weights,
      timing_ms: ms,
      groups: map_size(groups),
      scores_mode: scores_mode,
      ctx_dim: length(context_vec)
    }

    {:ok, %{choices: choices, boosts: boosts, inhibitions: inhibitions, audit: audit}}
  end

  @doc "Default weights."
  @spec default_weights() :: %{lex: float(), sim: float(), rel: float(), prag: float(), act: float()}
  def default_weights, do: %{lex: 0.25, sim: 0.40, rel: 0.15, prag: 0.10, act: 0.10}

  @doc "Cosine similarity; 0.0 on nil/zero-norm."
  @spec cosine([number()] | nil, [number()] | nil) :: float()
  def cosine(nil, _), do: 0.0
  def cosine(_, nil), do: 0.0
  def cosine(a, b) when is_list(a) and is_list(b) do
    dot = dot(a, b)
    na = l2(a)
    nb = l2(b)
    if na == 0.0 or nb == 0.0, do: 0.0, else: dot / (na * nb)
  end

  @spec normalize_scores([number()]) :: [float()]
  def normalize_scores(scores) do
    m    = Enum.max([0.0 | scores])
    exps = Enum.map(scores, fn s -> :math.exp(s - m) end)
    z    = Enum.sum(exps)
    if z == 0.0, do: List.duplicate(0.0, length(scores)), else: Enum.map(exps, &(&1 / z))
  end

  # ───────────── internals ─────────────

  defp score_and_normalize(cands, ctx, ctx_norm, weights) do
    cand_feats =
      Enum.map(cands, fn cand ->
        sim =
          case cand[:embedding] do
            nil -> 0.0
            vec -> cosine_with_ctxnorm(vec, ctx, ctx_norm)
          end

        score =
          weights.lex * safe_num(cand[:lex_fit]) +
            weights.sim * sim +
            weights.rel * safe_num(cand[:rel_prior]) +
            weights.prag * safe_num(cand[:intent_bias]) +
            weights.act * safe_num(cand[:activation])

        {cand, %{sim: sim, score_raw: score}}
      end)

    normed_vals = normalize_scores(Enum.map(cand_feats, fn {_c, f} -> f.score_raw end))

    normed =
      Enum.zip(cand_feats, normed_vals)
      |> Enum.map(fn {{cand, feats}, s_norm} ->
        {cand, Map.put(feats, :score_norm, s_norm)}
      end)

    {cand_feats, normed}
  end

  defp build_choice(token_idx, normed, margin_threshold, scores_mode) do
    case top2_unordered(normed) do
      :none ->
        nil

      {:one, {cand1, f1}} ->
        %SenseChoice{
          token_index: token_idx,
          lemma: cand1.lemma || "",
          chosen_id: cand1.id,
          alt_ids: [],
          scores: scores_map_from_normed(normed, scores_mode, [{cand1, f1}]),
          margin: 0.0,
          features: base_features(cand1, f1)
        }

      {:two, {cand1, f1}, {cand2, f2}} ->
        margin  = f1.score_norm - f2.score_norm
        alt_ids = if margin < margin_threshold, do: [cand2.id], else: []

        %SenseChoice{
          token_index: token_idx,
          lemma: cand1.lemma || "",
          chosen_id: cand1.id,
          alt_ids: alt_ids,
          scores: scores_map_from_normed(normed, scores_mode, [{cand1, f1}, {cand2, f2}]),
          margin: margin,
          features: base_features(cand1, f1)
        }
    end
  end

  defp scores_map_from_normed(normed, :all, _pairs), do: Enum.map(normed, fn {c, f} -> {c.id, f.score_norm} end) |> Map.new()
  defp scores_map_from_normed(_normed, :top2, pairs), do: Enum.map(pairs, fn {c, f} -> {c.id, f.score_norm} end) |> Map.new()
  defp scores_map_from_normed(_normed, :none, _pairs), do: %{}

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

  # pick best & runner-up by score_norm; stable tie-break by id
  defp top2_unordered([]), do: :none
  defp top2_unordered([one]), do: {:one, one}
  defp top2_unordered([first, second | rest]) do
    {b_c, b_f, s_c, s_f} =
      Enum.reduce(rest, init_order(first, second), fn {c, f}, {bc, bf, sc, sf} ->
        cond do
          better?({c, f}, {bc, bf}) -> {c, f, bc, bf}
          sc == nil                 -> {bc, bf, c, f}
          better?({c, f}, {sc, sf}) -> {bc, bf, c, f}
          true                      -> {bc, bf, sc, sf}
        end
      end)

    {:two, {b_c, b_f}, {s_c, s_f}}
  end

  defp init_order({c1, f1}, {c2, f2}) do
    if better?({c1, f1}, {c2, f2}), do: {c1, f1, c2, f2}, else: {c2, f2, c1, f1}
  end

  defp better?({_c1, f1}, {_c2, f2}) when f1.score_norm > f2.score_norm, do: true
  defp better?({_c1, f1}, {_c2, f2}) when f1.score_norm < f2.score_norm, do: false
  defp better?({c1, _f1}, {c2, _f2}), do: c1.id <= c2.id

  defp cosine_with_ctxnorm(_vec, _ctx, 0.0), do: 0.0
  defp cosine_with_ctxnorm(nil, _ctx, _ctxn), do: 0.0
  defp cosine_with_ctxnorm(vec, ctx, ctxn) do
    na = l2(vec)
    if na == 0.0, do: 0.0, else: dot(vec, ctx) / (na * ctxn)
  end

  defp dot(a, b), do: Enum.zip(a, b) |> Enum.reduce(0.0, fn {x, y}, acc -> acc + x * y end)
  defp l2(v),      do: v |> Enum.reduce(0.0, fn x, acc -> acc + x * x end) |> :math.sqrt()
  defp safe_num(nil), do: 0.0
  defp safe_num(x) when is_number(x), do: x * 1.0
end

