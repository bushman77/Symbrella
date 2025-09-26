
defmodule Brain.LIFG do
  @moduledoc """
  Stage‑1 **Disambiguation** (Controlled Retrieval & Selection) inspired by the
  **Left Inferior Frontal Gyrus (LIFG)**. Pure, stateless, fast.

  ## What it does
  Given a batch of **candidate senses** (already active in STM) and a
  **context vector**, it scores and normalizes senses *within each token*
  group, chooses a winner (optionally keeping runner‑up when margin is small),
  and emits **control signals** (boosts for winners, inhibitions for others).

  ## Score recipe (weights are tunable)
      score = w_lex*lex_fit
            + w_sim*cosine(context_vec, sense_vec)
            + w_rel*rel_prior
            + w_prag*intent_bias
            + w_act*activation

  Defaults: `w_lex=0.25, w_sim=0.40, w_rel=0.15, w_prag=0.10, w_act=0.10`.

  ## Candidate shape
      %{
        id: binary(),
        token_index: non_neg_integer(),
        lemma: binary(),
        pos: binary(),
        embedding: [number()] | nil,
        embedding_id: any() | nil,
        lex_fit: number(),       # 0..1
        rel_prior: number(),     # 0..1
        intent_bias: number(),   # 0..1
        activation: number()     # 0..1
      }

  ## API
      disambiguate_stage1(candidates, context_vec, opts \\ []) :: {:ok, result}

  ### Options
    * `:weights` — map override for weights
    * `:normalize` — `:softmax` (default) | `:maxnorm`
    * `:margin_threshold` — default `0.12`
    * `:boost_delta` — default `+0.5`
    * `:inhibit_delta` — default `-0.25`
    * `:vector_lookup` — `fn embedding_id -> [floats] end` (for lazy embeddings)
    * `:scores` — `:all` (default) | `:top2` | `:none` (controls how much score detail to carry)
    * `:parallel` — `false` (default). When true, groups run via `Task.async_stream/3`.
      Use for large G/dims; overhead can dominate tiny batches.
    * `:max_concurrency` — when `:parallel`, overrides the concurrency level.

  ### Result
      %{
        choices: [%SenseChoice{}],
        boosts: [{id, float}],
        inhibitions: [{id, float}],
        audit: %{...}
      }

  ## Doctest
      iex> cands = [
      ...>   %{id: "w0a", token_index: 0, lemma: "bank", pos: "noun",
      ...>     embedding: [1.0, 0.0], lex_fit: 0.9, rel_prior: 0.7, intent_bias: 0.6, activation: 0.5},
      ...>   %{id: "w0b", token_index: 0, lemma: "bank", pos: "noun",
      ...>     embedding: [0.0, 1.0], lex_fit: 0.8, rel_prior: 0.4, intent_bias: 0.3, activation: 0.4}
      ...> ]
      iex> {:ok, out} = Brain.LIFG.disambiguate_stage1(cands, [0.9, 0.1], margin_threshold: 0.05)
      iex> Enum.map(out.choices, & &1.chosen_id)
      ["w0a"]
  """

  alias __MODULE__.SenseChoice

  @typedoc """
  Per‑token choice.
  """
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

  @spec disambiguate_stage1([map()], [number()], keyword() | map()) ::
          {:ok,
           %{
             choices: [t_choice()],
             boosts: [{binary(), number()}],
             inhibitions: [{binary(), number()}],
             audit: map()
           }}
  def disambiguate_stage1(candidates, context_vec, opts \\ []) when is_list(candidates) do
    t0 = System.monotonic_time()

    weights = Map.merge(default_weights(), Map.new(Keyword.get(opts, :weights, %{})))
    normalize = Keyword.get(opts, :normalize, :softmax)
    margin_threshold = Keyword.get(opts, :margin_threshold, 0.12)
    boost_delta = Keyword.get(opts, :boost_delta, 0.5)
    inhibit_delta = Keyword.get(opts, :inhibit_delta, -0.25)
    vector_lookup = Keyword.get(opts, :vector_lookup, nil)
    scores_mode = Keyword.get(opts, :scores, :all)
    parallel? = Keyword.get(opts, :parallel, false)
    max_conc = Keyword.get(opts, :max_concurrency, System.schedulers_online())

    # group by token_index
    groups = Enum.group_by(candidates, & &1.token_index)

    ctx_norm = l2(context_vec)

    chooser = fn {token_idx, cands} ->
      {cand_feats, normed} = score_and_normalize(cands, context_vec, ctx_norm, vector_lookup, weights, normalize)
      build_choice(token_idx, cand_feats, normed, margin_threshold, scores_mode)
    end

    choices =
      if parallel? do
        groups
        |> Task.async_stream(chooser,
              max_concurrency: max_conc,
              timeout: :infinity)
        |> Enum.map(fn {:ok, choice} -> choice end)
      else
        Enum.map(groups, chooser)
      end

    # control signals using group membership (independent of scores_mode)
    choices_by_token = Map.new(choices, &{&1.token_index, &1})
    losers =
      for {tidx, cands} <- groups,
          c <- cands,
          ch = Map.fetch!(choices_by_token, tidx),
          c.id != ch.chosen_id,
          uniq: true do
        c.id
      end

    boosts = Enum.map(choices, &{&1.chosen_id, boost_delta})
    inhibitions = Enum.map(losers, &{&1, inhibit_delta})

    t1 = System.monotonic_time()
    ms = System.convert_time_unit(t1 - t0, :native, :millisecond)

    audit = %{
      stage: :lifg_stage1,
      normalize: normalize,
      margin_threshold: margin_threshold,
      weights: weights,
      timing_ms: ms,
      groups: map_size(groups),
      scores_mode: scores_mode,
      parallel: parallel?,
      max_concurrency: max_conc,
      ctx_dim: length(context_vec)
    }

    {:ok, %{choices: choices, boosts: boosts, inhibitions: inhibitions, audit: audit}}
  end

  @doc "Default weights used by `disambiguate_stage1/3`."
  @spec default_weights() :: %{lex: float(), sim: float(), rel: float(), prag: float(), act: float()}
  def default_weights, do: %{lex: 0.25, sim: 0.40, rel: 0.15, prag: 0.10, act: 0.10}

  @doc """
  Cosine similarity between two vectors. Returns `0.0` on nil or zero‑norm.
  """
  @spec cosine([number()] | nil, [number()] | nil) :: float()
  def cosine(nil, _), do: 0.0
  def cosine(_, nil), do: 0.0
  def cosine(a, b) when is_list(a) and is_list(b) do
    dot = dot(a, b)
    na = l2(a)
    nb = l2(b)
    if na == 0.0 or nb == 0.0, do: 0.0, else: dot / (na * nb)
  end

  @doc """
  Normalize a list of scores:
    * `:softmax` — numerically stable softmax
    * `:maxnorm` — divide by max (or zeros)
  """
  @spec normalize_scores([number()], :softmax | :maxnorm) :: [float()]
  def normalize_scores(scores, :maxnorm) do
    maxv = Enum.max([0.0 | scores])
    if maxv == 0.0, do: List.duplicate(0.0, length(scores)), else: Enum.map(scores, &(&1 / maxv))
  end
  def normalize_scores(scores, :softmax) do
    m = Enum.max([0.0 | scores])
    exps = Enum.map(scores, fn s -> :math.exp(s - m) end)
    z = Enum.sum(exps)
    if z == 0.0, do: List.duplicate(0.0, length(scores)), else: Enum.map(exps, &(&1 / z))
  end

  # ─────────────── internals ───────────────

  defp score_and_normalize(cands, ctx, ctx_norm, vector_lookup, weights, normalize) do
    # score raw
    cand_feats =
      Enum.map(cands, fn cand ->
        vec =
          cond do
            is_list(cand[:embedding]) -> cand[:embedding]
            cand[:embedding_id] && is_function(vector_lookup, 1) -> safe_lookup(vector_lookup, cand.embedding_id)
            true -> nil
          end

        sim = cosine_with_ctxnorm(vec, ctx, ctx_norm)

        score =
          weights.lex * safe_num(cand[:lex_fit]) +
            weights.sim * sim +
            weights.rel * safe_num(cand[:rel_prior]) +
            weights.prag * safe_num(cand[:intent_bias]) +
            weights.act * safe_num(cand[:activation])

        {cand, %{sim: sim, score_raw: score}}
      end)

    # normalize
    normed_vals = normalize_scores(Enum.map(cand_feats, fn {_c, f} -> f.score_raw end), normalize)

    normed =
      Enum.zip(cand_feats, normed_vals)
      |> Enum.map(fn {{cand, feats}, s_norm} ->
        {cand, Map.put(feats, :score_norm, s_norm)}
      end)

    {cand_feats, normed}
  end

  defp build_choice(token_idx, cand_feats, normed, margin_threshold, scores_mode) do
    case top2_unordered(normed) do
      :none ->
        nil

      {:one, {cand1, f1}} ->
        scores_map = build_scores_map([{cand1, f1}], scores_mode)
        %SenseChoice{
          token_index: token_idx,
          lemma: cand1.lemma || "",
          chosen_id: cand1.id,
          alt_ids: [],
          scores: scores_map,
          margin: 0.0,
          features: base_features(cand1, f1)
        }

      {:two, {cand1, f1}, {cand2, f2}} ->
        margin = f1.score_norm - f2.score_norm
        alt_ids = if margin < margin_threshold, do: [cand2.id], else: []
        scores_map = build_scores_map([{cand1, f1}, {cand2, f2} | rest_fill(normed, scores_mode)], scores_mode)

        %SenseChoice{
          token_index: token_idx,
          lemma: cand1.lemma || "",
          chosen_id: cand1.id,
          alt_ids: alt_ids,
          scores: scores_map,
          margin: margin,
          features: base_features(cand1, f1)
        }
    end
  end

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

  # For :all we want the full map; for :top2 we pass only the given list;
  # for :none we pass empty.
  defp build_scores_map(pairs, :none), do: %{}
  defp build_scores_map(pairs, _mode) do
    pairs |> Enum.map(fn {c, f} -> {c.id, f.score_norm} end) |> Map.new()
  end

  # If scores_mode is :all, we need the whole list; else we don't.
  defp rest_fill(_normed, :all), do: []
  defp rest_fill(_normed, _), do: []

  # Single pass selection of top‑2 by score_norm.
  defp top2_unordered([]), do: :none
  defp top2_unordered([one]), do: {:one, one}
  defp top2_unordered([first, second | rest]) do
    {b_c, b_f, s_c, s_f} =
      Enum.reduce(rest, init_order(first, second), fn {c, f}, {bc, bf, sc, sf} ->
        if f.score_norm > bf.score_norm do
          {c, f, bc, bf}
        else
          if sf == nil or f.score_norm > sf.score_norm, do: {bc, bf, c, f}, else: {bc, bf, sc, sf}
        end
      end)

    {:two, {b_c, b_f}, {s_c, s_f}}
  end

  defp init_order({c1, f1}, {c2, f2}) do
    if f1.score_norm >= f2.score_norm, do: {c1, f1, c2, f2}, else: {c2, f2, c1, f1}
  end

  defp cosine_with_ctxnorm(nil, _ctx, _ctxn), do: 0.0
  defp cosine_with_ctxnorm(_vec, _ctx, 0.0), do: 0.0
  defp cosine_with_ctxnorm(vec, ctx, ctxn) do
    na = l2(vec)
    if na == 0.0, do: 0.0, else: dot(vec, ctx) / (na * ctxn)
  end

  defp safe_lookup(fun, id) do
    try do
      fun.(id)
    rescue
      _ -> nil
    end
  end

  defp dot(a, b), do: Enum.zip(a, b) |> Enum.reduce(0.0, fn {x, y}, acc -> acc + x * y end)
  defp l2(v), do: v |> Enum.reduce(0.0, fn x, acc -> acc + x * x end) |> :math.sqrt()

  defp safe_num(nil), do: 0.0
  defp safe_num(x) when is_number(x), do: x * 1.0
end
