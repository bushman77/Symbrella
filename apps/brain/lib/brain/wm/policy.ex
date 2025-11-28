defmodule Brain.WM.Policy do
  @moduledoc """
  Working-Memory admission policy.

  Responsibilities:
  • Compute gate scores for candidates (score shaping)
  • Decide allow/block/boost using thresholds and budgets
  • Helpers (lemma budget, semantic bias, fallbacks)

  This keeps your **public API** intact:
    - acceptable_candidate?/2
    - gate_score_for/3
    - decide_gate_policy/4

  Enhancements:
  • Recency decay (seconds-scale) via `cand[:ts_ms]`
  • Novelty boost using recent WM contents (computed in decide/4)
  • Intent nudge (light preference for current intent)
  • Outcome uplift (bounded, from `cand[:episodes]` or `cand[:episode_score]`)
  • Preserves fallback scaling, lemma budget, and source preferences
  """

  alias Brain.Utils.Numbers

  @type cfg ::
          %{
            required(:gate_threshold) => number(),
            required(:fallback_scale) => number(),
            required(:lemma_budget) => pos_integer(),
            required(:replace_margin) => number(),
            required(:allow_unk?) => boolean(),
            required(:allow_seed?) => boolean(),
            required(:allow_fallback_into_wm?) => boolean(),
            optional(:half_life_ms) => pos_integer(),
            optional(:novelty_window) => non_neg_integer(),
            optional(:novelty_weight) => number(),
            optional(:intent_weight) => number(),
            optional(:recency_weight) => number(),
            optional(:outcome_weight) => number(),
            optional(:current_intent) => atom() | nil,
            optional(:semantic_boost) => number()
          }

  # >>> NEW: default gating config used to fill in missing keys <<<
  @default_cfg %{
    gate_threshold: 0.0,
    fallback_scale: 0.70,
    lemma_budget: 16,
    replace_margin: 0.10,
    allow_unk?: true,
    allow_seed?: true,
    allow_fallback_into_wm?: true,
    half_life_ms: 7_500,
    novelty_window: 16,
    novelty_weight: 0.15,
    intent_weight: 0.05,
    recency_weight: 0.10,
    outcome_weight: 0.10,
    current_intent: nil,
    semantic_boost: 0.10
  }

  # Normalize *any* cfg (even %{capacity: 3, decay_ms: 8000}) into a full cfg
  defp normalize_cfg(nil), do: @default_cfg
  defp normalize_cfg(cfg) when is_map(cfg), do: Map.merge(@default_cfg, cfg)

  # --- Public API -------------------------------------------------------------

  @spec acceptable_candidate?(map(), cfg()) :: boolean()
  def acceptable_candidate?(cand, cfg) do
    cfg = normalize_cfg(cfg)

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

  @doc """
  Compute a *base* gate score for `cand`.

  Inputs:
    • cand[:score] / cand[:activation_snapshot]   → base signal
    • `salience` (0..1)                            → external salience (e.g., attention/conflict)
    • Recency, intent, semantic_bias, fallback scaling, source preference

  Note:
    • Novelty (requires WM context) and outcome uplift are added in decide/4.
  """
  @spec gate_score_for(map(), number(), cfg()) :: float()
  def gate_score_for(cand, salience, cfg) do
    cfg = normalize_cfg(cfg)

    base =
      (cand[:score] || cand[:activation_snapshot] || 0.0) * 1.0

    prefer =
      if cand[:source] in [:runtime, :recency, :lifg, :ltm], do: 0.10, else: 0.0

    b_scaled =
      if fallback_id?(cand[:id]) do
        scale = Map.get(cfg, :fallback_scale, 0.70)
        base * scale
      else
        base
      end

    sem_boost =
      case safe_sem_bias(cand) do
        b when is_number(b) -> Map.get(cfg, :semantic_boost, 0.10) * Numbers.clamp01(b)
        _ -> 0.0
      end

    # --- Recency & Intent nudges (light, bounded) ---
    recency_nudge = recency_nudge(cand, cfg)
    intent_nudge = intent_nudge(cand, cfg)

    b_scaled
    |> Kernel.+(0.5 * Numbers.clamp01(salience))
    |> Kernel.+(prefer)
    |> Kernel.+(sem_boost)
    |> Kernel.+(recency_nudge)
    |> Kernel.+(intent_nudge)
    |> Kernel.-(diversity_penalty(cand, cfg))
    |> Numbers.clamp01()
  end

  @doc """
  Decide gate policy using the base `gate_score` plus WM-aware terms.

  Adds:
    • Novelty boost (vs. recent WM contents)
    • Outcome uplift (bounded)
  Respects:
    • Lemma budget (replace only if margin beat)
    • Fallback rules and gate threshold
    • Source preference bump

  Returns `{decision, final_score}` where decision ∈ `:allow | :block | :boost`.
  """
  @spec decide_gate_policy([map()], map(), float(), cfg()) ::
          {:allow | :block | :boost, float()}
  def decide_gate_policy(wm, cand, gate_score, cfg) do
    cfg = normalize_cfg(cfg)

    prefer_source? = cand[:source] in [:runtime, :recency, :lifg, :ltm]
    thr = Map.fetch!(cfg, :gate_threshold)
    allow_fallback? = Map.get(cfg, :allow_fallback_into_wm?, false)
    is_fallback = fallback_id?(cand[:id])

    {within_budget?, beats_by?} = within_lemma_budget?(wm, cand, cfg)

    # WM-aware shaping (computed here because novelty needs WM context)
    novelty_boost =
      novelty_boost(wm, cand, cfg)

    outcome_uplift =
      outcome_uplift(cand, cfg)

    final_score =
      gate_score
      |> Kernel.+(novelty_boost)
      |> Kernel.+(outcome_uplift)
      |> Numbers.clamp01()

    cond do
      not within_budget? and not beats_by? ->
        {:block, final_score}

      # P-206: fallbacks must meet normal threshold unless explicitly allowed
      is_fallback and not allow_fallback? ->
        if final_score >= thr, do: {:allow, final_score}, else: {:block, final_score}

      prefer_source? and final_score >= thr ->
        {:boost, final_score}

      prefer_source? and final_score >= 0.20 ->
        {:boost, final_score}

      final_score >= thr ->
        {:allow, final_score}

      true ->
        {:block, final_score}
    end
  end

  # --- Internals --------------------------------------------------------------

  defp fallback_id?(nil), do: false
  defp fallback_id?(id) when is_binary(id), do: String.ends_with?(id, "|phrase|fallback")
  defp fallback_id?(_), do: false

  # Placeholder for future anti-redundancy; keep zero for now
  defp diversity_penalty(_cand, _cfg), do: 0.0

  @spec within_lemma_budget?([map()], map(), cfg()) :: {boolean(), boolean()}
  defp within_lemma_budget?(wm, cand, cfg) do
    lemma = to_string(cand[:lemma] || guess_lemma_from_id(cand[:id]) || "")
    budget = Map.get(cfg, :lemma_budget, 2)
    margin = Map.get(cfg, :replace_margin, 0.10)

    if lemma == "" or budget <= 0 do
      {true, true}
    else
      same = Enum.filter(wm, &(to_string(&1[:lemma] || "") == lemma))

      if length(same) < budget do
        {true, true}
      else
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

  # --- New scoring helpers ----------------------------------------------------

  # Light, seconds-scale recency nudge; safe if ts_ms missing.
  defp recency_nudge(cand, cfg) do
    now = System.system_time(:millisecond)
    half = Map.get(cfg, :half_life_ms, 7_500)
    wt = Map.get(cfg, :recency_weight, 0.10)

    ts = cand[:ts_ms]
    dt = if is_integer(ts) and ts > 0, do: max(0, now - ts), else: 0
    decay = :math.pow(0.5, dt / max(1, half))

    wt * decay
  end

  # Small nudge when candidate intent matches current intent.
  defp intent_nudge(cand, cfg) do
    cur = Map.get(cfg, :current_intent)
    wt = Map.get(cfg, :intent_weight, 0.05)
    cin = cand[:intent] || get_in(cand, [:features, :intent])

    if cur && cin && cur == cin, do: wt, else: 0.0
  end

  # Novelty boost based on whether the lemma is absent from recent WM items.
  defp novelty_boost(wm, cand, cfg) do
    window = Map.get(cfg, :novelty_window, 16)
    wt = Map.get(cfg, :novelty_weight, 0.15)

    lemma = to_string(cand[:lemma] || guess_lemma_from_id(cand[:id]) || "")

    if lemma == "" do
      0.0
    else
      recent =
        wm
        |> Enum.take(-window)
        |> Enum.map(&to_string(&1[:lemma] || ""))
        |> MapSet.new()

      if MapSet.member?(recent, lemma), do: 0.0, else: wt
    end
  end

  # Outcome uplift: accept either a single score (0..1) or a list(%{score: 0..1})
  defp outcome_uplift(cand, cfg) do
    wt = Map.get(cfg, :outcome_weight, 0.10)

    cond do
      is_number(cand[:episode_score]) ->
        wt * Numbers.clamp01(cand[:episode_score])

      is_list(cand[:episodes]) ->
        best =
          cand[:episodes]
          |> Enum.map(&Map.get(&1, :score, 0.0))
          |> Enum.max(fn -> 0.0 end)

        wt * Numbers.clamp01(best)

      true ->
        0.0
    end
  end

  defp safe_sem_bias(cand) do
    try do
      Brain.Semantics.bias_for(cand)
    rescue
      _ -> 0.0
    catch
      _, _ -> 0.0
    end
  end
end
