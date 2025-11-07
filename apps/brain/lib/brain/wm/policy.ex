defmodule Brain.WM.Policy do
  @moduledoc """
  Working-Memory admission policy:
  - computes gate scores for candidates
  - decides allow/block/boost
  - small helpers (lemma budget, fallback handling)

  Pure & stateless. Call from `Brain.do_focus/3`.
  """

  alias Brain.Utils.Numbers

  @type cfg :: %{
          gate_threshold: number(),
          fallback_scale: number(),
          lemma_budget: pos_integer(),
          replace_margin: number(),
          allow_unk?: boolean(),
          allow_seed?: boolean(),
          allow_fallback_into_wm?: boolean()
        }

  # --- Public API -------------------------------------------------------------

  @spec acceptable_candidate?(map(), cfg()) :: boolean()
  def acceptable_candidate?(cand, cfg) do
    id  = to_string(cand[:id] || "")
    pos = to_string(get_in(cand, [:pos]) || get_in(cand, [:features, :pos]) || "")

    allow_unk?  = Map.get(cfg, :allow_unk?, true)
    allow_seed? = Map.get(cfg, :allow_seed?, true)

    cond do
      not allow_seed? and String.ends_with?(id, "|seed|") -> false
      not allow_unk?  and String.contains?(String.downcase(pos), "unk") -> false
      true -> true
    end
  end

  @spec gate_score_for(map(), number(), cfg()) :: float()
  def gate_score_for(cand, salience, cfg) do
    base =
      (cand[:score] || cand[:activation_snapshot] || 0.0) * 1.0

    prefer = if cand[:source] in [:runtime, :recency, :lifg, :ltm], do: 0.10, else: 0.0

    b_scaled =
      if fallback_id?(cand[:id]) do
        scale = cfg[:fallback_scale] || 0.70
        base * scale
      else
        base
      end

    # Optional semantic bias hook (safe even if Semantics isn’t running)
    sem_boost =
      case safe_sem_bias(cand) do
        b when is_number(b) -> (cfg[:semantic_boost] || 0.1) * Numbers.clamp01(b)
        _ -> 0.0
      end

    b_scaled
    |> Kernel.+(0.5 * Numbers.clamp01(salience))
    |> Kernel.+(prefer)
    |> Kernel.+(sem_boost)
    |> Kernel.-(diversity_penalty(cand, cfg))
    |> Numbers.clamp01()
  end

  @spec decide_gate_policy([map()], map(), float(), cfg()) ::
          {:allow | :block | :boost, float()}
  def decide_gate_policy(wm, cand, gate_score, cfg) do
    prefer_source?   = cand[:source] in [:runtime, :recency, :lifg, :ltm]
    thr              = cfg.gate_threshold
    allow_fallback?  = Map.get(cfg, :allow_fallback_into_wm?, false)
    is_fallback      = fallback_id?(cand[:id])

    {within_budget?, beats_by?} = within_lemma_budget?(wm, cand, cfg)

    cond do
      not within_budget? and not beats_by? ->
        {:block, gate_score}

      # P-206: fallbacks must meet normal threshold unless explicitly allowed
      is_fallback and not allow_fallback? ->
        if gate_score >= thr, do: {:allow, gate_score}, else: {:block, gate_score}

      prefer_source? and gate_score >= thr ->
        {:boost, gate_score}

      prefer_source? and gate_score >= 0.20 ->
        {:boost, gate_score}

      gate_score >= thr ->
        {:allow, gate_score}

      true ->
        {:block, gate_score}
    end
  end

  # --- Internals --------------------------------------------------------------

  defp fallback_id?(nil), do: false
  defp fallback_id?(id) when is_binary(id), do: String.ends_with?(id, "|phrase|fallback")
  defp fallback_id?(_), do: false

  # currently a no-op; placeholder for future anti-redundancy
  defp diversity_penalty(_cand, _cfg), do: 0.0

  @spec within_lemma_budget?([map()], map(), cfg()) :: {boolean(), boolean()}
  defp within_lemma_budget?(wm, cand, cfg) do
    lemma  = to_string(cand[:lemma] || guess_lemma_from_id(cand[:id]) || "")
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

