defmodule Brain.BasalGanglia do
  @moduledoc """
  Stateless gating policy for Working Memory (WM).

  ## API
    decide(wm, cand, attn, cfg) :: {:allow | :boost | :block, score :: float}

  * `wm`  – current WM items (newest-first), as produced by Brain.WorkingMemory.normalize/3
  * `cand` – candidate map (can be an LIFG winner, runtime hit, etc.)
  * `attn` – attention context (used by Brain.Attention.salience/2)
  * `cfg` – map of options (usually from Brain’s wm_cfg merged with per-call opts)

  ## Config keys (all optional)
    :capacity               (pos int, default 7)
    :gate_threshold         (0..1, default 0.4)
    :source_boosts          (map, e.g. %{runtime: 0.2, recency: 0.1})
    :prefer_sources         (list, default [:runtime, :recency, :lifg])
    :dup_penalty            (0..1, default 0.0)   # lower score when duplicate exists
    :cooldown_ms            (int, default 0)      # if same id bumped recently -> treat as :boost
    :fullness_penalty_mult  (0..1, default 0.2)   # raises threshold as WM fills up
  """

  @type decision :: :allow | :boost | :block

  @spec decide([map()], map(), map(), map()) :: {decision(), float()}
  def decide(wm, cand, attn, cfg) when is_list(wm) and is_map(cand) and is_map(attn) and is_map(cfg) do
    now        = System.system_time(:millisecond)
    capacity   = get_pos_int(cfg[:capacity], 7)
    thr_base   = clamp01(cfg[:gate_threshold] || 0.4)
    src_boosts = cfg[:source_boosts] || %{}
    prefer_src = Map.get(cfg, :prefer_sources, [:runtime, :recency, :lifg])
    dup_pen    = clamp01(cfg[:dup_penalty] || 0.0)
    cooldown   = get_pos_int(cfg[:cooldown_ms], 0)
    fmult      = clamp01(cfg[:fullness_penalty_mult] || 0.2)

    # fullness increases the effective threshold as WM fills
    fullness   = if capacity > 0, do: min(length(wm) / capacity, 1.0), else: 1.0
    thr_eff    = clamp01(thr_base + fmult * fullness)

    # base evidence from candidate + attention salience + source prior
    base       = to_float(cand[:score] || cand[:activation_snapshot] || 0.0)
    salience   = Brain.Attention.salience(cand, attn)
    src_bonus  = to_float(src_boosts[cand[:source]]) # may be nil

    score0     = clamp01(0.7 * base + 0.3 * salience + (src_bonus || 0.0))

    {dup?, recent?} = duplicate_flags(wm, cand, now, cooldown)

    score =
      score0
      |> then(fn s -> if dup?, do: s * max(1.0 - dup_pen, 0.0), else: s end)
      |> clamp01()

    prefer_source? = cand[:source] in prefer_src

    cond do
      # If we just saw this id and cooldown applies, treat as a targeted boost.
      recent? ->
        {:boost, score}

      # Prefer certain sources when strong enough.
      prefer_source? and score >= thr_eff ->
        {:boost, score}

      # Strong enough overall: allow it into WM.
      score >= thr_eff ->
        {:allow, score}

      # Otherwise block; caller will emit gate telemetry either way.
      true ->
        {:block, score}
    end
  end

  # ---------- internals ----------

  defp duplicate_flags(wm, cand, now_ms, cooldown_ms) do
    id   = cand[:id] || cand[:chosen_id] || nil
    key1 = down(cand[:lemma] || cand[:word] || cand[:phrase])
    key2 = down(parse_id_word(cand[:id]))

    found =
      Enum.find(wm, fn it ->
        it_id  = it[:id]
        ip     = it[:payload] || %{}
        ip_key = down(ip[:lemma] || ip[:word] || ip[:phrase] || parse_id_word(ip[:id]))

        it_id == id or (key1 && down(it_id) == key1) or (key1 && ip_key == key1) or
          (key2 && ip_key == key2)
      end)

    dup? = !!found

    recent? =
      case {found, cooldown_ms} do
        {%{last_bump: lb} = _it, c} when is_integer(c) and c > 0 ->
          age = now_ms - (lb || 0)
          age >= 0 and age < c
        _ ->
          false
      end

    {dup?, recent?}
  end

  defp parse_id_word(nil), do: nil
  defp parse_id_word(id) when is_binary(id) do
    case String.split(id, "|", parts: 2) do
      [w | _] -> w
      _ -> nil
    end
  end

  defp down(nil), do: nil
  defp down(b) when is_binary(b), do: String.downcase(b)
  defp down(other), do: other

  defp clamp01(x) when is_number(x), do: x |> max(0.0) |> min(1.0)
  defp clamp01(_), do: 0.0

  defp to_float(x) when is_number(x), do: x * 1.0
  defp to_float(_), do: 0.0

  defp get_pos_int(v, d) when is_integer(v) and v > 0, do: v
  defp get_pos_int(_, d), do: d
end

