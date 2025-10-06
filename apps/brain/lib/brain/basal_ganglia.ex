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
@spec decide([map()], map(), map(), map()) :: {decision(), float()}
def decide(wm, cand, attn, cfg) when is_list(wm) and is_map(cand) and is_map(attn) and is_map(cfg) do
  now        = System.system_time(:millisecond)
  capacity   = get_pos_int(cfg[:capacity], 7)
  thr_base   = clamp01(cfg[:gate_threshold] || 0.4)
  src_boosts = cfg[:source_boosts] || %{}

  # Accept both spellings; broaden default to include hippocampus/pmtg/intent
  prefer_src = Map.get(cfg, :prefer_sources,
                  Map.get(cfg, :preferred_sources, [:hippocampus, :pmtg, :lifg, :runtime, :recency, :intent])
                )
  disp_src   = Map.get(cfg, :disprefer_sources,
                  Map.get(cfg, :dispreferred_sources, [])
                )

  dup_pen    = clamp01(cfg[:dup_penalty] || 0.0)
  cooldown   = get_pos_int(cfg[:cooldown_ms], 0)
  fmult      = clamp01(cfg[:fullness_penalty_mult] || 0.2)

  # Threshold knobs (optional in cfg)
  boost_thr      = clamp01(Map.get(cfg, :boost_threshold, thr_base))
  boost_thr_pref = clamp01(Map.get(cfg, :boost_threshold_pref, max(thr_base - 0.05, 0.0)))
  block_thr      = clamp01(Map.get(cfg, :block_threshold, 0.20))
  block_thr_disp = clamp01(Map.get(cfg, :block_threshold_pref, 0.25))

  # fullness raises the effective gate
  fullness = if capacity > 0, do: min(length(wm) / capacity, 1.0), else: 1.0
  thr_eff  = clamp01(thr_base + fmult * fullness)

  # --- scoring: NEVER undercut the caller-provided cand[:score] ---
  base      = to_float(cand[:score] || cand[:activation_snapshot] || 0.0)
  salience  = Brain.Attention.salience(cand, attn)
  src_bonus = to_float(src_boosts[cand[:source]]) # may be nil

  # blended adds salience/source, but we keep at least `base`
  blended = clamp01(0.7 * base + 0.3 * salience + (src_bonus || 0.0))
  score0  = max(base, blended)

  {dup?, recent?} = duplicate_flags(wm, cand, now, cooldown)

  score =
    score0
    |> then(fn s -> if dup?, do: s * max(1.0 - dup_pen, 0.0), else: s end)
    |> clamp01()

  # normalized source keys (string compare avoids atom leaks)
  src_key        = source_key(cand[:source])
  prefer_keys    = Enum.map(prefer_src, &source_key/1)
  disprefer_keys = Enum.map(disp_src,   &source_key/1)

  pref? = src_key in prefer_keys
  disp? = src_key in disprefer_keys

  cond do
    # Cooldown hit = targeted boost
    recent? ->
      {:boost, score}

    # Preferred sources: easier boost path; don’t set bar below gate floor
    pref? and score >= max(boost_thr_pref, thr_eff) ->
      {:boost, score}

    # Strong enough overall → allow
    score >= max(boost_thr, thr_eff) ->
      {:allow, score}

    # Dispreferred weak → block
    disp? and score <= block_thr_disp ->
      {:block, score}

    # Generic weak → block
    score <= block_thr ->
      {:block, score}

    # Conservative default
    true ->
      {:block, score}
  end
end

# keep your helpers; add this if you don’t already have it:
defp source_key(nil),  do: ""
defp source_key(a) when is_atom(a),  do: a |> Atom.to_string() |> String.trim_leading(":") |> String.downcase()
defp source_key(s) when is_binary(s),do: s |> String.trim() |> String.trim_leading(":") |> String.downcase()
defp source_key(_),  do: ""

# --- helpers for source normalization (string-based; avoids atom leaks) ---
defp source_key(nil),  do: ""
defp source_key(a) when is_atom(a),  do: a |> Atom.to_string() |> String.trim_leading(":") |> String.downcase()
defp source_key(s) when is_binary(s),do: s |> String.trim() |> String.trim_leading(":") |> String.downcase()
defp source_key(_),  do: ""

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

  defp get_pos_int(v, _d) when is_integer(v) and v > 0, do: v
  defp get_pos_int(_, d), do: d
end

