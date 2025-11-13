# apps/brain/lib/brain/basal_ganglia.ex
defmodule Brain.BasalGanglia do
  @moduledoc """
  Stateless gating policy for Working Memory (WM).

  decide/4 returns `{decision, score}` where decision ∈ `:allow | :boost | :block`.

  Inputs:
    * `wm`   – current WM items (newest-first); items may include `%{payload: %{id: ..., last_bump: ...}}`
    * `cand` – candidate map (e.g., LIFG winner) with at least `:score` or `:activation_snapshot`
    * `attn` – attention context (used by Brain.Attention.salience/2 if available)
    * `cfg`  – merged config (Brain's wm_cfg + per-call opts)

  Config keys (all optional; see normalize_cfg/1 for defaults):
    :capacity, :gate_threshold, :lifg_min_score, :source_boosts,
    :prefer_sources, :disprefer_sources, :dup_penalty, :cooldown_ms,
    :fullness_penalty_mult, :boost_threshold, :boost_threshold_pref,
    :block_threshold, :block_threshold_disprefer
  """

  @type decision :: :allow | :boost | :block
  @spec decide([map()], map(), map(), map()) :: {decision(), float()}
  def decide(wm, cand, attn, cfg)
      when is_list(wm) and is_map(cand) and is_map(attn) and is_map(cfg) do
    now = System.system_time(:millisecond)
    cfg = normalize_cfg(cfg)

    capacity = cfg.capacity
    thr_base = cfg.gate_threshold
    min_floor = cfg.lifg_min_score
    dup_pen = cfg.dup_penalty
    cooldown = cfg.cooldown_ms
    fmult = cfg.fullness_penalty_mult

    boost_thr = cfg.boost_threshold
    boost_thr_pref = cfg.boost_threshold_pref
    block_thr = cfg.block_threshold
    block_thr_disp = cfg.block_threshold_disprefer

    prefer_src = cfg.prefer_sources |> Enum.map(&source_key/1)
    disp_src = cfg.disprefer_sources |> Enum.map(&source_key/1)

    # Fullness raises the effective gate; never drop below hard floor
    fullness = min(length(wm) / max(capacity, 1), 1.0)
    thr_eff0 = clamp01(thr_base + fmult * fullness)
    thr_eff = max(thr_eff0, min_floor)

    # Base score (never undercut caller-provided score)
    base =
      cand
      |> Map.get(:score, Map.get(cand, :activation_snapshot, 0.0))
      |> to_float()

    salience =
      try do
        if Code.ensure_loaded?(Brain.Attention) and
             function_exported?(Brain.Attention, :salience, 2) do
          Brain.Attention.salience(cand, attn)
        else
          0.0
        end
      rescue
        _ -> 0.0
      end

    src_bonus =
      cfg.source_boosts
      |> Map.get(Map.get(cand, :source))
      |> to_float()

    blended = clamp01(0.7 * base + 0.3 * salience + src_bonus)
    score0 = max(base, blended)

    {dup?, match} = duplicate_match(wm, cand)

    score =
      if(dup?, do: score0 * max(1.0 - dup_pen, 0.0), else: score0)
      |> clamp01()

    recent? = cooldown_match?(match, now, cooldown)

    src_key = source_key(Map.get(cand, :source))
    pref? = src_key in prefer_src
    disp? = src_key in disp_src

    cond do
      # Hard floor
      score < min_floor ->
        {:block, score}

      # Cooldown rebump
      recent? ->
        {:boost, score}

      # Preferred sources: easier boost path, but not below effective gate
      pref? and score >= max(boost_thr_pref, thr_eff) ->
        {:boost, score}

      # Strong enough overall
      score >= max(boost_thr, thr_eff) ->
        {:allow, score}

      # Dispreferred weak
      disp? and score <= block_thr_disp ->
        {:block, score}

      # Generic weak
      score <= block_thr ->
        {:block, score}

      # Conservative default
      true ->
        {:block, score}
    end
  end

  # ── helpers ─────────────────────────────────────────────────────────────────

  # Normalize cfg with safe defaults & clamped ranges
  defp normalize_cfg(cfg) do
    thr_base = clamp01(Map.get(cfg, :gate_threshold, 0.40))

    %{
      capacity: get_pos_int(Map.get(cfg, :capacity, 7), 7),
      gate_threshold: thr_base,
      lifg_min_score: clamp01(Map.get(cfg, :lifg_min_score, Map.get(cfg, :min_score, 0.0))),
      source_boosts: Map.get(cfg, :source_boosts, %{}),
      prefer_sources:
        Map.get(
          cfg,
          :prefer_sources,
          Map.get(cfg, :preferred_sources, [
            :hippocampus,
            :pmtg,
            :lifg,
            :runtime,
            :recency,
            :intent
          ])
        ),
      disprefer_sources:
        Map.get(cfg, :disprefer_sources, Map.get(cfg, :dispreferred_sources, [])),
      dup_penalty: clamp01(Map.get(cfg, :dup_penalty, 0.0)),
      cooldown_ms: get_pos_int(Map.get(cfg, :cooldown_ms, 0), 0),
      fullness_penalty_mult: clamp01(Map.get(cfg, :fullness_penalty_mult, 0.20)),
      boost_threshold: clamp01(Map.get(cfg, :boost_threshold, thr_base)),
      boost_threshold_pref:
        clamp01(Map.get(cfg, :boost_threshold_pref, max(thr_base - 0.05, 0.0))),
      block_threshold: clamp01(Map.get(cfg, :block_threshold, 0.20)),
      block_threshold_disprefer: clamp01(Map.get(cfg, :block_threshold_disprefer, 0.25))
    }
  end

  # Duplicate detection (by id or fuzzy key)
  # Returns {dup?, matching_item_or_nil}
  defp duplicate_match(wm, cand) do
    {id, fuzzy_key} = extract_keys(cand)

    found =
      Enum.find(wm, fn it ->
        item_matches?(it, id, fuzzy_key)
      end)

    {not is_nil(found), found}
  end

  # Cooldown rebump if a recent matching item exists
  defp cooldown_match?(nil, _now_ms, _cooldown_ms), do: false

  defp cooldown_match?(match, now_ms, cooldown_ms)
       when is_map(match) and is_integer(cooldown_ms) and cooldown_ms > 0 do
    last = Map.get(match, :last_bump, Map.get(match, :ts, 0)) |> to_int()
    age = now_ms - last
    age >= 0 and age < cooldown_ms
  end

  defp cooldown_match?(_match, _now_ms, _cooldown_ms), do: false

  defp item_matches?(it, cand_id, fuzzy_key) do
    ip = Map.get(it, :payload, %{})
    it_id = Map.get(ip, :id) || Map.get(it, :id)

    cond do
      # exact id match
      not is_nil(cand_id) and it_id == cand_id ->
        true

      # fuzzy: id head or stored word/lemma/phrase matches
      not is_nil(fuzzy_key) and down("#{it_id || ""}") == fuzzy_key ->
        true

      not is_nil(fuzzy_key) and
          fuzzy_key == down(extract_fuzzy_key(ip) || extract_fuzzy_key(it)) ->
        true

      true ->
        false
    end
  end

  # Extract consistent matching keys from candidate
  defp extract_keys(cand) do
    payload = Map.get(cand, :payload, %{})
    id = Map.get(payload, :id) || Map.get(cand, :id)
    fuzzy_key = down(extract_fuzzy_key(payload) || extract_fuzzy_key(cand))
    {id, fuzzy_key}
  end

  defp extract_fuzzy_key(%{lemma: l}) when is_binary(l), do: l
  defp extract_fuzzy_key(%{word: w}) when is_binary(w), do: w
  defp extract_fuzzy_key(%{phrase: p}) when is_binary(p), do: p
  defp extract_fuzzy_key(%{id: id}) when is_binary(id), do: parse_id_word(id)
  defp extract_fuzzy_key(_), do: nil

  # Normalize source keys via string path (avoid atom leaks)
  defp source_key(nil), do: ""

  defp source_key(a) when is_atom(a),
    do: a |> Atom.to_string() |> String.trim_leading(":") |> String.downcase()

  defp source_key(s) when is_binary(s),
    do: s |> String.trim() |> String.trim_leading(":") |> String.downcase()

  defp source_key(_), do: ""

  defp parse_id_word(id) when is_binary(id) do
    case String.split(id, "|", parts: 2) do
      [w | _] -> w
      _ -> nil
    end
  end

  defp parse_id_word(_), do: nil

  defp down(nil), do: nil
  defp down(b) when is_binary(b), do: String.downcase(b)
  defp down(other), do: other

  defp clamp01(x) when is_number(x), do: min(1.0, max(0.0, x * 1.0))
  defp clamp01(_), do: 0.0

  defp to_float(x) when is_number(x), do: x * 1.0
  defp to_float(_), do: 0.0

  defp to_int(x) when is_integer(x), do: x
  defp to_int(x) when is_float(x), do: trunc(x)
  defp to_int(_), do: 0

  defp get_pos_int(v, _default) when is_integer(v) and v > 0, do: v
  defp get_pos_int(_, default), do: default
end
