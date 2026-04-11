defmodule Brain.WM.Policy do
  @moduledoc """
  Working-memory policy.

  Responsibilities:

  * admission policy
  * gate scoring
  * retention policy
  * decay and eviction helpers
  """

  alias Brain.Utils.Numbers

  @type cfg :: %{
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
          optional(:semantic_boost) => number(),
          optional(:capacity) => non_neg_integer()
        }

  @type wm_entry :: map()
  @type wm_state :: %{
          required(:wm) => [wm_entry()],
          optional(:wm_cfg) => cfg(),
          optional(:wm_last_ms) => integer() | nil
        }

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
    semantic_boost: 0.10,
    capacity: 7
  }

  # Public API: Admission

  @spec acceptable_candidate?(map(), cfg()) :: boolean()
  def acceptable_candidate?(cand, cfg) do
    cfg = normalize_cfg(cfg)

    id = to_string(cand[:id] || "")
    pos = to_string(get_in(cand, [:pos]) || get_in(cand, [:features, :pos]) || "")

    cond do
      not cfg.allow_seed? and String.ends_with?(id, "|seed|") -> false
      not cfg.allow_unk? and String.contains?(String.downcase(pos), "unk") -> false
      true -> true
    end
  end

  @spec gate_score_for(map(), number(), cfg()) :: float()
  def gate_score_for(cand, salience, cfg) do
    cfg = normalize_cfg(cfg)

    base = (cand[:score] || cand[:activation_snapshot] || 0.0) * 1.0
    prefer = if cand[:source] in [:runtime, :recency, :lifg, :ltm], do: 0.10, else: 0.0
    scaled_base = maybe_scale_fallback(base, cand, cfg)
    sem_boost = semantic_boost(cand, cfg)

    scaled_base
    |> Kernel.+(0.5 * Numbers.clamp01(salience))
    |> Kernel.+(prefer)
    |> Kernel.+(sem_boost)
    |> Kernel.+(recency_nudge(cand, cfg))
    |> Kernel.+(intent_nudge(cand, cfg))
    |> Kernel.-(diversity_penalty(cand, cfg))
    |> Numbers.clamp01()
  end

  @spec decide_gate_policy([map()], map(), float(), cfg()) :: {:allow | :block | :boost, float()}
  def decide_gate_policy(wm, cand, gate_score, cfg) do
    cfg = normalize_cfg(cfg)

    prefer_source? = cand[:source] in [:runtime, :recency, :lifg, :ltm]
    thr = cfg.gate_threshold
    allow_fallback? = cfg.allow_fallback_into_wm?
    is_fallback = fallback_id?(cand[:id])

    {within_budget?, beats_by?} = within_lemma_budget?(wm, cand, cfg)

    final_score =
      gate_score
      |> Kernel.+(novelty_boost(wm, cand, cfg))
      |> Kernel.+(outcome_uplift(cand, cfg))
      |> Numbers.clamp01()

    cond do
      not within_budget? and not beats_by? ->
        {:block, final_score}

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

  # Public API: Retention

  @spec apply_decay(wm_state(), integer()) :: wm_state()
  def apply_decay(%{wm: wm} = state, now_ms) when is_list(wm) and is_integer(now_ms) do
    dt = elapsed_ms(state, now_ms)
    decay_factor = Numbers.decay_factor_ms(dt)

    state
    |> Map.put(:wm, decay_wm_entries(wm, decay_factor))
    |> Map.put(:wm_last_ms, now_ms)
  end

  def apply_decay(state, _now_ms), do: state

  @spec evict_if_needed(wm_state()) :: wm_state()
  def evict_if_needed(%{wm: wm, wm_cfg: %{capacity: cap}} = state)
      when is_list(wm) and is_integer(cap) and cap >= 0 do
    if length(wm) <= cap do
      state
    else
      Map.put(state, :wm, keep_best_entries(wm, cap))
    end
  end

  def evict_if_needed(state), do: state

  @spec decay_and_evict(wm_state(), integer()) :: wm_state()
  def decay_and_evict(state, now_ms) do
    state
    |> apply_decay(now_ms)
    |> evict_if_needed()
  end

  # Config

  defp normalize_cfg(nil), do: @default_cfg
  defp normalize_cfg(cfg) when is_map(cfg), do: Map.merge(@default_cfg, cfg)

  # Admission helpers

  defp maybe_scale_fallback(base, cand, cfg) do
    if fallback_id?(cand[:id]), do: base * cfg.fallback_scale, else: base
  end

  defp semantic_boost(cand, cfg) do
    case safe_sem_bias(cand) do
      bias when is_number(bias) -> cfg.semantic_boost * Numbers.clamp01(bias)
      _ -> 0.0
    end
  end

  defp fallback_id?(nil), do: false
  defp fallback_id?(id) when is_binary(id), do: String.ends_with?(id, "|phrase|fallback")
  defp fallback_id?(_), do: false

  defp diversity_penalty(_cand, _cfg), do: 0.0

  defp within_lemma_budget?(wm, cand, cfg) do
    lemma = to_string(cand[:lemma] || guess_lemma_from_id(cand[:id]) || "")
    budget = cfg.lemma_budget
    margin = cfg.replace_margin

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
      [word | _] -> word
      _ -> nil
    end
  end

  defp novelty_boost(wm, cand, cfg) do
    weight = cfg.novelty_weight || 0.0

    if weight <= 0.0 do
      0.0
    else
      recent_items = Enum.take(wm, cfg.novelty_window || 0)
      cand_id = normalize_value(cand[:id])
      cand_lemma = normalize_value(cand[:lemma] || guess_lemma_from_id(cand[:id]))

      repeated? =
        Enum.any?(recent_items, fn item ->
          same_id? = cand_id != "" and normalize_value(item[:id]) == cand_id
          same_lemma? = cand_lemma != "" and normalize_value(item[:lemma]) == cand_lemma
          same_id? or same_lemma?
        end)

      if repeated?, do: 0.0, else: Numbers.clamp01(weight)
    end
  end

  defp outcome_uplift(cand, cfg) do
    weight = cfg.outcome_weight || 0.0

    if weight <= 0.0 do
      0.0
    else
      raw = cand[:episode_score] || episode_score_from_list(cand[:episodes]) || 0.0
      Numbers.clamp01(weight * numeric(raw))
    end
  end

  defp recency_nudge(cand, cfg) do
    weight = cfg.recency_weight || 0.0
    ts_ms = timestamp_ms(cand)

    cond do
      weight <= 0.0 ->
        0.0

      is_nil(ts_ms) ->
        0.0

      true ->
        now_ms = System.system_time(:millisecond)
        dt = max(now_ms - ts_ms, 0)
        half_life_ms = max(cfg.half_life_ms || 7_500, 1)
        recency = :math.exp(-dt / half_life_ms)
        Numbers.clamp01(weight * recency)
    end
  end

  defp intent_nudge(cand, cfg) do
    weight = cfg.intent_weight || 0.0
    current_intent = cfg.current_intent

    cond do
      weight <= 0.0 or is_nil(current_intent) ->
        0.0

      intent_matches?(cand, current_intent) ->
        Numbers.clamp01(weight)

      true ->
        0.0
    end
  end

  defp safe_sem_bias(cand) do
    bias =
      cand[:semantic_bias] || cand[:sem_bias] || get_in(cand, [:features, :semantic_bias]) ||
        get_in(cand, [:features, :sem_bias])

    case bias do
      value when is_number(value) -> value * 1.0
      _ -> 0.0
    end
  end

  defp intent_matches?(cand, current_intent) do
    cand_intent = cand[:intent] || cand[:intent_tag] || get_in(cand, [:features, :intent])

    cond do
      cand_intent == current_intent ->
        true

      is_binary(cand_intent) ->
        cand_intent == Atom.to_string(current_intent)

      true ->
        false
    end
  end

  defp episode_score_from_list(list) when is_list(list) do
    list
    |> Enum.map(fn
      %{score: score} -> numeric(score)
      %{"score" => score} -> numeric(score)
      _ -> 0.0
    end)
    |> Enum.max(fn -> 0.0 end)
  end

  defp episode_score_from_list(_), do: 0.0

  defp timestamp_ms(cand) do
    ts = cand[:ts_ms] || cand[:timestamp_ms] || cand[:ts]

    case ts do
      value when is_integer(value) -> value
      value when is_float(value) -> round(value)
      _ -> nil
    end
  end

  defp numeric(value) when is_integer(value), do: value * 1.0
  defp numeric(value) when is_float(value), do: value

  defp numeric(value) when is_binary(value) do
    case Float.parse(String.trim(value)) do
      {parsed, _} -> parsed
      _ -> 0.0
    end
  end

  defp numeric(_), do: 0.0

  defp normalize_value(nil), do: ""

  defp normalize_value(value) do
    value
    |> to_string()
    |> String.trim()
    |> String.downcase()
  end

  # Retention helpers

  defp elapsed_ms(state, now_ms) do
    case Map.get(state, :wm_last_ms) do
      last_ms when is_integer(last_ms) -> max(now_ms - last_ms, 0)
      _ -> 0
    end
  end

  defp decay_wm_entries(wm, decay_factor) do
    Enum.map(wm, &decay_entry(&1, decay_factor))
  end

  defp decay_entry(%{score: score} = entry, decay_factor) when is_number(score) do
    %{entry | score: Numbers.clamp01(score * decay_factor)}
  end

  defp decay_entry(entry, _decay_factor), do: entry

  defp keep_best_entries(wm, cap) do
    wm
    |> Enum.sort_by(&entry_sort_key/1, :desc)
    |> Enum.take(cap)
  end

  defp entry_sort_key(entry) do
    {Map.get(entry, :score, 0.0), Map.get(entry, :ts, 0)}
  end
end
