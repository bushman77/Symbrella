defmodule Brain.Config do
  @moduledoc """
  Runtime configuration helpers for Brain.

  Notably:

    * assistant identity (name/norm/aliases) used across regions and UI-adjacent
      semantics
    * effective Working Memory / cognitive gate configuration
  """

  @default_name "Symbrella"

  @wm_defaults %{
    capacity: 7,
    decay_ms: 30_000,
    gate_threshold: 0.4,
    merge_duplicates?: true,
    lemma_budget: 2,
    replace_margin: 0.10,
    diversity_lambda: 0.06,
    allow_unk?: true,
    allow_seed?: true,
    fallback_scale: 0.70,
    allow_fallback_into_wm?: false,
    lifg_min_score: 0.0,
    prefer_sources: [:curiosity, :hippocampus, :pmtg, :lifg, :runtime, :recency, :intent],
    disprefer_sources: [],
    source_boosts: %{},
    dup_penalty: 0.0,
    cooldown_ms: 0,
    fullness_penalty_mult: 0.20,
    boost_threshold: nil,
    boost_threshold_pref: nil,
    block_threshold: 0.20,
    block_threshold_disprefer: 0.25,
    half_life_ms: 7_500,
    novelty_window: 16,
    novelty_weight: 0.15,
    intent_weight: 0.05,
    recency_weight: 0.10,
    outcome_weight: 0.10,
    current_intent: nil,
    semantic_boost: 0.10
  }

  @wm_keys Map.keys(@wm_defaults) ++
             [
               :preferred_sources,
               :dispreferred_sources,
               :evidence_floor,
               :min_input_score,
               :min_score
             ]

  @type assistant :: %{
          name: String.t(),
          norm: String.t(),
          aliases: [String.t()]
        }

  @type wm :: map()

  @doc """
  Effective Working Memory / cognitive-gating configuration.

  Precedence is:

    * safe code defaults
    * supported top-level `:brain` application env keys
    * optional nested `config :brain, :wm, ...`
    * explicit overrides
  """
  @spec wm(map() | keyword()) :: wm()
  def wm(overrides \\ %{}) do
    @wm_defaults
    |> Map.merge(top_level_wm_env())
    |> Map.merge(env_map(Application.get_env(:brain, :wm, %{})))
    |> Map.merge(env_map(overrides))
    |> normalize_wm()
  end

  @doc false
  @spec wm_defaults() :: wm()
  def wm_defaults, do: @wm_defaults

  @doc false
  @spec wm_keys() :: [atom()]
  def wm_keys, do: @wm_keys

  @spec assistant() :: assistant()
  def assistant do
    cfg0 =
      Application.get_env(
        :symbrella,
        :assistant,
        Application.get_env(:brain, :assistant, [])
      )

    cfg =
      cond do
        is_map(cfg0) -> Map.to_list(cfg0)
        is_list(cfg0) -> cfg0
        true -> []
      end

    name =
      cfg
      |> Keyword.get(:name, @default_name)
      |> to_string()
      |> String.trim()
      |> case do
        "" -> @default_name
        v -> v
      end

    norm =
      cfg
      |> Keyword.get(:norm)
      |> case do
        v when is_binary(v) ->
          v1 = String.trim(v)
          if v1 == "", do: norm_text(name), else: norm_text(v1)

        _ ->
          norm_text(name)
      end

    aliases =
      cfg
      |> Keyword.get(:aliases, [])
      |> List.wrap()
      |> Enum.map(&to_string/1)
      |> Enum.map(&String.trim/1)
      |> Enum.reject(&(&1 == ""))
      |> Enum.map(&norm_text/1)
      |> Enum.uniq()

    %{
      name: name,
      norm: norm,
      aliases: aliases
    }
  end

  @spec assistant_match?(map() | String.t() | atom() | nil) :: boolean()
  def assistant_match?(nil), do: false

  def assistant_match?(%{} = tok) do
    v =
      Map.get(tok, :norm) ||
        Map.get(tok, "norm") ||
        Map.get(tok, :phrase) ||
        Map.get(tok, "phrase") ||
        Map.get(tok, :text) ||
        Map.get(tok, "text") ||
        Map.get(tok, :word) ||
        Map.get(tok, "word")

    assistant_match?(v)
  end

  def assistant_match?(v) when is_atom(v), do: assistant_match?(Atom.to_string(v))

  def assistant_match?(v) when is_binary(v) do
    a = assistant()
    n = norm_text(v)
    n == a.norm or n in a.aliases
  end

  def assistant_match?(_), do: false

  defp norm_text(v) when is_binary(v) do
    v
    |> String.downcase()
    |> String.replace(~r/\s+/u, " ")
    |> String.trim()
  end

  defp top_level_wm_env do
    :brain
    |> Application.get_all_env()
    |> Enum.filter(fn {key, _value} -> key in @wm_keys end)
    |> Map.new()
  end

  defp normalize_wm(%{} = cfg) do
    gate_threshold = clamp01(Map.get(cfg, :gate_threshold, @wm_defaults.gate_threshold))

    lifg_min_score =
      cfg
      |> Map.get(:lifg_min_score, Map.get(cfg, :min_score, @wm_defaults.lifg_min_score))
      |> clamp01()

    evidence_floor =
      cfg
      |> Map.get(:evidence_floor, Map.get(cfg, :min_input_score))
      |> fallback(0.0)
      |> clamp01()

    %{
      capacity: pos_int(Map.get(cfg, :capacity), @wm_defaults.capacity),
      decay_ms: pos_int(Map.get(cfg, :decay_ms), @wm_defaults.decay_ms),
      gate_threshold: gate_threshold,
      merge_duplicates?: bool(Map.get(cfg, :merge_duplicates?), @wm_defaults.merge_duplicates?),
      lemma_budget: pos_int(Map.get(cfg, :lemma_budget), @wm_defaults.lemma_budget),
      replace_margin: clamp01(Map.get(cfg, :replace_margin, @wm_defaults.replace_margin)),
      diversity_lambda: clamp01(Map.get(cfg, :diversity_lambda, @wm_defaults.diversity_lambda)),
      allow_unk?: bool(Map.get(cfg, :allow_unk?), @wm_defaults.allow_unk?),
      allow_seed?: bool(Map.get(cfg, :allow_seed?), @wm_defaults.allow_seed?),
      fallback_scale: clamp01(Map.get(cfg, :fallback_scale, @wm_defaults.fallback_scale)),
      allow_fallback_into_wm?:
        bool(
          Map.get(cfg, :allow_fallback_into_wm?),
          @wm_defaults.allow_fallback_into_wm?
        ),
      lifg_min_score: lifg_min_score,
      evidence_floor: evidence_floor,
      prefer_sources:
        source_list(
          Map.get(cfg, :prefer_sources, Map.get(cfg, :preferred_sources)),
          @wm_defaults.prefer_sources
        ),
      disprefer_sources:
        source_list(
          Map.get(cfg, :disprefer_sources, Map.get(cfg, :dispreferred_sources)),
          @wm_defaults.disprefer_sources
        ),
      source_boosts: map_or_default(Map.get(cfg, :source_boosts), @wm_defaults.source_boosts),
      dup_penalty: clamp01(Map.get(cfg, :dup_penalty, @wm_defaults.dup_penalty)),
      cooldown_ms: non_neg_int(Map.get(cfg, :cooldown_ms), @wm_defaults.cooldown_ms),
      fullness_penalty_mult:
        clamp01(Map.get(cfg, :fullness_penalty_mult, @wm_defaults.fullness_penalty_mult)),
      boost_threshold: clamp01(fallback(Map.get(cfg, :boost_threshold), gate_threshold)),
      boost_threshold_pref:
        clamp01(fallback(Map.get(cfg, :boost_threshold_pref), max(gate_threshold - 0.05, 0.0))),
      block_threshold: clamp01(Map.get(cfg, :block_threshold, @wm_defaults.block_threshold)),
      block_threshold_disprefer:
        clamp01(Map.get(cfg, :block_threshold_disprefer, @wm_defaults.block_threshold_disprefer)),
      half_life_ms: pos_int(Map.get(cfg, :half_life_ms), @wm_defaults.half_life_ms),
      novelty_window: non_neg_int(Map.get(cfg, :novelty_window), @wm_defaults.novelty_window),
      novelty_weight: clamp01(Map.get(cfg, :novelty_weight, @wm_defaults.novelty_weight)),
      intent_weight: clamp01(Map.get(cfg, :intent_weight, @wm_defaults.intent_weight)),
      recency_weight: clamp01(Map.get(cfg, :recency_weight, @wm_defaults.recency_weight)),
      outcome_weight: clamp01(Map.get(cfg, :outcome_weight, @wm_defaults.outcome_weight)),
      current_intent: Map.get(cfg, :current_intent, @wm_defaults.current_intent),
      semantic_boost: clamp01(Map.get(cfg, :semantic_boost, @wm_defaults.semantic_boost))
    }
  end

  defp env_map(nil), do: %{}
  defp env_map(m) when is_map(m), do: m

  defp env_map(list) when is_list(list) do
    if Keyword.keyword?(list), do: Map.new(list), else: %{}
  end

  defp env_map(_), do: %{}

  defp fallback(nil, fallback_value), do: fallback_value
  defp fallback(value, _fallback_value), do: value

  defp pos_int(value, _default) when is_integer(value) and value > 0, do: value
  defp pos_int(value, _default) when is_float(value) and value > 0, do: trunc(value)
  defp pos_int(_value, default), do: default

  defp non_neg_int(value, _default) when is_integer(value) and value >= 0, do: value
  defp non_neg_int(value, _default) when is_float(value) and value >= 0, do: trunc(value)
  defp non_neg_int(_value, default), do: default

  defp bool(value, _default) when is_boolean(value), do: value
  defp bool(_value, default), do: default

  defp source_list(value, default) do
    case value do
      list when is_list(list) -> list
      nil -> default
      other -> List.wrap(other)
    end
  end

  defp map_or_default(%{} = map, _default), do: map
  defp map_or_default(_value, default), do: default

  defp clamp01(value) when is_integer(value), do: (value * 1.0) |> clamp01()
  defp clamp01(value) when is_float(value), do: value |> max(0.0) |> min(1.0)
  defp clamp01(_), do: 0.0
end
