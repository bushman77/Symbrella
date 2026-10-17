defmodule Brain.WM.Admission do
  @moduledoc """
  Canonical Working Memory admission boundary.

  This module prepares candidate evidence, delegates the final admission decision
  to `Brain.BasalGanglia`, and applies the resulting mechanical WM update through
  `Brain.WorkingMemory`.
  """

  alias Brain.BasalGanglia
  alias Brain.Config, as: BrainConfig
  alias Brain.WM.Policy, as: WMPolicy
  alias Brain.WorkingMemory

  @gate_event [:brain, :gate, :decision]

  @type wm_item :: map()

  @spec run(map(), list() | map(), map() | keyword()) ::
          {[wm_item()], non_neg_integer(), non_neg_integer()}
  def run(state, cands_or_si, opts) when is_map(state) do
    opts_map = opts_map(opts)
    now = now_ms_from(opts_map)
    self_state = self_state_from(state, opts_map)
    cfg = wm_cfg_from(state, opts_map)
    attn = attention_from(state, cfg, self_state, opts_map)

    base_wm =
      state
      |> Map.get(:wm, [])
      |> WorkingMemory.decay(now, cfg.decay_ms)

    cands_or_si
    |> normalize_candidates(opts_map)
    |> Enum.reduce({base_wm, 0, 0}, fn cand, acc ->
      admit_candidate(cand, acc, now, cfg, attn, self_state)
    end)
    |> trim_and_count(cfg.capacity)
  end

  def run(_state, _cands_or_si, _opts), do: {[], 0, 0}

  @doc false
  @spec normalize_candidates(list() | map(), map() | keyword()) :: [map()]
  def normalize_candidates(cands_or_si, opts \\ %{}) do
    opts_map = opts_map(opts)

    cands_or_si
    |> extract_candidates()
    |> Enum.map(&normalize_candidate(&1, opts_map))
    |> Enum.reject(&is_nil/1)
  end

  defp admit_candidate(cand, {wm_acc, added, removed}, now, cfg, attn, self_state) do
    {decision, score} = BasalGanglia.decide(wm_acc, cand, attn, cfg)
    self_state_bias = WMPolicy.self_state_bias(self_state)

    emit_gate_decision(cand, decision, score, self_state_bias, self_state)

    case decision do
      :block ->
        {wm_acc, added, removed}

      :allow ->
        item = WorkingMemory.normalize(cand, now, activation: score)
        {WorkingMemory.upsert(wm_acc, item, cfg), added + 1, removed}

      :boost ->
        item = WorkingMemory.normalize(cand, now, activation: min(score + 0.2, 1.0))
        {WorkingMemory.upsert(wm_acc, item, cfg), added + 1, removed}
    end
  end

  defp trim_and_count({wm_tmp, added, removed}, capacity) do
    wm_trim = WorkingMemory.trim(wm_tmp, capacity)
    {wm_trim, added, removed + max(length(wm_tmp) - length(wm_trim), 0)}
  end

  defp emit_gate_decision(cand, decision, score, self_state_bias, self_state) do
    :telemetry.execute(
      @gate_event,
      %{
        score: score,
        self_state_bias: self_state_bias
      },
      %{
        decision: decision,
        source: Map.get(cand, :source),
        id: Map.get(cand, :id),
        token_index: Map.get(cand, :token_index),
        self_state_applied?: not is_nil(self_state),
        gate: :basal_ganglia
      }
    )
  end

  defp wm_cfg_from(state, opts_map) do
    base =
      case Map.get(state, :wm_cfg) do
        %{} = cfg -> cfg
        _ -> BrainConfig.wm()
      end

    base
    |> Map.merge(Map.take(opts_map, BrainConfig.wm_keys()))
    |> BrainConfig.wm()
  end

  defp attention_from(state, cfg, self_state, opts_map) do
    state_attn =
      case Map.get(state, :attention) do
        %{} = attn -> attn
        _ -> %{}
      end

    opt_attn =
      case Map.get(opts_map, :attention) do
        %{} = attn -> attn
        _ -> %{}
      end

    state_attn
    |> Map.merge(opt_attn)
    |> Map.put_new(:min_score, 0.0)
    |> Map.put_new(:capacity, cfg.capacity)
    |> maybe_put(:self_state, self_state)
  end

  defp self_state_from(state, opts_map) do
    case Map.get(opts_map, :self_state, Map.get(state, :self_state)) do
      %{} = self_state -> self_state
      _ -> nil
    end
  end

  defp extract_candidates(cands_or_si) when is_list(cands_or_si), do: cands_or_si

  defp extract_candidates(%{} = cands_or_si) do
    Map.get(cands_or_si, :winners) || Map.get(cands_or_si, "winners") ||
      Map.get(cands_or_si, :choices) || Map.get(cands_or_si, "choices") ||
      Map.get(cands_or_si, :lifg_choices) || Map.get(cands_or_si, "lifg_choices") ||
      case Map.get(cands_or_si, :sense_candidates) ||
             Map.get(cands_or_si, "sense_candidates") do
        %{winners: ws} when is_list(ws) -> ws
        %{"winners" => ws} when is_list(ws) -> ws
        ws when is_list(ws) -> ws
        _ -> []
      end
  end

  defp extract_candidates(_), do: []

  defp normalize_candidate({:commit, %{} = choice}, opts) do
    choice
    |> Map.put_new(:source, :lifg)
    |> Map.put_new(:stage2_action, :commit)
    |> normalize_candidate(opts)
  end

  defp normalize_candidate({decision, %{} = choice}, opts)
       when decision in [:allow, :boost] do
    choice
    |> Map.put_new(:source, :lifg)
    |> Map.put(:stage2_decision, decision)
    |> normalize_candidate(opts)
  end

  defp normalize_candidate(%{} = candidate, opts) do
    id0 =
      get(candidate, :id) ||
        get(candidate, :chosen_id) ||
        get(candidate, :winner_id)

    lemma0 =
      get(candidate, :lemma) ||
        (id0 && guess_lemma_from_id(id0)) ||
        get(candidate, :phrase) ||
        get(candidate, :word) ||
        ""

    lemma = to_string(lemma0)

    id =
      cond do
        not is_nil(id0) ->
          to_string(id0)

        lemma != "" ->
          "#{lemma}|phrase|fallback"

        true ->
          "unk|phrase|fallback"
      end

    source = get(candidate, :source) || Map.get(opts, :source) || :runtime

    score =
      [
        get(candidate, :score),
        get(candidate, :prob),
        get(candidate, :p_top1),
        get(candidate, :activation_snapshot)
      ]
      |> first_number(nil)

    # Never promote absent or merely relative evidence (such as :margin)
    # into a maximum-confidence WM candidate.
    if is_nil(score) do
      nil
    else
      candidate
      |> Map.put(:token_index, nonneg_int(get(candidate, :token_index) || get(candidate, :index)))
      |> Map.put(:id, id)
      |> Map.put(:lemma, lemma)
      |> Map.put(:score, score)
      |> Map.put(:source, source)
    end
  end

  # Bare strings and unknown terms do not carry scored cognitive evidence.
  # LIFG and recall callers must provide explicitly scored candidate maps.
  defp normalize_candidate(_unscored, _opts), do: nil

  # Deterministic timestamps are needed only by the legacy pure-list adapter.
  # Production calls without :now_ms retain the system timestamp.
  defp now_ms_from(%{now_ms: ms}) when is_integer(ms) and ms >= 0, do: ms
  defp now_ms_from(_opts), do: System.system_time(:millisecond)

  defp opts_map(opts) when is_map(opts), do: opts

  defp opts_map(opts) when is_list(opts) do
    if Keyword.keyword?(opts), do: Map.new(opts), else: %{}
  end

  defp opts_map(_), do: %{}

  defp maybe_put(map, _key, nil), do: map
  defp maybe_put(map, key, value), do: Map.put(map, key, value)

  defp get(map, key) when is_atom(key) do
    Map.get(map, key) || Map.get(map, Atom.to_string(key))
  end

  defp first_number(values, default) do
    found =
      Enum.find_value(values, fn
        value when is_integer(value) ->
          value * 1.0

        value when is_float(value) ->
          value

        value when is_binary(value) ->
          case Float.parse(String.trim(value)) do
            {parsed, ""} -> parsed
            _ -> nil
          end

        _ ->
          nil
      end)

    if is_nil(found), do: default, else: clamp01(found)
  end

  defp nonneg_int(nil), do: 0
  defp nonneg_int(value) when is_integer(value) and value >= 0, do: value
  defp nonneg_int(value) when is_float(value) and value >= 0, do: trunc(value)

  defp nonneg_int(value) when is_binary(value) do
    case Integer.parse(String.trim(value)) do
      {parsed, _} when parsed >= 0 -> parsed
      _ -> 0
    end
  end

  defp nonneg_int(_), do: 0

  defp guess_lemma_from_id(nil), do: nil

  defp guess_lemma_from_id(id) when is_binary(id) do
    case String.split(id, "|", parts: 2) do
      [word | _] -> word
      _ -> nil
    end
  end

  defp guess_lemma_from_id(id), do: id |> to_string() |> guess_lemma_from_id()

  defp clamp01(value) when is_number(value), do: (value * 1.0) |> max(0.0) |> min(1.0)
  defp clamp01(_), do: 0.0
end
