defmodule Brain.WM.Focus do
  @moduledoc false

  alias Brain.Attention
  alias Brain.WM.Policy, as: WMPolicy
  alias Brain.WorkingMemory

  @type wm_item :: map()

  @spec run(map(), list() | map(), map() | keyword()) :: {[wm_item()], non_neg_integer(), non_neg_integer()}
  def run(state, cands_or_si, _opts) when is_map(state) do
    now = System.system_time(:millisecond)

    wm_cfg0 = Map.get(state, :wm_cfg, %{})

    cfg = %{
      capacity: Map.get(wm_cfg0, :capacity, 3),
      decay_ms: Map.get(wm_cfg0, :decay_ms, 8_000)
    }

    attention0 = Map.get(state, :attention, %{})

    attn = %{
      min_score: Map.get(attention0, :min_score, 0.0),
      capacity: Map.get(attention0, :capacity, cfg.capacity)
    }

    base_wm =
      state
      |> Map.get(:wm, [])
      |> WorkingMemory.decay(now, cfg.decay_ms)

    cands_or_si
    |> normalize_candidates()
    |> Enum.reduce({base_wm, 0, 0}, fn cand, acc ->
      focus_reduce_step(cand, acc, now, cfg, attn)
    end)
    |> trim_and_count(cfg.capacity)
  end

  def run(_state, _cands_or_si, _opts), do: {[], 0, 0}

  defp focus_reduce_step(cand, {wm_acc, a_cnt, r_cnt}, now, cfg, attn) do
    if not WMPolicy.acceptable_candidate?(cand, cfg) do
      {wm_acc, a_cnt, r_cnt}
    else
      salience = Attention.salience(cand, attn)
      gate_score = WMPolicy.gate_score_for(cand, salience, cfg)
      {decision, s} = WMPolicy.decide_gate_policy(wm_acc, cand, gate_score, cfg)

      :telemetry.execute(
        [:brain, :gate, :decision],
        %{score: gate_score},
        %{decision: decision, source: Map.get(cand, :source)}
      )

      case decision do
        :block ->
          {wm_acc, a_cnt, r_cnt}

        :allow ->
          item = WorkingMemory.normalize(cand, now, activation: s)
          {WorkingMemory.upsert(wm_acc, item, cfg), a_cnt + 1, r_cnt}

        :boost ->
          item = WorkingMemory.normalize(cand, now, activation: min(s + 0.2, 1.0))
          {WorkingMemory.upsert(wm_acc, item, cfg), a_cnt + 1, r_cnt}
      end
    end
  end

  defp trim_and_count({wm_tmp, a_cnt, r_cnt}, capacity) do
    wm_trim = WorkingMemory.trim(wm_tmp, capacity)
    {wm_trim, a_cnt, r_cnt + (length(wm_tmp) - length(wm_trim))}
  end

  # ───────────────────────── Candidates normalization (WM) ─────────────────────

  defp normalize_candidates(cands_or_si) do
    base =
      cond do
        is_list(cands_or_si) ->
          cands_or_si

        is_map(cands_or_si) ->
          Map.get(cands_or_si, :winners) || Map.get(cands_or_si, "winners") ||
            Map.get(cands_or_si, :choices) || Map.get(cands_or_si, "choices") ||
            case Map.get(cands_or_si, :sense_candidates) ||
                   Map.get(cands_or_si, "sense_candidates") do
              %{winners: ws} when is_list(ws) -> ws
              %{"winners" => ws} when is_list(ws) -> ws
              ws when is_list(ws) -> ws
              _ -> []
            end

        true ->
          []
      end

    Enum.map(base, &normalize_candidate/1)
  end

  defp normalize_candidate(%{} = c) do
    id = c[:id] || c["id"]

    lemma =
      c[:lemma] || c["lemma"] ||
        (id && guess_lemma_from_id(id)) ||
        c[:phrase] || c["phrase"] ||
        c[:word] || c["word"] || ""

    %{
      token_index: c[:token_index] || c["token_index"] || 0,
      id:
        (id && to_string(id)) || (lemma != "" && "#{lemma}|phrase|fallback") ||
          "unk|phrase|fallback",
      lemma: to_string(lemma),
      score: (c[:score] || c["score"] || c[:margin] || c["margin"] || 1.0) * 1.0,
      source: c[:source] || c["source"] || :runtime
    }
  end

  defp normalize_candidate(w) when is_binary(w) do
    if String.contains?(w, "|") do
      %{
        token_index: 0,
        id: w,
        lemma: guess_lemma_from_id(w) || "",
        score: 1.0,
        source: :runtime
      }
    else
      l = String.downcase(w)

      %{
        token_index: 0,
        id: "#{l}|phrase|fallback",
        lemma: l,
        score: 1.0,
        source: :runtime
      }
    end
  end

  defp normalize_candidate(_other) do
    %{
      token_index: 0,
      id: "unk|phrase|fallback",
      lemma: "",
      score: 0.0,
      source: :runtime
    }
  end

  defp guess_lemma_from_id(nil), do: nil

  defp guess_lemma_from_id(id) when is_binary(id) do
    case String.split(id, "|", parts: 2) do
      [w | _] -> w
      _ -> nil
    end
  end

  defp guess_lemma_from_id(_), do: nil
end

