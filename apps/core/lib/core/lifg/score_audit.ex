defmodule Core.LIFG.ScoreAudit do
  @moduledoc """
  Observation-only diagnostics for LIFG's per-token sense probabilities.

  Each Stage1 softmax is normalized across the senses of one token, not
  across the sentence. Probabilities are not calibrated comprehension scores.
  When Stage1 uses :top2 or :none, the reported set is incomplete.
  """

  @max_tokens 16

  @spec summarize(list(), list(), list(), map(), atom()) :: [map()]
  def summarize(raw_choices, final_choices, tokens, virtual_mwes, scores_mode)
      when is_list(raw_choices) and is_list(final_choices) and
             is_list(tokens) and is_map(virtual_mwes) do
    virtual_indices = virtual_mwes |> Map.keys() |> MapSet.new()

    final_by_index =
      final_choices
      |> Enum.filter(&is_map/1)
      |> Map.new(fn choice -> {Map.get(choice, :token_index), choice} end)

    raw_choices
    |> Enum.filter(&is_map/1)
    |> Enum.reject(&MapSet.member?(virtual_indices, Map.get(&1, :token_index)))
    |> Enum.take(@max_tokens)
    |> Enum.flat_map(fn raw ->
      index = Map.get(raw, :token_index)
      if is_integer(index) and index >= 0 do
        scores =
          case Map.get(raw, :scores) do
            %{} = m ->
              m
              |> Enum.filter(fn {id, p} ->
                is_binary(id) and is_number(p) and p >= 0.0 and p <= 1.0
              end)
              |> Enum.sort_by(fn {_id, p} -> -p end)
            _ -> []
          end

        {top_id, top_p} =
          case scores do
            [{id, p} | _] -> {id, p}
            _ -> {nil, nil}
          end

        {runner_up_id, runner_up_p} =
          case scores do
            [_, {id, p} | _] -> {id, p}
            _ -> {nil, nil}
          end

        pos_mass =
          Enum.reduce(scores, %{}, fn {id, probability}, acc ->
            pos = sense_pos(id)
            Map.update(acc, pos, probability, &(&1 + probability))
          end)

        n = length(scores)
        uniform_baseline = if n > 0, do: 1.0 / n, else: nil

        unit_score_softmax_ceiling =
          if n > 0, do: :math.exp(1.0) / (:math.exp(1.0) + n - 1), else: nil

        selected = Map.get(final_by_index, index, %{})
        token = Enum.find(tokens, &(is_map(&1) and Map.get(&1, :index) == index))

        [ %{
          token_index: index,
          token: if(is_map(token), do: Map.get(token, :phrase, ""), else: ""),
          raw_winner: Map.get(raw, :chosen_id),
          final_winner: Map.get(selected, :id),
          reported_top_id: top_id,
          reported_top_probability: top_p,
          runner_up_id: runner_up_id,
          runner_up_probability: runner_up_p,
          # Retain bounded actual Stage1 candidates even when Core omits the
          # full >64-sense probability map from the final choice. Diagnostics
          # must not confuse that memory optimization with missing evidence.
          reported_top_candidates:
            scores
            |> Enum.take(8)
            |> Enum.map(fn {id, probability} ->
              %{id: id, probability: probability}
            end),
          reported_probability_margin:
            if(is_number(top_p) and is_number(runner_up_p),
              do: max(top_p - runner_up_p, 0.0), else: nil),
          reported_candidate_count: length(scores),
          candidate_count_complete?: scores_mode == :all,
          uniform_baseline: uniform_baseline,
          unit_interval_logit_ceiling: unit_score_softmax_ceiling,
          pos_probability_mass: pos_mass,
          pos_candidate_counts:
            Enum.frequencies_by(scores, fn {id, _} -> sense_pos(id) end),
          normalized_entropy: entropy(scores, scores_mode),
          score_kind: :per_token_post_softmax_probability
        } ]
      else
        []
      end
    end)
  end

  def summarize(_, _, _, _, _), do: []

  # This ceiling assumes final logits are in [0, 1] and temperature is
  # one. Cerebellum/grammar may change that range; it is a comparison only.
  defp sense_pos(id) when is_binary(id) do
    case String.split(id, "|", parts: 3) do
      [_, pos, _] -> pos
      _ -> "unknown"
    end
  end

  defp sense_pos(_), do: "unknown"

  defp entropy(scores, :all) when length(scores) > 1 do
    total = Enum.reduce(scores, 0.0, fn {_id, p}, acc -> acc + p end)

    if total > 0.0 do
      h =
        scores
        |> Enum.map(fn {_id, p} -> p / total end)
        |> Enum.reduce(0.0, fn p, sum ->
          if p > 0.0, do: sum - p * :math.log(p), else: sum
        end)

      h / :math.log(length(scores))
    else
      nil
    end
  end

  defp entropy(_, _), do: nil



  @doc "Read word-score audit metadata from a mixed pipeline trace."
  @spec trace_reports(term()) :: [map()]
  def trace_reports(trace) when is_list(trace) do
    trace
    |> Enum.flat_map(fn
      %{stage: :lifg_word_score_audit, meta: %{} = report} -> [report]
      _ -> []
    end)
    |> Enum.take(1)
  end

  def trace_reports(_), do: []

end
