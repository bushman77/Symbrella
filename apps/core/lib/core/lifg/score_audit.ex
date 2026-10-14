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
          reported_probability_margin:
            if(is_number(top_p) and is_number(runner_up_p),
              do: max(top_p - runner_up_p, 0.0), else: nil),
          reported_candidate_count: length(scores),
          candidate_count_complete?: scores_mode == :all,
          score_kind: :per_token_post_softmax_probability
        } ]
      else
        []
      end
    end)
  end

  def summarize(_, _, _, _, _), do: []
end
