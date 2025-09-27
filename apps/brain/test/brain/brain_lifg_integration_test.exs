
# test/brain/brain_lifg_integration_test.exs
defmodule BrainLIFGIntegrationTest do
  use ExUnit.Case, async: true
  alias Brain.LIFG

  @half_life 3.0   # seconds
  @dt 0.5          # step seconds
  @steps 6
  @k 2

  @decay :math.pow(0.5, @dt / @half_life)

  setup_all do
    # Make randomness deterministic for stable tests
    :rand.seed(:exsplus, {101, 202, 303})
    :ok
  end

  defp rerank(acts), do: Enum.sort_by(acts, &elem(&1, 1), :desc)

  defp step_decay(acts) do
    for {id, a} <- acts, do: {id, a * @decay}
  end

  defp apply_deltas(acts, deltas_map) do
    for {id, a} <- acts do
      {id, a + Map.get(deltas_map, id, 0.0)}
    end
  end

  test "winners stabilize in Top‑K; losers drift with decay + inhibitions" do
    # Three tokens × three senses = 9 candidates total
    cands =
      for t <- 0..2, s <- 0..2 do
        %{
          id: "t#{t}|s#{s}",
          token_index: t,
          lemma: "lemma_#{t}",
          pos: "noun",
          embedding: [1.0 - s * 0.1, 0.0, s * 0.1],
          lex_fit: 0.6 + 0.1 * (2 - s),
          rel_prior: 0.5 + 0.05 * (2 - s),
          intent_bias: 0.5,
          activation: 0.5
        }
      end

    ctx = [1.0, 0.0, 0.0]

    # Start with small, slightly noisy activations (deterministic due to seed)
    acts = for c <- cands, do: {c.id, 0.5 + :rand.uniform() * 0.05}

    # One LIFG pass → winners and per-id deltas (boosts override inhibitions)
    {:ok, out} = LIFG.disambiguate_stage1(cands, ctx, margin_threshold: 0.05, normalize: :softmax, scores: :none)
    winners = out.choices |> Enum.map(& &1.chosen_id) |> MapSet.new()

    # sanity: one winner per token
    assert MapSet.size(winners) == 3

    deltas =
      out.inhibitions
      |> Map.new()
      |> Map.merge(Map.new(out.boosts))

    # Apply N steps of decay + constant control deltas; track Top‑K history
    {final_acts, topk_history} =
      Enum.reduce(1..@steps, {acts, []}, fn _step, {acc_acts, hist} ->
        new_acts =
          acc_acts
          |> step_decay()
          |> apply_deltas(deltas)

        topk =
          new_acts
          |> rerank()
          |> Enum.take(@k)
          |> Enum.map(&elem(&1, 0))
          |> MapSet.new()

        {new_acts, [topk | hist]}
      end)

    # Expect winners to appear in Top‑K in most steps
    winner_hits =
      Enum.count(topk_history, fn topk ->
        MapSet.size(MapSet.intersection(winners, topk)) >= 1
      end)

    assert winner_hits >= div(@steps * 2, 3)  # ≥ 2/3 of the time (i.e., ≥4/6)

    # Bonus: winners mean activation > losers mean activation at the end
    {w_acts, l_acts} =
      Enum.split_with(final_acts, fn {id, _a} -> MapSet.member?(winners, id) end)

    w_mean =
      w_acts
      |> Enum.map(&elem(&1, 1))
      |> then(fn xs -> Enum.sum(xs) / max(1, length(xs)) end)

    l_mean =
      l_acts
      |> Enum.map(&elem(&1, 1))
      |> then(fn xs -> Enum.sum(xs) / max(1, length(xs)) end)

    assert w_mean > l_mean
  end
end
