
# test/brain/brain_lifg_integration_test.exs
# Pure integration-style test that simulates Brain's focus narrowing using
# LIFG’s boosts/inhibitions + a simple decay model. No GenServers involved.

defmodule BrainLIFGIntegrationTest do
  use ExUnit.Case, async: true
  alias Brain.LIFG

  @half_life 3.0  # seconds
  @dt 0.5         # step seconds
  @decay :math.pow(0.5, @dt / @half_life)  # per-step decay factor
  @k 2           # focus size

  defp rerank(acts), do: acts |> Enum.sort_by(&elem(&1, 1), :desc)

  defp step_decay(acts) do
    for {id, a} <- acts, do: {id, a * @decay}
  end

  defp apply_deltas(acts, deltas_map) do
    for {id, a} <- acts do
      {id, a + Map.get(deltas_map, id, 0.0)}
    end
  end

  test "winners stabilize in Top‑K; losers drift out with decay + inhibitions" do
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

    # Start with small, slightly noisy activations
    acts = for c <- cands, do: {c.id, 0.5 + :rand.uniform() * 0.05}

    # One LIFG pass
    {:ok, out} = LIFG.disambiguate_stage1(cands, ctx, margin_threshold: 0.05)
    winners = Enum.map(out.choices, & &1.chosen_id) |> MapSet.new()

    # Build deltas
    boost = Map.new(out.boosts)
    inhib = Map.new(out.inhibitions)
    deltas = Map.merge(inhib, boost) # boosts override inhibitions for winners

    # Apply 6 steps of decay+control and track Top‑K stability
    topk_history =
      1..6
      |> Enum.reduce({acts, []}, fn _step, {acc_acts, hist} ->
        acc_acts
        |> step_decay()
        |> apply_deltas(deltas)
        |> then(fn new_acts ->
          topk = new_acts |> rerank() |> Enum.take(@k) |> Enum.map(&elem(&1, 0)) |> MapSet.new()
          {new_acts, [topk | hist]}
        end)
      end)
      |> elem(1)

    # Expect winners to appear in Top‑K most of the time
    winner_hits =
      Enum.count(topk_history, fn topk -> MapSet.subset?(winners, topk) or MapSet.size(MapSet.intersection(winners, topk)) >= 1 end)

    assert winner_hits >= 4  # in at least 4/6 steps, a winner is in Top‑K
  end
end
