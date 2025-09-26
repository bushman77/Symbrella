
# test/brain/brain_lifg_property_test.exs
# Add to your deps (apps/brain/mix.exs):
#   {:stream_data, "~> 0.6", only: :test}
#
# Then run:
#   mix test test/brain/brain_lifg_property_test.exs

defmodule BrainLIFGPropertyTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

  alias Brain.LIFG

  defp approx(a, b, eps \\ 1.0e-6), do: abs(a - b) <= eps

  defp candidate_gen(dim) do
    gen all token_index <- integer(0..15),
            lemma <- string(:alphanumeric, min_length: 1, max_length: 10),
            id <- string(:alphanumeric, min_length: 3, max_length: 12),
            pos <- member_of(~w(noun verb adj adv)a |> Enum.map(&to_string/1)),
            lex_fit <- float(min: 0.0, max: 1.0),
            rel_prior <- float(min: 0.0, max: 1.0),
            intent_bias <- float(min: 0.0, max: 1.0),
            activation <- float(min: 0.0, max: 1.0),
            embedding <- list_of(float(min: -1.0, max: 1.0), length: dim) do
      %{
        id: id,
        token_index: token_index,
        lemma: lemma,
        pos: pos,
        embedding: embedding,
        lex_fit: lex_fit,
        rel_prior: rel_prior,
        intent_bias: intent_bias,
        activation: activation
      }
    end
  end

  defp grouped_candidates_gen(groups, senses_per_group, dim) do
    gen all base <- list_of(candidate_gen(dim), length: groups * senses_per_group) do
      # Force token_index & lemma grouping exactly
      base
      |> Enum.with_index()
      |> Enum.map(fn {cand, i} ->
        t = div(i, senses_per_group)
        Map.merge(cand, %{token_index: t, lemma: "lemma_#{t}", id: "t#{t}|s#{rem(i, senses_per_group)}"})
      end)
    end
  end

  property "softmax normalization yields per-group sums ≈ 1 and score range [0,1]" do
    check all groups <- integer(1..8),
              senses <- integer(2..6),
              dim <- member_of([16, 32, 64]),
              cands <- grouped_candidates_gen(groups, senses, dim),
              ctx <- list_of(float(min: -1.0, max: 1.0), length: dim) do

      {:ok, %{choices: choices}} = LIFG.disambiguate_stage1(cands, ctx)

      # Reconstruct groups and check score sums
      by_token = Enum.group_by(choices, & &1.token_index)
      assert map_size(by_token) == groups

      Enum.each(choices, fn ch ->
        sum = ch.scores |> Map.values() |> Enum.sum()
        assert approx(sum, 1.0, 1.0e-6) or approx(sum, 0.0, 1.0e-6) # handle degenerate all-zero edge
        Enum.each(ch.scores, fn {_id, s} ->
          assert s >= 0.0 and s <= 1.0
        end)
        assert ch.margin >= 0.0 and ch.margin <= 1.0
      end)
    end
  end

  property "boosts target winners; inhibitions exclude winners and cover the rest" do
    check all groups <- integer(1..6),
              senses <- integer(2..5),
              dim <- member_of([8, 16, 32]),
              cands <- grouped_candidates_gen(groups, senses, dim),
              ctx <- list_of(float(min: -1.0, max: 1.0), length: dim) do

      {:ok, %{choices: choices, boosts: boosts, inhibitions: inhibs}} =
        LIFG.disambiguate_stage1(cands, ctx)

      winners = MapSet.new(Enum.map(choices, & &1.chosen_id))
      boosted = MapSet.new(Enum.map(boosts, &elem(&1, 0)))
      inhibited = MapSet.new(Enum.map(inhibs, &elem(&1, 0)))

      # Each winner should be boosted, not inhibited
      assert MapSet.subset?(winners, boosted)
      assert MapSet.disjoint?(winners, inhibited)

      # Combined (boosted ∪ inhibited) should cover all candidate ids
      all_ids = MapSet.new(Enum.map(cands, & &1.id))
      union = MapSet.union(boosted, inhibited)
      assert MapSet.subset?(all_ids, union)
    end
  end
end
