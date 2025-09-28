defmodule BrainLIFGPropertyTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

  alias Brain.LIFG
  alias Core.SemanticInput

  defp approx(a, b, eps \\ 1.0e-6), do: abs(a - b) <= eps

  # Run LIFG via the SI-based API, return just what the properties need
  defp lifg_run(cands, ctx) do
    token_idxs =
      cands
      |> Enum.map(& &1.token_index)
      |> Enum.uniq()
      |> Enum.sort()

    tokens = Enum.map(token_idxs, fn i -> %{index: i, phrase: "t#{i}"} end)

    si =
      struct(SemanticInput, %{
        tokens: tokens,
        active_cells: cands,
        context_vec: ctx
      })

    si2 = LIFG.disambiguate_stage1(si)

    ev =
      case si2.trace do
        [head | _] when is_map(head) -> head
        _ -> %{}
      end

    %{
      choices: Map.get(ev, :choices, []),
      boosts: Map.get(ev, :boosts, []),
      inhibitions: Map.get(ev, :inhibitions, [])
    }
  end

  defp candidate_gen(dim) do
    gen all(
          token_index <- integer(0..15),
          lemma <- string(:alphanumeric, min_length: 1, max_length: 10),
          id <- string(:alphanumeric, min_length: 3, max_length: 12),
          pos <- member_of(~w(noun verb adj adv)),
          lex_fit <- float(min: 0.0, max: 1.0),
          rel_prior <- float(min: 0.0, max: 1.0),
          intent_bias <- float(min: 0.0, max: 1.0),
          activation <- float(min: 0.0, max: 1.0),
          embedding <- list_of(float(min: -1.0, max: 1.0), length: dim)
        ) do
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
    gen all(base <- list_of(candidate_gen(dim), length: groups * senses_per_group)) do
      base
      |> Enum.with_index()
      |> Enum.map(fn {cand, i} ->
        t = div(i, senses_per_group)

        Map.merge(cand, %{
          token_index: t,
          lemma: "lemma_#{t}",
          id: "t#{t}|s#{rem(i, senses_per_group)}"
        })
      end)
    end
  end

  property "softmax normalization yields per-group sums ≈ 1 and score range [0,1]" do
    check all(
            groups <- integer(1..8),
            senses <- integer(2..6),
            dim <- member_of([16, 32, 64]),
            cands <- grouped_candidates_gen(groups, senses, dim),
            ctx <- list_of(float(min: -1.0, max: 1.0), length: dim)
          ) do
      %{choices: choices} = lifg_run(cands, ctx)

      by_token = Enum.group_by(choices, & &1.token_index)
      assert map_size(by_token) == groups

      Enum.each(choices, fn ch ->
        vals = Map.values(ch.scores)
        sum = Enum.reduce(vals, 0.0, &+/2)
        # LIFG normalizes (softmax); per-choice score map sums to 1.0
        assert approx(sum, 1.0, 1.0e-6)
        assert Enum.all?(vals, fn v -> v >= 0.0 and v <= 1.0 end)
        assert ch.margin >= 0.0 and ch.margin <= 1.0
      end)
    end
  end

  property "boosts target winners; inhibitions exclude winners and cover the rest" do
    check all(
            groups <- integer(1..6),
            senses <- integer(2..5),
            dim <- member_of([8, 16, 32]),
            cands <- grouped_candidates_gen(groups, senses, dim),
            ctx <- list_of(float(min: -1.0, max: 1.0), length: dim)
          ) do
      %{choices: choices, boosts: boosts, inhibitions: inhibs} = lifg_run(cands, ctx)

      winners = MapSet.new(Enum.map(choices, & &1.chosen_id))
      boosted = MapSet.new(Enum.map(boosts, &elem(&1, 0)))
      inhibited = MapSet.new(Enum.map(inhibs, &elem(&1, 0)))
      all_ids = MapSet.new(Enum.map(cands, & &1.id))

      # Each winner should be boosted, not inhibited
      assert MapSet.subset?(winners, boosted)
      assert MapSet.disjoint?(winners, inhibited)

      # Combined coverage equals all candidates
      union = MapSet.union(boosted, inhibited)
      assert union == all_ids
    end
  end
end
