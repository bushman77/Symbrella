
defmodule BrainLIFGTest do
  use ExUnit.Case, async: true

  doctest Brain.LIFG
  alias Brain.LIFG

  defp approx_equal(a, b, eps \\ 1.0e-6), do: abs(a - b) <= eps
  defp sum(xs), do: Enum.reduce(xs, 0.0, &+/2)

  describe "cosine/2" do
    test "identical vectors -> 1.0" do
      assert LIFG.cosine([1.0, 2.0, 3.0], [1.0, 2.0, 3.0]) |> approx_equal(1.0)
    end
    test "orthogonal -> 0.0" do
      assert LIFG.cosine([1.0, 0.0], [0.0, 1.0]) |> approx_equal(0.0)
    end
    test "nil/zero handling" do
      assert LIFG.cosine(nil, [1.0]) == 0.0
      assert LIFG.cosine([0.0, 0.0], [1.0, 2.0]) == 0.0
    end
  end

  describe "normalize_scores/2" do
    test ":maxnorm divides by max" do
      assert LIFG.normalize_scores([1.0, 2.0, 4.0], :maxnorm) == [0.25, 0.5, 1.0]
    end
    test ":softmax sums to 1.0" do
      vals = [1.0, 2.0, 3.0]
      norm = LIFG.normalize_scores(vals, :softmax)
      assert approx_equal(sum(norm), 1.0, 1.0e-9)
    end
  end

  defp sample_candidates do
    [
      %{id: "bank|noun|money", token_index: 0, lemma: "bank", pos: "noun",
        embedding: [0.1, 0.2, 0.3], lex_fit: 0.9, rel_prior: 0.7, intent_bias: 0.6, activation: 0.5},
      %{id: "bank|noun|river", token_index: 0, lemma: "bank", pos: "noun",
        embedding: [0.0, 0.1, 0.0], lex_fit: 0.8, rel_prior: 0.4, intent_bias: 0.3, activation: 0.4},
      %{id: "charge|verb|money", token_index: 1, lemma: "charge", pos: "verb",
        embedding: [0.2, 0.2, 0.2], lex_fit: 0.8, rel_prior: 0.6, intent_bias: 0.7, activation: 0.6},
      %{id: "charge|verb|attack", token_index: 1, lemma: "charge", pos: "verb",
        embedding: [0.0, 0.3, 0.1], lex_fit: 0.7, rel_prior: 0.3, intent_bias: 0.2, activation: 0.4}
    ]
  end

  describe "disambiguate_stage1/3 basics" do
    test "winners and control signals" do
      {:ok, out} = LIFG.disambiguate_stage1(sample_candidates(), [0.1, 0.2, 0.2])
      chosen = out.choices |> Enum.map(& &1.chosen_id) |> Enum.sort()
      assert chosen == ["bank|noun|money", "charge|verb|money"]

      assert Enum.member?(out.boosts, {"bank|noun|money", 0.5})
      assert Enum.member?(out.inhibitions, {"bank|noun|river", -0.25})
      assert out.audit.groups == 2
    end

    test "margin -> alt_ids when low" do
      cands = [
        %{id: "w0a", token_index: 0, lemma: "bank", pos: "noun",
          embedding: [1.0, 0.0], lex_fit: 0.8, rel_prior: 0.5, intent_bias: 0.5, activation: 0.5},
        %{id: "w0b", token_index: 0, lemma: "bank", pos: "noun",
          embedding: [0.99, 0.0], lex_fit: 0.8, rel_prior: 0.5, intent_bias: 0.5, activation: 0.5}
      ]
      {:ok, %{choices: [choice]}} = LIFG.disambiguate_stage1(cands, [1.0, 0.0], margin_threshold: 0.12)
      assert length(choice.alt_ids) == 1
    end
  end

  describe "scores modes" do
    test ":all keeps full map" do
      {:ok, out} = LIFG.disambiguate_stage1(sample_candidates(), [0.1, 0.2, 0.2], scores: :all)
      for ch <- out.choices, do: assert map_size(ch.scores) == 2
    end

    test ":top2 keeps only top entries" do
      {:ok, out} = LIFG.disambiguate_stage1(sample_candidates(), [0.1, 0.2, 0.2], scores: :top2)
      for ch <- out.choices do
        assert map_size(ch.scores) in 1..2
      end
    end

    test ":none keeps empty map but inhibitions still cover losers" do
      {:ok, out} = LIFG.disambiguate_stage1(sample_candidates(), [0.1, 0.2, 0.2], scores: :none)
      for ch <- out.choices, do: assert ch.scores == %{}

      winners = MapSet.new(Enum.map(out.choices, & &1.chosen_id))
      losers = MapSet.new(Enum.map(sample_candidates(), & &1.id)) |> MapSet.difference(winners)
      inhibited = MapSet.new(Enum.map(out.inhibitions, &elem(&1, 0)))
      assert MapSet.subset?(losers, inhibited)
    end
  end

  describe "parallel equality" do
    test "parallel results equal serial results" do
      cands = for t <- 0..7, s <- 0..3 do
        %{
          id: "t#{t}|s#{s}",
          token_index: t,
          lemma: "lemma_#{t}",
          pos: "noun",
          embedding: [1.0 - s * 0.1, 0.0, s * 0.1],
          lex_fit: 0.6 + 0.1 * (3 - s),
          rel_prior: 0.5 + 0.05 * (3 - s),
          intent_bias: 0.5,
          activation: 0.5
        }
      end
      ctx = [1.0, 0.0, 0.0]
      {:ok, serial} = LIFG.disambiguate_stage1(cands, ctx, parallel: false)
      {:ok, parallel} = LIFG.disambiguate_stage1(cands, ctx, parallel: true, max_concurrency: System.schedulers_online())

      s_ids = Enum.sort(Enum.map(serial.choices, & &1.chosen_id))
      p_ids = Enum.sort(Enum.map(parallel.choices, & &1.chosen_id))
      assert s_ids == p_ids
    end
  end
end
