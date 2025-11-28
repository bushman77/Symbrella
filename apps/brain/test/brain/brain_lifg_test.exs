defmodule BrainLIFGTest do
  use ExUnit.Case, async: true

  doctest Brain.LIFG
  alias Brain.LIFG
  alias Core.SemanticInput

  # ── helpers ──────────────────────────────────────────────────────────────────
  defp approx_equal(a, b, eps \\ 1.0e-6), do: abs(a - b) <= eps
  defp sum(xs), do: Enum.reduce(xs, 0.0, &+/2)

  defp lifg_event!(%{trace: [%{stage: :lifg_stage1} = ev | _]}), do: ev

  defp lifg_event!(%{trace: trace}) when is_list(trace) do
    Enum.find(trace, &match?(%{stage: :lifg_stage1}, &1)) ||
      flunk("No lifg_stage1 event in trace")
  end

  defp sample_si(ctx_vec) do
    # Explicit indices so slate + Stage1 bucket candidates correctly
    tokens = [
      %{index: 0, phrase: "bank"},
      %{index: 1, phrase: "charge"}
    ]

    # Attach token_index so Input.slate_for/1 can group by token
    cells = [
      %{
        id: "bank|noun|money",
        word: "bank",
        pos: "noun",
        token_index: 0,
        embedding: [0.1, 0.2, 0.3],
        lex_fit: 0.9,
        rel_prior: 0.7,
        intent_bias: 0.6,
        activation: 0.5
      },
      %{
        id: "bank|noun|river",
        word: "bank",
        pos: "noun",
        token_index: 0,
        embedding: [0.0, 0.1, 0.0],
        lex_fit: 0.8,
        rel_prior: 0.4,
        intent_bias: 0.3,
        activation: 0.4
      },
      %{
        id: "charge|verb|money",
        word: "charge",
        pos: "verb",
        token_index: 1,
        embedding: [0.2, 0.2, 0.2],
        lex_fit: 0.8,
        rel_prior: 0.6,
        intent_bias: 0.7,
        activation: 0.6
      },
      %{
        id: "charge|verb|attack",
        word: "charge",
        pos: "verb",
        token_index: 1,
        embedding: [0.0, 0.3, 0.1],
        lex_fit: 0.7,
        rel_prior: 0.3,
        intent_bias: 0.2,
        activation: 0.4
      }
    ]

    %SemanticInput{
      sentence: "bank charge",
      source: :test,
      tokens: tokens,
      active_cells: cells
    }
    # we pass context via a loose field the LIFG reads with Map.get/3
    |> Map.put(:context_vec, ctx_vec)
  end

  # ── unit: cosine ─────────────────────────────────────────────────────────────
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

  # ── unit: normalize_scores (softmax only) ────────────────────────────────────
  describe "normalize_scores/1" do
    test "softmax sums to 1.0" do
      vals = [1.0, 2.0, 3.0]
      norm = LIFG.normalize_scores(vals)
      assert approx_equal(sum(norm), 1.0, 1.0e-9)
    end

    test "uniform inputs -> uniform distribution" do
      norm = LIFG.normalize_scores([0.0, 0.0, 0.0])
      # exp(0)=1 → 1/3 each
      assert Enum.all?(norm, &approx_equal(&1, 1.0 / 3.0))
    end
  end

  # ── integration: SI → LIFG → SI (choices in trace) ───────────────────────────
  describe "disambiguate_stage1/1 basics" do
    test "winners and control signals are emitted in trace" do
      si = sample_si([0.1, 0.2, 0.2]) |> LIFG.disambiguate_stage1()
      ev = lifg_event!(si)

      chosen =
        ev.choices
        |> Enum.map(& &1.chosen_id)
        |> Enum.sort()

      assert chosen == ["bank|noun|money", "charge|verb|money"]

      # boosts/inhibitions are now maps like %{id, token_index, amount}
      boost_ids =
        ev.boosts
        |> Enum.map(fn
          {id, _amt} -> id
          %{id: id} -> id
        end)

      inhib_ids =
        ev.inhibitions
        |> Enum.map(fn
          {id, _amt} -> id
          %{id: id} -> id
        end)

      assert "bank|noun|money" in boost_ids
      assert "bank|noun|river" in inhib_ids

      # groups are implicit in choices via token_index
      group_count =
        ev.choices
        |> Enum.map(& &1.token_index)
        |> Enum.uniq()
        |> length()

      assert group_count == 2
    end

    test "per-group softmax sums ≈ 1.0" do
      si = sample_si([0.1, 0.2, 0.2]) |> LIFG.disambiguate_stage1()
      ev = lifg_event!(si)

      for ch <- ev.choices do
        s = ch.scores |> Map.values() |> Enum.sum()
        assert approx_equal(s, 1.0, 1.0e-6)
      end
    end

    test "low margin → runner-up appears in alt_ids" do
      # Create a single-token SI with two *nearly identical* senses
      tokens = [%{phrase: "bank"}]

      cells = [
        %{
          id: "w0a",
          word: "bank",
          pos: "noun",
          embedding: [1.0, 0.0],
          lex_fit: 0.8,
          rel_prior: 0.5,
          intent_bias: 0.5,
          activation: 0.5
        },
        %{
          id: "w0b",
          word: "bank",
          pos: "noun",
          embedding: [0.9999, 0.0],
          lex_fit: 0.8,
          rel_prior: 0.5,
          intent_bias: 0.5,
          activation: 0.5
        }
      ]

      si =
        %SemanticInput{
          sentence: "bank",
          source: :test,
          tokens: tokens,
          active_cells: cells
        }
        |> Map.put(:context_vec, [1.0, 0.0])
        |> LIFG.disambiguate_stage1()

      ev = lifg_event!(si)
      assert length(ev.choices) == 1
      [choice] = ev.choices
      assert length(choice.alt_ids) in 0..1
      # With near-identical scores and default margin_threshold (0.12),
      # we expect a runner-up to appear.
      assert length(choice.alt_ids) == 1
    end
  end
end
