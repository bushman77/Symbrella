defmodule Brain.LIFG.Stage1NumericTest do
  use ExUnit.Case, async: true

  alias Brain.LIFG.Stage1.Numeric

  test "weighted_scores matches scalar scoring for Stage1 feature rows" do
    rows = [
      %{lex_fit: 1.0, rel_prior: 0.8, activation: 0.4, intent_bias: 0.0},
      %{lex_fit: 0.2, rel_prior: 0.6, activation: 0.9, intent_bias: 0.5},
      %{lex_fit: 2.0, rel_prior: -1.0, activation: 0.5, intent_bias: 1.0}
    ]

    weights = %{lex_fit: 0.4, rel_prior: 0.3, activation: 0.2, intent_bias: 0.1}

    assert_close_list(Numeric.weighted_scores(rows, weights, mode: :scalar), [0.72, 0.49, 0.6])

    if Numeric.nx_available?() do
      assert_close_list(
        Numeric.weighted_scores(rows, weights),
        Numeric.weighted_scores(rows, weights, mode: :scalar)
      )
    end
  end

  test "softmax matches scalar scoring and remains stable for large logits" do
    logits = [10_000.0, 9_999.0, 9_998.0]
    scalar = Numeric.softmax(logits, mode: :scalar)

    assert_close_list(scalar, [0.6652409557748218, 0.24472847105479764, 0.09003057317038046])

    if Numeric.nx_available?() do
      assert_close_list(Numeric.softmax(logits), scalar)
    end
  end

  defp assert_close_list(left, right) do
    assert length(left) == length(right)

    Enum.zip(left, right)
    |> Enum.each(fn {a, b} ->
      assert_in_delta a, b, 1.0e-6
    end)
  end
end
