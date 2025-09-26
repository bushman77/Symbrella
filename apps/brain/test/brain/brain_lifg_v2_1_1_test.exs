
defmodule BrainLIFGv211Test do
  use ExUnit.Case, async: true
  alias Brain.LIFG

  test "softmax with scores: :all yields group scores summing to ~1.0" do
    cands = [
      %{id: "a", token_index: 0, lemma: "x", pos: "n", embedding: [1.0, 0.0], lex_fit: 0.5, rel_prior: 0.5, intent_bias: 0.5, activation: 0.5},
      %{id: "b", token_index: 0, lemma: "x", pos: "n", embedding: [0.0, 1.0], lex_fit: 0.5, rel_prior: 0.5, intent_bias: 0.5, activation: 0.5}
    ]
    {:ok, out} = LIFG.disambiguate_stage1(cands, [1.0, 0.0], normalize: :softmax, scores: :all)
    [choice] = out.choices
    sum = choice.scores |> Map.values() |> Enum.sum()
    assert abs(sum - 1.0) < 1.0e-6
  end
end
