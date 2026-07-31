defmodule Brain.LIFG.SpanOrderInvariantTest do
  use ExUnit.Case, async: true

  test "choices preserve token order and are deterministic across runs" do
    si = %{
      sentence: "hello there",
      tokens: [
        %{index: 0, phrase: "hello", span: {0, 5}, n: 1},
        %{index: 1, phrase: "there", span: {6, 11}, n: 1}
      ],
      sense_candidates: %{
        0 => [
          %{id: "hello|interjection|1", features: %{pos: "interjection"}, lemma: "hello"},
          %{id: "hello|noun|1", features: %{pos: "noun"}, lemma: "hello"}
        ],
        1 => [
          %{id: "there|noun|1", features: %{pos: "noun"}, lemma: "there"},
          %{id: "there|adv|1", features: %{pos: "adverb"}, lemma: "there"}
        ]
      }
    }

    {:ok, %{choices: c1}} = Brain.LIFG.Stage1.run(si)
    {:ok, %{choices: c2}} = Brain.LIFG.Stage1.run(si)

    # Order follows token indices
    assert Enum.map(c1, & &1.token_index) == [0, 1]

    # Determinism: same winners across identical runs
    assert Enum.map(c1, & &1.chosen_id) == Enum.map(c2, & &1.chosen_id)

    # Margins well-formed
    assert Enum.all?(c1, fn ch -> is_number(ch.margin) and ch.margin >= 0.0 end)
  end
end
