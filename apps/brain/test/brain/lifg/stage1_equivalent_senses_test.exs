defmodule Brain.LIFG.Stage1EquivalentSensesTest do
  use ExUnit.Case, async: true

  alias Brain.LIFG.Stage1

  test "equivalent greeting senses do not cause false uncertainty" do
    candidates =
      for n <- 1..3 do
        %{
          id: "hi|interjection|#{n}",
          lemma: "hi",
          norm: "hi",
          pos: "interjection",
          definition: "A greeting.",
          activation: 0.5
        }
      end

    si = %{
      sentence: "hi",
      tokens: [
        %{
          index: 0,
          phrase: "hi",
          n: 1,
          mw: false,
          span: {0, 2}
        }
      ],
      sense_candidates: %{0 => candidates}
    }

    assert {:ok, %{choices: [choice], audit: audit}} =
             Stage1.run(
               si,
               scores: :all,
               preserve_sense_candidates: true
             )

    # Preserve the distinct lexical identifiers.
    assert map_size(choice.scores) == 3

    # Identical semantic evidence should not
    # create artificial uncertainty.
    refute choice.margin_weak?
    refute choice.low_confidence?
    refute audit.degraded?
  end
end
