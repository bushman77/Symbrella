defmodule Brain.LIFG.Stage1DistinctSensesTest do
  use ExUnit.Case, async: true

  alias Brain.LIFG.Stage1

  test "different meanings retain uncertainty" do
    si = %{
      sentence: "bank",
      tokens: [
        %{
          index: 0,
          phrase: "bank",
          n: 1,
          mw: false,
          span: {0, 4}
        }
      ],
      sense_candidates: %{
        0 => [
          %{
            id: "bank|noun|11",
            lemma: "bank",
            pos: "noun",
            definition: "The edge of a river.",
            activation: 0.5
          },
          %{
            id: "bank|noun|22",
            lemma: "bank",
            pos: "noun",
            definition: "A financial institution.",
            activation: 0.5
          }
        ]
      }
    }

    assert {:ok, %{choices: [choice], audit: audit}} =
             Stage1.run(
               si,
               scores: :all,
               preserve_sense_candidates: true
             )

    assert map_size(choice.scores) == 2
    assert choice.low_confidence?
    assert choice.margin_weak?
    assert audit.degraded?
  end
end
