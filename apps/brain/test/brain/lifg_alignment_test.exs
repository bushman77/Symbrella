defmodule Brain.LIFGAlignmentTest do
  use ExUnit.Case, async: true
  alias Brain.LIFG

  test "each choice stays attached to its token_index" do
    si0 = %{
      tokens: [
        %{index: 0, phrase: "this"},
        %{index: 1, phrase: "is"}
      ],
      # Provide ≥2 candidates per token_index
      active_cells: [
        %{token_index: 0, lemma: "this", id: "THIS/strong", score: 0.9},
        %{token_index: 0, lemma: "this", id: "THIS/weak", score: 0.1},
        %{token_index: 1, lemma: "is", id: "BE/strong", score: 0.8},
        %{token_index: 1, lemma: "is", id: "BE/weak", score: 0.2}
      ],
      trace: []
    }

    si1 = LIFG.disambiguate_stage1(si0)

    # Get the most recent event’s choices
    [evt | _] = si1.trace
    choices = evt.choices

    # We expect one choice per token (2 tokens)
    assert length(choices) == 2
    assert choices |> Enum.map(& &1.token_index) |> Enum.sort() == [0, 1]

    # Build the allowed ids per token_index from the input
    allowed =
      si0.active_cells
      |> Enum.group_by(& &1.token_index, & &1.id)

    # For each choice, ensure its chosen_id belongs to its own token’s candidate ids
    for ch <- choices do
      assert ch.chosen_id in Map.fetch!(allowed, ch.token_index)
    end
  end
end
