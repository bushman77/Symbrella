defmodule Brain.LIFG.ChoicesTest do
  use ExUnit.Case, async: true

  alias Brain.LIFG.Choices

  test "Option A: alt_ids are scored-only competitors; slate_alt_ids are slate-only candidates" do
    si_after = %{
      sense_candidates: %{
        1 => [
          %{id: "evening|noun|0"},
          %{id: "evening|verb|0"}
        ]
      }
    }

    raw_choices = [
      %{
        token_index: 1,
        chosen_id: "good evening|phrase|fallback",
        scores: %{"good evening|phrase|fallback" => 0.5},
        # even if present in raw choice, Option A must *not* mix this into alt_ids
        alt_ids: ["evening|noun|0"]
      }
    ]

    [ch] = Choices.augment(raw_choices, si_after, 0.05)

    assert ch.chosen_id == "good evening|phrase|fallback"

    # scored-only competitors excluding chosen => empty (singleton score map)
    assert ch.alt_ids == []

    # slate candidates not present in scores (and not chosen) => surfaced separately
    assert Enum.sort(ch.slate_alt_ids) == ["evening|noun|0", "evening|verb|0"]

    # singleton gap => 0.0, floor to min_margin
    assert ch.margin == 0.05
  end

  test "margin is computed from top-2 gap when missing, and only floored if below min_margin" do
    si_after = %{
      sense_candidates: %{
        4 => [
          %{id: "evening|noun|0"},
          %{id: "evening|verb|0"}
        ]
      }
    }

    raw_choices = [
      %{
        token_index: 4,
        # chosen_id omitted -> computed by argmax(scores)
        scores: %{
          "evening|noun|0" => 0.492629,
          "evening|verb|0" => 0.507371
        }
      }
    ]

    [ch] = Choices.augment(raw_choices, si_after, 0.01)

    assert ch.chosen_id == "evening|verb|0"
    assert ch.alt_ids == ["evening|noun|0"]

    # slate has only scored ids, so slate_alt_ids should be empty
    assert ch.slate_alt_ids == []

    # 0.507371 - 0.492629 = 0.014742
    assert ch.margin == 0.014742
  end

  test "existing raw alt_ids do not contaminate alt_ids under Option A" do
    si_after = %{
      sense_candidates: %{
        9 => [
          %{id: "x|noun|0"},
          %{id: "b|noun|0"}
        ]
      }
    }

    raw_choices = [
      %{
        token_index: 9,
        chosen_id: "a|noun|0",
        scores: %{
          "a|noun|0" => 0.9,
          "b|noun|0" => 0.1
        },
        # must not appear in alt_ids for Option A
        alt_ids: ["x|noun|0"]
      }
    ]

    [ch] = Choices.augment(raw_choices, si_after, 0.05)

    assert ch.chosen_id == "a|noun|0"
    assert Enum.sort(ch.alt_ids) == ["b|noun|0"]

    # slate-only ids (not scored) stay separate
    assert Enum.sort(ch.slate_alt_ids) == ["x|noun|0"]
  end
end
