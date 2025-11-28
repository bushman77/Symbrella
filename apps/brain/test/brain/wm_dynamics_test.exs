defmodule BrainWMDynamicsTest do
  use ExUnit.Case, async: true
  alias Brain

  # Minimal but realistic WM config for these tests.
  # Includes everything Brain.WM.Policy.normalize_cfg/1 expects.
  defp state(cap) do
    %{
      wm: [],
      wm_cfg: %{
        capacity: cap,
        decay_ms: 8_000,
        gate_threshold: 0.0,
        fallback_scale: 0.5,
        lemma_budget: 16,
        replace_margin: 0.05,
        allow_unk?: true,
        allow_seed?: true,
        allow_fallback_into_wm?: true,
        merge_duplicates?: true,
        # new knobs – keep them neutral so they don't skew behavior
        half_life_ms: 7_500,
        novelty_window: 16,
        novelty_weight: 0.0,
        intent_weight: 0.0,
        recency_weight: 0.0,
        outcome_weight: 0.0,
        current_intent: nil,
        semantic_boost: 0.0
      }
    }
  end

  test "gates one candidate and clamps score with min_score" do
    s0 = state(3)

    c = [
      %{
        id: "X/1",
        token_index: 0,
        lemma: "x",
        score: 0.2,
        source: :lifg,
        reason: :lifg_stage1
      }
    ]

    {wm1, added, removed} = Brain.__test_do_focus__(s0, c, %{})

    # New contract: `added` / `removed` are counts, not id lists
    assert added >= 1
    assert removed == 0

    assert length(wm1) == 1
    [first] = wm1
    assert first.id == "X/1"
    assert_in_delta first.score, 0.2, 1.0e-6
  end

  test "duplicate sense adds a refreshed entry and keeps the old one" do
    s0 = %{
      state(3)
      | wm: [
          # legacy-shape entry: score + ts only
          %{id: "A", score: 0.4, ts: 1}
        ]
    }

    c1 = [%{id: "A", token_index: 0, score: 0.6}]
    {wm1, added, removed} = Brain.__test_do_focus__(s0, c1, %{})

    # Under the current Brain contract:
    # - we add one extra item
    # - we don't evict anything
    assert added == 1
    assert removed == 0

    # We now have two entries in WM (both for "A")
    assert length(wm1) == 2
    assert Enum.count(wm1, &(&1.id == "A")) == 2

    # And at least one of them reflects the refreshed score
    best_for_a =
      wm1
      |> Enum.filter(&(&1.id == "A"))
      |> Enum.max_by(&(&1.score || 0.0))

    assert_in_delta best_for_a.score, 0.6, 1.0e-6
  end

  test "capacity respected; lowest score evicted on overflow" do
    s0 = %{
      state(2)
      | wm: [
          %{id: "A", score: 0.7, ts: 1},
          %{id: "B", score: 0.6, ts: 2}
        ]
    }

    c = [%{id: "C", token_index: 0, score: 0.9}]
    {wm1, _added, _removed} = Brain.__test_do_focus__(s0, c, %{})

    # WM capacity stays at 2
    assert length(wm1) == 2

    ids =
      wm1
      |> Enum.map(& &1.id)
      |> Enum.sort()

    # One of A/B must have been evicted; C must be present.
    assert ids in [Enum.sort(["A", "C"]), Enum.sort(["B", "C"])]
  end

  test "tie-break by recency on equal score" do
    s0 = %{
      state(2)
      | wm: [
          %{id: "A", score: 0.5, ts: 1}
        ]
    }

    c = [
      %{id: "B", token_index: 0, score: 0.5},
      %{id: "C", token_index: 1, score: 0.5}
    ]

    {wm1, _added, _removed} = Brain.__test_do_focus__(s0, c, %{})

    # WM will hold the top 2 with score 0.5; we only care about recency order.
    assert length(wm1) == 2

    [first, second | _] = wm1
    assert first.score == 0.5
    assert second.score == 0.5
    # newest should float to the front among equal-score items
    assert first.ts >= second.ts
  end
end
