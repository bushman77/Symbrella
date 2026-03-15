defmodule BrainWMDynamicsTest do
  use ExUnit.Case, async: true

  alias Brain

  # Minimal but realistic WM config for these tests.
  # Includes everything Brain.WM.Policy.normalize_cfg/1 expects.
  defp state(cap, wm_cfg_overrides \\ %{}) do
    wm_cfg = %{
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
      # keep these neutral so they do not skew behavior
      half_life_ms: 7_500,
      novelty_window: 16,
      novelty_weight: 0.0,
      intent_weight: 0.0,
      recency_weight: 0.0,
      outcome_weight: 0.0,
      current_intent: nil,
      semantic_boost: 0.0
    }

    %{
      wm: [],
      wm_cfg: Map.merge(wm_cfg, wm_cfg_overrides)
    }
  end

  test "admits one candidate and preserves score" do
    s0 = state(3)

    cands = [
      %{
        id: "X/1",
        token_index: 0,
        lemma: "x",
        score: 0.2,
        source: :lifg,
        reason: :lifg_stage1
      }
    ]

    {wm1, added, removed} = Brain.__test_do_focus__(s0, cands, %{})

    assert added >= 1
    assert removed == 0

    assert length(wm1) == 1
    [first] = wm1
    assert first.id == "X/1"
    assert_in_delta first.score, 0.2, 1.0e-6
  end

  test "merge_duplicates?: true merges duplicate identity into one WM entry" do
    s0 = %{
      state(3)
      | wm: [
          # legacy-shape entry: score + ts only
          %{id: "A", score: 0.4, ts: 1}
        ]
    }

    cands = [
      %{id: "A", token_index: 0, lemma: "a", score: 0.6, source: :lifg}
    ]

    {wm1, added, removed} = Brain.__test_do_focus__(s0, cands, %{})

    # One candidate was admitted, but because merge_duplicates? is true,
    # WM should contain a single merged entry for "A".
    assert added == 1
    assert removed == 0

    assert length(wm1) == 1
    [only] = wm1
    assert only.id == "A"
    assert_in_delta only.score, 0.6, 1.0e-6
  end

  test "honors gate_threshold from state.wm_cfg for non-preferred source candidates" do
    s0 = state(3, %{gate_threshold: 0.8})

    cands = [
      %{
        id: "weak|noun|0",
        token_index: 0,
        lemma: "weak",
        score: 0.0,
        source: :test
      }
    ]

    {wm1, added, removed} = Brain.__test_do_focus__(s0, cands, %{})

    assert wm1 == []
    assert added == 0
    assert removed == 0
  end

  test "capacity respected; newest admission is retained and tail is trimmed" do
    s0 = %{
      state(2)
      | wm: [
          %{id: "A", score: 0.7, ts: 1},
          %{id: "B", score: 0.6, ts: 2}
        ]
    }

    cands = [
      %{id: "C", token_index: 0, lemma: "c", score: 0.9, source: :lifg}
    ]

    {wm1, added, removed} = Brain.__test_do_focus__(s0, cands, %{})

    assert added == 1
    assert removed == 1
    assert length(wm1) == 2

    # Current WM contract is prepend + trim:
    # [C | [A, B]] -> take(2) => [C, A]
    assert Enum.map(wm1, & &1.id) == ["C", "A"]
  end

  test "newest equal-score admissions stay at the front" do
    s0 = %{
      state(2)
      | wm: [
          %{id: "A", score: 0.5, ts: 1}
        ]
    }

    cands = [
      %{id: "B", token_index: 0, lemma: "b", score: 0.5, source: :lifg},
      %{id: "C", token_index: 1, lemma: "c", score: 0.5, source: :lifg}
    ]

    {wm1, added, removed} = Brain.__test_do_focus__(s0, cands, %{})

    assert added == 2
    assert removed == 1
    assert length(wm1) == 2

    # Both new candidates are prepended in arrival order, so the later one
    # stays at the very front after trimming.
    assert Enum.map(wm1, & &1.id) == ["C", "B"]
  end
end
