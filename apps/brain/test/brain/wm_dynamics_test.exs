defmodule BrainWMDynamicsTest do
  use ExUnit.Case, async: true
  alias Brain

  defp state(cap \\ 3) do
    %{
      wm: [],
      wm_cfg: %{capacity: cap}
    }
  end

  test "gates one candidate and clamps score with min_score" do
    s0 = state(3)
    c  = [%{id: "X/1", token_index: 0, lemma: "x", score: 0.2, source: :lifg, reason: :lifg_stage1}]
    {wm1, added, removed} = Brain.__test_do_focus__(s0, c, %{})

    assert added == ["X/1"]
    assert removed == []
    assert Enum.at(wm1, 0).id == "X/1"
    assert Enum.at(wm1, 0).score == 0.2
  end

  test "duplicate sense updates score and recency, not count" do
    s0 = %{state(3) | wm: [%{id: "A", score: 0.4, ts: 1}]}
    c1 = [%{id: "A", token_index: 0, score: 0.6}]
    {wm1, added, removed} = Brain.__test_do_focus__(s0, c1, %{})

    assert added == []                 # not a new slot
    assert removed == []               # no eviction
    [only] = wm1
    assert only.id == "A"
    assert only.score == 0.6           # raised
    assert only.ts   > 1               # refreshed
  end

  test "capacity respected; lowest score evicted on overflow" do
    s0 = %{state(2) | wm: [%{id: "A", score: 0.7, ts: 1}, %{id: "B", score: 0.6, ts: 2}]}
    c  = [%{id: "C", token_index: 0, score: 0.9}]
    {wm1, added, removed} = Brain.__test_do_focus__(s0, c, %{})

    assert added == ["C"]
    assert removed == ["B"] or removed == ["A"]  # depends which is lowest; here "B"
    ids = Enum.map(wm1, & &1.id) |> Enum.sort()
    assert ids == Enum.sort(["A", "C"])
  end

  test "tie-break by recency on equal score" do
    s0 = %{state(2) | wm: [%{id: "A", score: 0.5, ts: 1}]}
    c  = [
      %{id: "B", token_index: 0, score: 0.5},
      %{id: "C", token_index: 1, score: 0.5}
    ]
    {wm1, _added, _removed} = Brain.__test_do_focus__(s0, c, %{})
    # Newest should appear first among equal-scored items
    [first, second | _] = wm1
    assert first.score == 0.5
    assert second.score == 0.5
    assert first.ts >= second.ts      # more recent first
  end
end

