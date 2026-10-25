# apps/brain/test/brain/basal_ganglia_edges_test.exs
defmodule Brain.BasalGangliaEdgesTest do
  use ExUnit.Case, async: true
  alias Brain.BasalGanglia, as: BG

  defp wm_item(id, last_bump_ms) do
    %{
      id: id,
      activation: 0.4,
      score: 0.4,
      inserted_at: last_bump_ms,
      last_bump: last_bump_ms,
      payload: %{id: id, lemma: to_string(id)}
    }
  end

  test "duplicate penalty can flip allow → block (non-recent dup)" do
    now = System.system_time(:millisecond)
    wm = [wm_item("x|n|0", now - 60_000)]
    cand = %{id: "x|n|0", lemma: "x", source: :other, score: 0.45}
    cfg = %{gate_threshold: 0.4, dup_penalty: 0.5}
    {decision, _} = BG.decide(wm, cand, %{}, cfg)
    assert decision == :block
  end

  test "source_boosts nudges borderline over threshold" do
    cand = %{id: "y|n|0", source: :lifg, score: 0.35}
    cfg = %{gate_threshold: 0.4, source_boosts: %{lifg: 0.2}}
    {decision, s} = BG.decide([], cand, %{}, cfg)
    assert decision in [:allow, :boost]
    assert s >= 0.4
  end

  test "attention salience (goal term) raises score even for other source" do
    cand = %{lemma: "search", source: :other, score: 0.2}
    attn = %{goal_terms: ~w(search)}
    cfg = %{gate_threshold: 0.35}
    {decision, s} = BG.decide([], cand, attn, cfg)
    assert decision in [:allow, :boost]
    assert s >= 0.35
  end

  test "attention normalizes mixed-case multiword goals before WM gate scoring" do
    candidate = %{lemma: "the Pacific Ocean", source: :comprehension, score: 0.2}
    attention = %{goal_terms: ["the Pacific Ocean"]}

    assert Brain.Attention.salience(candidate, attention) == 1.0
    assert Brain.Attention.salience(%{candidate | lemma: "THE PACIFIC OCEAN"}, attention) == 1.0
    assert Brain.Attention.salience(candidate, %{goal_terms: ["unrelated topic"]}) == 0.0

    {decision, score} = BG.decide([], candidate, attention, %{gate_threshold: 0.4})
    assert decision in [:allow, :boost]
    assert score >= 0.4
  end

  test "cooldown disabled: recent dup does NOT force boost" do
    now = System.system_time(:millisecond)
    wm = [wm_item("z|n|0", now - 10)]
    cand = %{id: "z|n|0", score: 0.1, source: :other}
    cfg = %{gate_threshold: 0.4, cooldown_ms: 0}
    assert {:block, _} = BG.decide(wm, cand, %{}, cfg)
  end
end
