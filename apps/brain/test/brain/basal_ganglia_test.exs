defmodule Brain.BasalGangliaTest do
  use ExUnit.Case, async: true

  alias Brain.BasalGanglia

  defp wm_item(id, last_bump_ms \\ 0) do
    %{
      id: id,
      activation: 0.4,
      score: 0.4,
      inserted_at: last_bump_ms,
      last_bump: last_bump_ms,
      payload: %{id: id, lemma: to_string(id)}
    }
  end

  test "preferred sources get :boost when above threshold" do
    wm = []
    cand = %{id: "x|noun|0", lemma: "x", source: :runtime, score: 0.5}
    attn = %{}
    cfg = %{gate_threshold: 0.4, prefer_sources: [:runtime, :recency, :lifg]}

    {decision, score} = BasalGanglia.decide(wm, cand, attn, cfg)
    assert decision == :boost
    assert score >= 0.4
  end

  test "blocks when under effective threshold (fullness raises threshold)" do
    # Fill WM close to capacity so threshold increases
    wm = Enum.map(1..6, &wm_item("w#{&1}"))
    cand = %{id: "z|noun|0", lemma: "z", source: :other, score: 0.41}
    attn = %{}
    cfg = %{
      capacity: 7,
      gate_threshold: 0.4,
      fullness_penalty_mult: 0.5   # raises thr as WM fills
    }

    {decision, _score} = BasalGanglia.decide(wm, cand, attn, cfg)
    assert decision in [:block, :allow] # sanity
    assert decision == :block
  end

  test "recent duplicate triggers :boost with cooldown" do
    now = System.system_time(:millisecond)
    wm = [wm_item("a|noun|0", now - 50)]
    cand = %{id: "a|noun|0", lemma: "a", source: :lifg, score: 0.2}
    attn = %{}
    cfg = %{gate_threshold: 0.6, cooldown_ms: 200}

    {decision, _score} = BasalGanglia.decide(wm, cand, attn, cfg)
    assert decision == :boost
  end
end

