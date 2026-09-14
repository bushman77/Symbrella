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
      # raises thr as WM fills
      fullness_penalty_mult: 0.5
    }

    {decision, _score} = BasalGanglia.decide(wm, cand, attn, cfg)
    # sanity
    assert decision in [:block, :allow]
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

  test "cooldown forces :boost even if below gate" do
    wm = [%{id: "x", last_bump: System.system_time(:millisecond)}]
    cand = %{id: "x", source: :hippocampus, score: 0.1}
    attn = %{}
    cfg = %{gate_threshold: 0.6, cooldown_ms: 2000}
    {decision, _} = Brain.BasalGanglia.decide(wm, cand, attn, cfg)
    assert decision == :boost
  end

  test "neutral self-state preserves the neutral decision score" do
    cand = %{id: "neutral|noun|0", lemma: "neutral", source: :other, score: 0.48}
    cfg = %{gate_threshold: 0.5, lifg_min_score: 0.0}

    {decision0, score0} = BasalGanglia.decide([], cand, %{}, cfg)

    {decision1, score1} =
      BasalGanglia.decide([], cand, %{self_state: %Brain.SelfModel{}}, cfg)

    assert decision1 == decision0
    assert_in_delta score1, score0, 1.0e-12
  end

  test "vigilance and uncertainty can admit borderline evidence" do
    cand = %{id: "borderline|noun|0", lemma: "borderline", source: :other, score: 0.48}
    cfg = %{gate_threshold: 0.5, lifg_min_score: 0.2}

    assert {:block, neutral_score} =
             BasalGanglia.decide([], cand, %{self_state: %Brain.SelfModel{}}, cfg)

    {decision, receptive_score} =
      BasalGanglia.decide(
        [],
        cand,
        %{self_state: %Brain.SelfModel{vigilance: 1.0, uncertainty: 1.0}},
        cfg
      )

    assert decision in [:allow, :boost]
    assert receptive_score > neutral_score
  end

  test "inhibition and high cognitive load suppress borderline evidence" do
    cand = %{id: "strained|noun|0", lemma: "strained", source: :other, score: 0.53}
    cfg = %{gate_threshold: 0.5, lifg_min_score: 0.2}

    assert {decision0, neutral_score} =
             BasalGanglia.decide([], cand, %{self_state: %Brain.SelfModel{}}, cfg)

    assert decision0 == :allow

    {decision1, constrained_score} =
      BasalGanglia.decide(
        [],
        cand,
        %{self_state: %Brain.SelfModel{inhibition: 1.0, cognitive_load: 1.0}},
        cfg
      )

    assert decision1 == :block
    assert constrained_score < neutral_score
  end

  test "self-state cannot bypass hard evidence floors and scores remain bounded" do
    cand = %{id: "floor|noun|0", lemma: "floor", source: :other, score: 0.05}

    {decision, score} =
      BasalGanglia.decide(
        [],
        cand,
        %{self_state: %Brain.SelfModel{vigilance: 1.0, uncertainty: 1.0}},
        %{gate_threshold: 0.1, evidence_floor: 0.2}
      )

    assert decision == :block
    assert score >= 0.0 and score <= 1.0

    {_decision, high_score} =
      BasalGanglia.decide(
        [],
        %{id: "high|noun|0", lemma: "high", source: :other, score: 1.0},
        %{self_state: %Brain.SelfModel{vigilance: 1.0, uncertainty: 1.0}},
        %{gate_threshold: 0.1, lifg_min_score: 0.0}
      )

    assert high_score >= 0.0 and high_score <= 1.0
  end
end
