defmodule Brain.WM.PolicyTest do
  use ExUnit.Case, async: true

  alias Brain.WM.Policy

  test "apply_decay updates wm_last_ms and decays scores" do
    state = %{wm: [%{id: "a", score: 1.0}], wm_last_ms: 0}

    out = Policy.apply_decay(state, 10)

    assert is_integer(out.wm_last_ms)
    assert [%{id: "a", score: score}] = out.wm
    assert score <= 1.0
  end

  test "evict_if_needed keeps highest-scoring entries within capacity" do
    state = %{wm: [%{id: 1, score: 0.2}, %{id: 2, score: 0.9}], wm_cfg: %{capacity: 1}}

    out = Policy.evict_if_needed(state)

    assert length(out.wm) == 1
    assert hd(out.wm).id == 2
  end

  test "decay_and_evict composes both steps" do
    state = %{
      wm: [%{id: 1, score: 0.2}, %{id: 2, score: 0.9}],
      wm_cfg: %{capacity: 1},
      wm_last_ms: 0
    }

    out = Policy.decay_and_evict(state, 10)

    assert length(out.wm) == 1
    assert is_integer(out.wm_last_ms)
  end

  test "neutral self-state preserves the existing gate score" do
    cand = %{
      id: "demo|noun|1",
      lemma: "demo",
      score: 0.30,
      source: :other
    }

    cfg = %{
      recency_weight: 0.0,
      intent_weight: 0.0,
      semantic_boost: 0.0
    }

    legacy_score = Policy.gate_score_for(cand, 0.20, cfg)

    neutral_score =
      Policy.gate_score_for(
        cand,
        0.20,
        cfg,
        %Brain.SelfModel{}
      )

    assert_in_delta neutral_score, legacy_score, 1.0e-12
    assert_in_delta Policy.self_state_bias(%Brain.SelfModel{}), 0.0, 1.0e-12
  end

  test "vigilance and uncertainty make WM more receptive" do
    cand = %{
      id: "demo|noun|1",
      lemma: "demo",
      score: 0.30,
      source: :other
    }

    cfg = %{
      recency_weight: 0.0,
      intent_weight: 0.0,
      semantic_boost: 0.0
    }

    neutral =
      Policy.gate_score_for(cand, 0.20, cfg, %Brain.SelfModel{})

    receptive =
      Policy.gate_score_for(
        cand,
        0.20,
        cfg,
        %Brain.SelfModel{
          vigilance: 1.0,
          uncertainty: 1.0
        }
      )

    assert receptive > neutral
  end

  test "inhibition and cognitive overload make WM more selective" do
    cand = %{
      id: "demo|noun|1",
      lemma: "demo",
      score: 0.30,
      source: :other
    }

    cfg = %{
      recency_weight: 0.0,
      intent_weight: 0.0,
      semantic_boost: 0.0
    }

    neutral =
      Policy.gate_score_for(cand, 0.20, cfg, %Brain.SelfModel{})

    constrained =
      Policy.gate_score_for(
        cand,
        0.20,
        cfg,
        %Brain.SelfModel{
          inhibition: 1.0,
          cognitive_load: 1.0
        }
      )

    assert constrained < neutral
  end

  test "self-state gate scores remain bounded" do
    high =
      Policy.gate_score_for(
        %{id: "high|noun|1", lemma: "high", score: 1.0, source: :runtime},
        1.0,
        %{},
        %Brain.SelfModel{
          vigilance: 1.0,
          uncertainty: 1.0
        }
      )

    low =
      Policy.gate_score_for(
        %{id: "low|noun|1", lemma: "low", score: 0.0, source: :other},
        0.0,
        %{},
        %Brain.SelfModel{
          vigilance: 0.0,
          uncertainty: 0.0,
          inhibition: 1.0,
          cognitive_load: 1.0
        }
      )

    assert high >= 0.0 and high <= 1.0
    assert low >= 0.0 and low <= 1.0
  end
end
