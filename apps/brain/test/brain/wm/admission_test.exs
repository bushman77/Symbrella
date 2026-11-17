defmodule Brain.WM.AdmissionTest do
  use ExUnit.Case, async: false

  alias Brain.WM.Admission

  def handle_gate(_event, meas, meta, pid) when is_pid(pid) do
    send(pid, {:gate, meas, meta})
    :ok
  end

  setup do
    id = "wm-admission-test-#{System.unique_integer([:positive])}"
    :ok = :telemetry.attach(id, [:brain, :gate, :decision], &__MODULE__.handle_gate/4, self())
    on_exit(fn -> :telemetry.detach(id) end)
    :ok
  end

  defp state(overrides \\ %{}) do
    %{
      wm: [],
      attention: %{},
      wm_cfg:
        Map.merge(
          %{
            capacity: 4,
            decay_ms: 30_000,
            gate_threshold: 0.5,
            lifg_min_score: 0.0,
            merge_duplicates?: true,
            allow_unk?: true,
            allow_seed?: true,
            allow_fallback_into_wm?: true,
            fallback_scale: 1.0,
            semantic_boost: 0.0,
            recency_weight: 0.0,
            intent_weight: 0.0,
            novelty_weight: 0.0,
            outcome_weight: 0.0
          },
          overrides
        )
    }
  end

  test "blocked candidate emits one canonical BG event and does not insert" do
    {wm, added, removed} =
      Admission.run(
        state(),
        [%{id: "weak|noun|0", lemma: "weak", source: :other, score: 0.1}],
        []
      )

    assert wm == []
    assert added == 0
    assert removed == 0

    assert_receive {:gate, %{score: score, self_state_bias: self_state_bias},
                    %{decision: :block, gate: :basal_ganglia, id: "weak|noun|0"}}

    assert self_state_bias == 0.0
    assert score < 0.5
    refute_receive {:gate, _meas, _meta}, 50
  end

  test "allow inserts a normalized WM entry and preserves evidence payload" do
    cand = %{
      id: "fact|noun|0",
      lemma: "fact",
      source: :other,
      score: 0.7,
      margin: 0.22,
      semantic_bias: 0.4,
      conflict: %{kind: :none},
      provenance: %{stage: :unit}
    }

    {wm, added, removed} = Admission.run(state(), [cand], [])

    assert added == 1
    assert removed == 0
    assert [%{id: "fact|noun|0", source: :other, payload: payload}] = wm
    assert payload.margin == 0.22
    assert payload.provenance == %{stage: :unit}

    assert_receive {:gate, _meas, %{decision: :allow, gate: :basal_ganglia, source: :other}}
  end

  test "boost inserts with boosted activation" do
    {wm, added, removed} =
      Admission.run(
        state(%{prefer_sources: [:runtime], boost_threshold_pref: 0.45}),
        [%{id: "boost|noun|0", lemma: "boost", source: :runtime, score: 0.6}],
        []
      )

    assert added == 1
    assert removed == 0
    assert [%{id: "boost|noun|0", score: score, activation: activation}] = wm
    assert activation > score

    assert_receive {:gate, _meas, %{decision: :boost, gate: :basal_ganglia, id: "boost|noun|0"}}
  end

  test "unscored maps, standalone margins and bare strings cannot enter WM" do
    opts = [
      gate_threshold: 0.0,
      boost_threshold: 0.0,
      boost_threshold_pref: 0.0,
      evidence_floor: 0.0
    ]

    candidates = [
      %{id: "missing|noun|0", lemma: "missing", source: :lifg},
      %{id: "margin|noun|0", lemma: "margin", margin: 1.0, source: :lifg},
      "raw|noun|0",
      %{id: "fallback|phrase|fallback", source: :runtime},
      %{id: "explicit|noun|0", source: :runtime, score: 0.8}
    ]

    assert Enum.map(Admission.normalize_candidates(candidates), & &1.id) ==
             ["explicit|noun|0"]

    {wm, 1, 0} = Admission.run(state(), candidates, opts)
    assert Enum.map(wm, & &1.id) == ["explicit|noun|0"]
    assert_receive {:gate, _meas, %{gate: :basal_ganglia, id: "explicit|noun|0"}}
    refute_receive {:gate, _meas, _meta}, 50
  end

  test "every runtime source class obeys the same hard evidence floor" do
    cfg = state(%{gate_threshold: 0.0, evidence_floor: 0.8,
                  lifg_min_score: 0.8, prefer_sources: []})

    Enum.each([:lifg, :ltm, :curiosity, :intent, :comprehension,
               :verified_semantics], fn source ->
      id = "source-floor-#{source}"
      {wm, 0, 0} = Admission.run(cfg, [%{id: id, source: source, score: 0.1}], [])
      assert wm == []
      assert_receive {:gate, %{score: score},
                      %{gate: :basal_ganglia, decision: :block, id: ^id,
                        source: ^source}}
      assert score >= 0.0 and score <= 1.0
    end)
  end

  test "self-state modulation cannot override the hard evidence floor" do
    cfg = state(%{gate_threshold: 0.0, evidence_floor: 0.6})
    state = %Brain.SelfModel{vigilance: 1.0, uncertainty: 1.0}
    candidate = %{id: "weak|noun|0", source: :other, score: 0.05}

    {wm, 0, 0} = Admission.run(cfg, [candidate], self_state: state)
    assert wm == []

    assert_receive {:gate, %{score: score, self_state_bias: bias},
                    %{gate: :basal_ganglia, decision: :block, id: "weak|noun|0",
                      self_state_applied?: true}}
    assert score >= 0.0 and score <= 1.0
    assert bias > 0.0
  end

  test "Stage2 tuple decisions cannot forge a source to evade the lexical evidence floor" do
    cfg =
      state(%{
        gate_threshold: 0.0,
        lifg_min_score: 0.8,
        evidence_floor: 0.0,
        boost_threshold: 0.0,
        boost_threshold_pref: 0.0,
        block_threshold: 0.0,
        source_boosts: %{},
        prefer_sources: []
      })

    forged = [
      {:commit, %{id: "forged-commit|verb|1", score: 0.2, source: :comprehension}},
      {:allow, %{id: "forged-allow|verb|1", score: 0.2, source: :verified_semantics}},
      {:boost, %{id: "forged-boost|verb|1", score: 0.2, source: :runtime}}
    ]

    normalized = Admission.normalize_candidates(forged)
    assert length(normalized) == 3
    assert Enum.all?(normalized, &(&1.source == :lifg))

    {wm, added, removed} = Admission.run(cfg, forged, [])
    assert wm == []
    assert added == 0
    assert removed == 0

    Enum.each(normalized, fn %{id: id} ->
      assert_receive {:gate, _measurements,
                      %{id: ^id, gate: :basal_ganglia,
                        source: :lifg, decision: :block}}
    end)
  end

  test "direct lexical sense IDs cannot evade the LIFG floor by claiming another source" do
    cfg =
      state(%{
        gate_threshold: 0.0,
        lifg_min_score: 0.8,
        evidence_floor: 0.0,
        boost_threshold: 0.0,
        boost_threshold_pref: 0.0,
        block_threshold: 0.0,
        source_boosts: %{},
        prefer_sources: []
      })

    Enum.each([:runtime, :intent, :ltm, :curiosity], fn source ->
      id = "underfloor-#{source}|verb|1"
      candidate = %{id: id, lemma: "underfloor", score: 0.2, source: source}
      assert {[], 0, 0} = Admission.run(cfg, [candidate], [])

      assert_receive {:gate, _meas, %{id: ^id, source: ^source,
                                      gate: :basal_ganglia, decision: :block}}
    end)
  end

  test "a generic floor cannot be bypassed by a LIFG-specific lower floor" do
    cfg =
      state(%{
        gate_threshold: 0.0,
        lifg_min_score: 0.1,
        evidence_floor: 0.8,
        boost_threshold: 0.0,
        boost_threshold_pref: 0.0,
        prefer_sources: []
      })

    assert {[], 0, 0} =
      Admission.run(cfg, [%{id: "underfloor|verb|1",
                            source: :lifg, score: 0.2}], [])

    assert_receive {:gate, _meas,
                    %{id: "underfloor|verb|1", decision: :block,
                      gate: :basal_ganglia}}
  end

  test "unscoped privileged source labels are rejected even with zero thresholds" do
    cfg =
      state(%{
        gate_threshold: 0.0,
        lifg_min_score: 0.0,
        evidence_floor: 0.0,
        boost_threshold: 0.0,
        boost_threshold_pref: 0.0,
        block_threshold: 0.0,
        prefer_sources: []
      })

    candidates = [
      %{id: "comprehension:" <> String.duplicate("a", 32),
        source: :comprehension, score: 1.0},
      %{id: "verified_semantics:" <> String.duplicate("b", 32),
        source: :verified_semantics, score: 1.0},
      %{id: "verified_semantics:" <> String.duplicate("c", 32),
        source: "verified_semantics", score: 1.0,
        kind: :verified_semantic_frame,
        context: %{schema: 1, verification: :source_supported,
                   factual_authority?: true}}
    ]

    Enum.each(candidates, fn candidate ->
      assert {[], 0, 0} = Admission.run(cfg, [candidate], [])

      assert_receive {:gate, %{score: 0.0},
                      %{id: id, decision: :block, gate: :basal_ganglia,
                        reason: :invalid_scoped_source_evidence}}
      assert id == candidate.id
    end)
  end

  test "scoped candidate fingerprints cannot be stale or assigned an unrelated frame ID" do
    cfg =
      state(%{
        gate_threshold: 0.0,
        lifg_min_score: 0.0,
        evidence_floor: 0.0,
        boost_threshold: 0.0,
        boost_threshold_pref: 0.0,
        prefer_sources: []
      })

    stale = System.system_time(:millisecond) - 120_000
    context = %{schema: 1, verification: :conversation_context_only,
                updated_at_ms: stale,
                attention: %{scope: :session,
                             verification: :conversation_context_only}}

    for {id, context} <- [
      {"comprehension:" <> String.duplicate("a", 32), context},
      {"comprehension:unverified", %{context | updated_at_ms: System.system_time(:millisecond)}}
    ] do
      assert {[], 0, 0} = Admission.run(cfg, [
        %{id: id, source: :comprehension, kind: :comprehension_frame,
          context: context, score: 1.0}
      ], [])
      assert_receive {:gate, _meas, %{id: ^id, decision: :block,
                                      reason: :invalid_scoped_source_evidence}}
    end
  end

  test "normalization keeps stage2 evidence but final decision comes from BG" do
    decisions = [
      {:commit,
       %{
         id: "hello|interjection|2",
         token_index: 1,
         lemma: "hello",
         score: 0.8,
         margin: 0.5,
         decision: :boost,
         source: :lifg,
         payload: %{stage: :lifg_stage2}
       }}
    ]

    {wm, 1, 0} = Admission.run(state(%{prefer_sources: [:lifg]}), decisions, [])

    assert [%{id: "hello|interjection|2", source: :lifg, payload: payload}] = wm
    assert payload.margin == 0.5
    assert payload.stage2_action == :commit

    assert_receive {:gate, _meas, %{decision: decision, gate: :basal_ganglia, source: :lifg}}

    assert decision in [:allow, :boost]
  end
end
