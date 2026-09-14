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
