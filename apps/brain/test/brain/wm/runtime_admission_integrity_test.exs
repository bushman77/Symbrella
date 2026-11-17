defmodule Brain.WM.RuntimeAdmissionIntegrityTest do
  use ExUnit.Case, async: false

  @moduledoc """
  P-9A-03: exercise real Brain ingress, not just WM.Admission.run/3.
  Every observed write must be preceded by a canonical BasalGanglia decision.
  Stage2 and Thalamus evidence are not final WM admission.
  """

  @gate_event [:brain, :gate, :decision]
  @stage2_event [:brain, :lifg, :stage2, :evidence]
  @wm_event [:brain, :wm, :update]

  def forward_event(event, measurements, metadata, pid) do
    send(pid, {:admission_event, event, measurements, metadata})
    :ok
  end

  setup do
    assert is_pid(Process.whereis(Brain))
    :ok = Brain.defocus(fn _ -> true end)

    listener = "p9a03-runtime-#{System.unique_integer([:positive])}"

    :ok =
      :telemetry.attach_many(
        listener,
        [@gate_event, @stage2_event, @wm_event],
        &__MODULE__.forward_event/4,
        self()
      )

    on_exit(fn ->
      :telemetry.detach(listener)
      Brain.defocus(fn _ -> true end)
    end)

    :ok
  end

  test "Brain.focus rejects weak candidates before mutating runtime WM" do
    id = "runtime-weak-#{System.unique_integer([:positive])}"

    # This call goes through the Brain GenServer, not a stand-alone pure gate.
    wm =
      Brain.focus(
        [%{id: id, lemma: "weak", score: 0.01, source: :intent}],
        gate_threshold: 0.95,
        boost_threshold: 0.95,
        boost_threshold_pref: 0.95,
        prefer_sources: []
      )

    refute Enum.any?(wm, &(&1.id == id))

    assert_receive {:admission_event, [:brain, :gate, :decision], %{score: score},
                    %{gate: :basal_ganglia, decision: :block, id: ^id, source: :intent}}

    assert score >= 0.0 and score <= 1.0

    assert_receive {:admission_event, [:brain, :wm, :update], %{added: 0}, _}
    refute Enum.any?(Brain.snapshot_wm().wm, &(&1.id == id))
  end

  test "Brain.focus records an admitting BG decision before runtime insertion" do
    id = "runtime-strong-#{System.unique_integer([:positive])}"
    wm = Brain.focus([%{id: id, lemma: "strong", score: 1.0, source: :intent}])

    assert Enum.any?(wm, &(&1.id == id))

    assert_receive {:admission_event, [:brain, :gate, :decision], %{score: score},
                    %{gate: :basal_ganglia, decision: decision, id: ^id, source: :intent}}

    assert decision in [:allow, :boost]
    assert score >= 0.0 and score <= 1.0

    assert_receive {:admission_event, [:brain, :wm, :update], %{added: added}, _}
    assert added >= 1
    assert Enum.any?(Brain.snapshot_wm().wm, &(&1.id == id))
  end

  test "direct Brain.focus cannot lower a lexical sense floor by claiming runtime origin" do
    id = "source-spoof-#{System.unique_integer([:positive])}|verb|1"

    wm =
      Brain.focus(
        [%{id: id, lemma: "source-spoof", score: 0.2, source: :runtime}],
        lifg_min_score: 0.8,
        evidence_floor: 0.0,
        gate_threshold: 0.0,
        boost_threshold: 0.0,
        boost_threshold_pref: 0.0,
        prefer_sources: [],
        source_boosts: %{}
      )

    refute Enum.any?(wm, &(&1.id == id))

    assert_receive {:admission_event, [:brain, :gate, :decision], _,
                    %{id: ^id, source: :runtime,
                      gate: :basal_ganglia, decision: :block}}
    refute Enum.any?(Brain.snapshot_wm().wm, &(&1.id == id))
  end

  test "direct Brain.focus cannot invent a verified-semantics evidence frame" do
    id = "verified_semantics:" <> String.duplicate("d", 32)

    wm =
      Brain.focus(
        [%{id: id, lemma: "invented proof", score: 1.0,
           source: :verified_semantics}],
        gate_threshold: 0.0,
        evidence_floor: 0.0,
        boost_threshold: 0.0,
        boost_threshold_pref: 0.0
      )

    refute Enum.any?(wm, &(&1.id == id))

    assert_receive {:admission_event, [:brain, :gate, :decision], %{score: 0.0},
                    %{id: ^id, source: :verified_semantics,
                      gate: :basal_ganglia, decision: :block,
                      reason: :invalid_scoped_source_evidence}}
    refute Enum.any?(Brain.snapshot_wm().wm, &(&1.id == id))
  end

  test "configuration eviction only removes items and issues no new admission" do
    original_cfg = Brain.snapshot_wm().cfg
    on_exit(fn -> Brain.configure_wm(Map.to_list(original_cfg)) end)

    assert :ok = Brain.configure_wm(capacity: 4)

    prefix = "config-retention-#{System.unique_integer([:positive])}"
    ids = ["#{prefix}-a", "#{prefix}-b"]

    wm = Brain.focus(Enum.map(ids, fn id ->
      %{id: id, lemma: "candidate", score: 1.0, source: :intent}
    end))

    assert Enum.all?(ids, fn id -> Enum.any?(wm, &(&1.id == id)) end)
    assert_receive {:admission_event, [:brain, :gate, :decision], _,
                    %{id: first, gate: :basal_ganglia, decision: decision}}
    assert first in ids
    assert decision in [:allow, :boost]
    assert_receive {:admission_event, [:brain, :gate, :decision], _,
                    %{id: second, gate: :basal_ganglia, decision: second_decision}}
    assert second in ids
    assert second != first
    assert second_decision in [:allow, :boost]
    assert_receive {:admission_event, [:brain, :wm, :update], %{added: added}, _}
    assert added >= 2

    assert :ok = Brain.configure_wm(capacity: 1)
    snapshot = Brain.snapshot_wm()
    assert snapshot.cfg.capacity == 1
    assert length(snapshot.wm) == 1
    assert hd(snapshot.wm).id in ids

    assert_receive {:admission_event, [:brain, :wm, :update],
                    %{size: 1, added: 0, removed: 1, capacity: 1},
                    %{reason: :configure_wm}}
    refute_receive {:admission_event, [:brain, :gate, :decision], _, _}, 30
  end

  test "defocus removes an admitted item without creating a second gate decision" do
    id = "removal-only-#{System.unique_integer([:positive])}"
    assert Enum.any?(Brain.focus([%{
      id: id, lemma: "retention", score: 1.0, source: :intent
    }]), &(&1.id == id))

    assert_receive {:admission_event, [:brain, :gate, :decision], _,
                    %{id: ^id, gate: :basal_ganglia, decision: decision}}
    assert decision in [:allow, :boost]
    assert_receive {:admission_event, [:brain, :wm, :update], %{added: 1}, _}

    assert :ok = Brain.defocus(id)
    refute Enum.any?(Brain.snapshot_wm().wm, &(&1.id == id))

    assert_receive {:admission_event, [:brain, :wm, :update],
                    %{added: 0, removed: removed}, %{reason: :defocus}}
    assert removed >= 1
    refute_receive {:admission_event, [:brain, :gate, :decision], _, _}, 30
  end

  test "Stage1 with gating disabled makes no WM insertion, enabled path emits BG evidence" do
    id = "Hello there|phrase|greeting"
    si = %{
      sentence: "Hello there",
      tokens: [
        %{index: 0, phrase: "Hello there", span: {0, 11},
          kind: :word, pos: "phrase", n: 2, mw: true}
      ],
      sense_candidates: %{
        0 => [%{id: id, lemma: "Hello there", pos: "phrase",
                mw: true, score: 1.0}]
      }
    }

    assert {:ok, _} = Brain.lifg_stage1(si, [],
      gate_into_wm: false, scores: :all)
    refute Enum.any?(Brain.snapshot_wm().wm, &(&1.id == id))
    refute_receive {:admission_event, [:brain, :gate, :decision], _, _}, 30
    refute_receive {:admission_event, [:brain, :wm, :update], _, _}, 30

    assert {:ok, %{choices: choices}} = Brain.lifg_stage1(si, [],
      gate_into_wm: true, scores: :all, lifg_min_score: 0.6)
    assert Enum.any?(choices, &(Map.get(&1, :chosen_id) == id))

    assert_receive {:admission_event, [:brain, :gate, :decision],
                    %{score: gate_score},
                    %{id: ^id, source: :lifg, gate: :basal_ganglia,
                      decision: decision}}, 1_000
    assert gate_score >= 0.6
    assert decision in [:allow, :boost]

    assert_receive {:admission_event, [:brain, :wm, :update],
                    %{added: added}, %{reason: :gate_from_lifg}}, 1_000
    assert added >= 1
    assert Enum.any?(Brain.snapshot_wm().wm, &(&1.id == id))
  end

  test "Stage2 linguistic commitment cannot bypass a stricter BG admission gate" do
    id = "stage2-block-#{System.unique_integer([:positive])}|noun|0"

    si = %{
      sentence: "A candidate with limited evidence",
      tokens: [%{index: 0, phrase: "limited"}],
      trace: [
        %{
          stage: :lifg_stage1,
          choices: [
            %{token_index: 0, chosen_id: id, lemma: "limited",
              score: 0.70, margin: 0.20}
          ]
        }
      ]
    }

    assert :ok =
             Brain.gate_from_lifg(
               si,
               lifg_stage2_enabled: true,
               lifg_min_score: 0.50,
               gate_threshold: 1.0,
               boost_threshold: 1.0,
               boost_threshold_pref: 1.0,
               prefer_sources: [],
               source_boosts: %{},
               semantic_boost: 0.0
             )

    assert_receive {:admission_event, [:brain, :lifg, :stage2, :evidence],
                    %{score: evidence}, %{id: ^id, final_admission?: false}}, 1_000
    assert evidence >= 0.50

    assert_receive {:admission_event, [:brain, :gate, :decision], %{score: score},
                    %{id: ^id, source: :lifg, gate: :basal_ganglia,
                      decision: :block}}, 1_000

    assert score >= 0.0 and score <= 1.0

    assert_receive {:admission_event, [:brain, :wm, :update], %{added: 0},
                    %{reason: :lifg_stage2}}, 1_000
    refute Enum.any?(Brain.snapshot_wm().wm, &(&1.id == id))
  end

  test "Hippocampus recall candidates cannot bypass the Brain runtime gate" do
    assert is_pid(Process.whereis(Brain.Hippocampus))
    :ok = Brain.Hippocampus.reset()
    id = "recall-weak-#{System.unique_integer([:positive])}|ltm"

    wm =
      Brain.focus_from_recall(
        %{winners: [%{id: id, lemma: "recall-weak", score: 0.0}]},
        limit: 1
      )

    refute Enum.any?(wm, &(&1.id == id))

    assert_receive {:admission_event, [:brain, :gate, :decision], %{score: score},
                    %{id: ^id, source: :ltm, gate: :basal_ganglia,
                      decision: :block}}, 1_000

    assert score >= 0.0 and score <= 1.0
    refute Enum.any?(Brain.snapshot_wm().wm, &(&1.id == id))
  end

  test "weak Curiosity proposal passes Thalamus and DLPFC but not the BG gate" do
    assert is_pid(Process.whereis(Brain.Curiosity))
    assert is_pid(Process.whereis(Brain.Thalamus))
    assert is_pid(Process.whereis(Brain.DLPFC))

    :ok = Brain.Thalamus.reset()
    :ok = Brain.DLPFC.reset()
    :ok = Brain.DLPFC.set_opts(act_on_thalamus: true)

    on_exit(fn -> Brain.DLPFC.reset() end)

    id = "integrity|probe|#{System.unique_integer([:positive])}"
    assert :ok = Brain.Curiosity.nudge(id: id, source: :curiosity, score: 0.0)

    assert_receive {:admission_event, [:brain, :gate, :decision], %{score: score},
                    %{id: ^id, source: :curiosity, gate: :basal_ganglia,
                      decision: :block}}, 3_000

    assert score >= 0.0 and score <= 1.0
    refute Enum.any?(Brain.snapshot_wm().wm, &(&1.id == id))
    assert Brain.DLPFC.status().last_probe_id == id
  end
end
