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
