defmodule Brain.LIFG.Stage2Test do
  use ExUnit.Case, async: false

  alias Brain.LIFG.Stage2

  def handle_event(event, measurements, metadata, pid) when is_pid(pid) do
    send(pid, {:telemetry, event, measurements, metadata})
    :ok
  end

  test "run/2 skips with :not_enabled when a Stage1 event is present" do
    si = %{
      sentence: "dummy",
      trace: [
        %{stage: :lifg_stage1, choices: [%{token_index: 0}]},
        %{stage: :other, foo: :bar}
      ]
    }

    assert {:skip, %{si: si_after, reason: :not_enabled}} = Stage2.run(si)
    assert si_after == si
  end

  test "run/2 skips with :no_stage1_event when trace has no Stage1 event" do
    si = %{
      sentence: "dummy",
      trace: [
        %{stage: :other},
        %{stage: :pipeline}
      ]
    }

    assert {:skip, %{si: si_after, reason: :no_stage1_event}} = Stage2.run(si)
    assert si_after == si
  end

  test "run/2 skips with :no_stage1_event when trace is missing" do
    si = %{sentence: "hello world"}

    assert {:skip, %{si: si_after, reason: :no_stage1_event}} = Stage2.run(si)
    assert si_after == si
  end

  test "enabled Stage2 emits evidence telemetry but not canonical gate telemetry" do
    parent = self()
    evidence_id = "stage2-evidence-#{System.unique_integer([:positive])}"
    gate_id = "stage2-gate-#{System.unique_integer([:positive])}"

    :ok =
      :telemetry.attach(
        evidence_id,
        [:brain, :lifg, :stage2, :evidence],
        &__MODULE__.handle_event/4,
        parent
      )

    :ok =
      :telemetry.attach(
        gate_id,
        [:brain, :gate, :decision],
        &__MODULE__.handle_event/4,
        parent
      )

    on_exit(fn ->
      :telemetry.detach(evidence_id)
      :telemetry.detach(gate_id)
    end)

    si = %{
      sentence: "hello",
      tokens: [%{index: 0, phrase: "hello"}],
      trace: [
        %{
          stage: :lifg_stage1,
          choices: [
            %{token_index: 0, chosen_id: "hello|interjection|2", score: 0.9, margin: 0.2}
          ]
        }
      ]
    }

    assert {:ok, %{event: %{decisions: [{:commit, commit}]}}} =
             Stage2.run(si, lifg_stage2_enabled: true, lifg_min_score: 0.6)

    assert commit.id == "hello|interjection|2"

    assert_receive {:telemetry, [:brain, :lifg, :stage2, :evidence], %{score: score}, meta}
    assert score >= 0.6
    assert meta.final_admission? == false
    assert meta.gate == :lifg_stage2_evidence

    refute_receive {:telemetry, [:brain, :gate, :decision], _meas, _meta}, 50
  end
end
