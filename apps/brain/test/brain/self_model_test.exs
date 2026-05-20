defmodule Brain.SelfModelTest do
  use ExUnit.Case, async: true

  test "derives canonical fields from runtime evidence" do
    model =
      Brain.SelfModel.from_runtime(
        self_portrait: %{
          traits: %{
            confidence_baseline: 0.7,
            stability: 0.8
          },
          patterns: %{
            boundary_drops: 2,
            chargram_violations: 0,
            lifg_payload_gaps: 1,
            lifg_pos_anomalies: 0
          },
          last_events: [
            %{event: [:brain, :lifg, :reasons], at_ms: 123}
          ]
        },
        meta: %{
          conf: 0.6
        },
        mood: %{
          mood: %{
            vigilance: 0.9,
            plasticity: 0.4,
            inhibition: 0.3
          }
        },
        wm: %{
          wm: [%{id: "a"}, %{id: "b"}],
          cfg: %{capacity: 4}
        }
      )

    assert %Brain.SelfModel{} = model
    assert model.confidence == 0.6
    assert model.uncertainty == 0.4
    assert model.stability == 0.8
    assert model.focus == :balanced
    assert model.vigilance == 0.9
    assert model.plasticity == 0.4
    assert model.inhibition == 0.3
    assert model.cognitive_load == 0.5
    assert model.recent_actions == [%{event: [:brain, :lifg, :reasons], at_ms: 123}]

    assert %{kind: :boundary_drops, count: 2} in model.recent_errors
    assert %{kind: :lifg_payload_gaps, count: 1} in model.recent_errors
    refute Enum.any?(model.recent_errors, &(&1.kind == :chargram_violations))

    assert is_integer(model.updated_at_ms)
    assert model.v == 1
  end

  test "falls back to SelfPortrait confidence when meta confidence is absent" do
    model =
      Brain.SelfModel.from_runtime(
        self_portrait: %{
          traits: %{confidence_baseline: 0.72}
        },
        meta: %{}
      )

    assert model.confidence == 0.72
    assert model.uncertainty == 0.28
  end

  test "keeps bounded defaults when runtime evidence is sparse" do
    model = Brain.SelfModel.from_runtime([])

    assert %Brain.SelfModel{} = model
    assert model.confidence == 0.5
    assert model.uncertainty == 0.5
    assert model.stability == 0.5
    assert model.focus == :balanced
    assert model.vigilance == 0.5
    assert model.plasticity == 0.5
    assert model.inhibition == 0.5
    assert model.cognitive_load == 0.0
    assert model.recent_errors == []
    assert model.recent_actions == []
  end

  test "clamps runtime numeric state to public self-model bounds" do
    model =
      Brain.SelfModel.from_runtime(
        self_portrait: %{
          traits: %{
            confidence_baseline: 3.0,
            stability: -2.0
          }
        },
        meta: %{conf: 2.0},
        mood: %{
          mood: %{
            vigilance: 9.0,
            plasticity: -1.0,
            inhibition: 1.5
          }
        },
        wm: %{wm: Enum.map(1..5, &%{id: &1}), cfg: %{capacity: 2}}
      )

    assert model.confidence == 1.0
    assert model.uncertainty == 0.0
    assert model.stability == 0.0
    assert model.vigilance == 1.0
    assert model.plasticity == 0.0
    assert model.inhibition == 1.0
    assert model.cognitive_load == 1.0
  end

  test "exports and imports versioned bounded snapshots" do
    model = %Brain.SelfModel{
      confidence: 1.4,
      uncertainty: -0.2,
      stability: 0.25,
      focus: :clarify,
      cognitive_load: 0.9,
      active_goals: [:answer],
      continuity: %{reboot_restored?: true},
      v: 1
    }

    snapshot = Brain.SelfModel.export(model)

    assert snapshot.v == 1
    assert snapshot.confidence == 1.0
    assert snapshot.uncertainty == 0.0
    assert snapshot.focus == :clarify

    assert {:ok, restored} = Brain.SelfModel.import(Map.put(snapshot, "focus", "stabilize"))
    assert %Brain.SelfModel{} = restored
    assert restored.confidence == 1.0
    assert restored.uncertainty == 0.0
    assert restored.focus == :clarify
  end

  test "rejects unsupported continuity snapshot versions" do
    assert {:error, {:unsupported_version, 2}} =
             Brain.SelfModel.import_continuity(%{v: 2, confidence: 0.5})
  end
end
