defmodule Core.Brain.IntrospectionTest do
  use ExUnit.Case, async: false

  alias Core.Brain.Introspection

  setup do
    ensure_started(Brain)
    ensure_started(Brain.MoodCore)
    ensure_started(Brain.Meta)
    ensure_started(Brain.SelfPortrait)
    :ok = Brain.SelfPortrait.reset()

    :ok
  end

  defp ensure_started(mod) do
    case Process.whereis(mod) do
      nil -> start_supervised!({mod, []})
      _pid -> :ok
    end
  end

  test "update_self_model attaches appraisal, mood indices, and canonical self model" do
    scope = "core-introspection-#{System.unique_integer([:positive])}"

    si = %Core.SemanticInput{
      sentence: "hello symbrella",
      intent: :greeting,
      confidence: 0.9,
      lifg_choices: [%{id: "hello|interjection|0", score: 0.9}],
      trace: []
    }

    out = Introspection.update_self_model(si, self_snapshot_scope: scope)

    assert %{} = out.appraisal
    assert %{} = out.mood
    assert %Brain.SelfModel{} = out.self_model
    assert out.self_model.last_appraisal == out.appraisal
    assert out.self_model.last_lifg.choices_count == 1
    assert %{status: :persisted, snapshot_id: snapshot_id, snapshot_scope: ^scope} = out.self_continuity
    assert is_integer(snapshot_id)
    assert out.self_model.continuity.last_snapshot_id == snapshot_id

    assert [
             %{
               stage: :self_model,
               decision: :updated_and_persisted,
               scores: scores,
               meta: meta
             }
             | _
           ] = out.trace

    assert is_number(scores.confidence)
    assert is_number(scores.uncertainty)
    assert meta.continuity == :persisted

    assert {:ok, row} = Db.SelfSnapshots.latest_snapshot(scope: scope)
    assert row.id == snapshot_id
    assert row.source == "core_turn"
    assert row.snapshot["last_appraisal"] || row.snapshot[:last_appraisal]
  end

  test "update_self_model can be disabled" do
    si = %Core.SemanticInput{sentence: "hello", trace: []}

    assert Introspection.update_self_model(si, self_model: :off) == si
  end

  test "update_self_model can skip continuity persistence" do
    si = %Core.SemanticInput{sentence: "hello", trace: []}

    out = Introspection.update_self_model(si, persist_self_model: false)

    assert %Brain.SelfModel{} = out.self_model
    assert %{status: :skipped} = out.self_continuity
    assert [%{stage: :self_model, decision: :updated, meta: meta} | _] = out.trace
    assert meta.continuity == :skipped
  end
end
