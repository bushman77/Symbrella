defmodule Core.Brain.IntrospectionTest do
  use ExUnit.Case, async: false

  alias Core.Brain.Introspection

  setup do
    ensure_started(Brain)
    ensure_started(Brain.MoodCore)
    ensure_started(Brain.Meta)
    ensure_started(Brain.SelfPortrait)
    ensure_started(Brain.Hippocampus)
    :ok = Brain.SelfPortrait.reset()
    :ok = Brain.Hippocampus.reset()

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
    assert %{status: status, warnings: warnings, recovery_suggestions: suggestions} = out.self_monitor
    assert status in [:ok, :warning]
    assert is_list(warnings)
    assert is_list(suggestions)
    assert %{status: :persisted, snapshot_id: snapshot_id, snapshot_scope: ^scope} = out.self_continuity
    assert is_integer(snapshot_id)
    assert out.self_model.continuity.last_snapshot_id == snapshot_id

    assert %{
             stage: :self_model,
             decision: :updated_and_persisted,
             scores: scores,
             meta: meta
           } = Enum.find(out.trace, &(&1.stage == :self_model))

    assert %{stage: :meta_monitor, decision: ^status, meta: monitor_meta} =
             Enum.find(out.trace, &(&1.stage == :meta_monitor))

    assert is_number(scores.confidence)
    assert is_number(scores.uncertainty)
    assert meta.continuity == :persisted
    assert is_list(monitor_meta.warning_kinds)
    assert is_list(monitor_meta.recovery_suggestions)

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
    assert %{stage: :self_model, decision: :updated, meta: meta} =
             Enum.find(out.trace, &(&1.stage == :self_model))

    assert meta.continuity == :skipped
  end

  test "update_self_model attaches ok meta-monitor state" do
    si = %Core.SemanticInput{sentence: "hello", trace: []}

    out =
      Introspection.update_self_model(si,
        persist_self_model: false,
        self_monitor_thresholds: %{
          uncertainty: 2.0,
          stability: -1.0,
          cognitive_load: 2.0,
          recent_errors: 999,
          contradiction_delta: 2.0,
          stuck_loop_repeats: 999
        }
      )

    assert %{status: :ok, warnings: [], recovery_suggestions: []} = out.self_monitor
    assert %{stage: :meta_monitor, decision: :ok, meta: meta} =
             Enum.find(out.trace, &(&1.stage == :meta_monitor))

    assert meta.warning_kinds == []
    assert meta.recovery_suggestions == []
    assert %{stage: :self_memory_recall, decision: :none} =
             Enum.find(out.trace, &(&1.stage == :self_memory_recall))

    assert Brain.Hippocampus.recall_self_memories(["self_monitor_warning"],
             limit: 5,
             ignore_head: :never
           ) == []

    refute Enum.any?(out.trace, &(&1.stage == :self_memory))
  end

  test "update_self_model attaches warning meta-monitor state and recovery suggestions" do
    si = %Core.SemanticInput{sentence: "unclear", trace: []}

    out =
      Introspection.update_self_model(si,
        persist_self_model: false,
        self_monitor_thresholds: %{
          uncertainty: 0.0,
          stability: -1.0,
          cognitive_load: 2.0,
          recent_errors: 999,
          contradiction_delta: 2.0,
          stuck_loop_repeats: 999
        }
      )

    assert %{status: :warning, warnings: warnings, recovery_suggestions: suggestions} =
             out.self_monitor

    assert Enum.any?(warnings, &(&1.kind == :high_uncertainty))
    assert :ask_clarifying_question in suggestions

    assert %{stage: :meta_monitor, decision: :warning, meta: meta} =
             Enum.find(out.trace, &(&1.stage == :meta_monitor))

    assert :high_uncertainty in meta.warning_kinds
    assert :ask_clarifying_question in meta.recovery_suggestions
  end

  test "update_self_model writes warning states as autobiographical self-memory" do
    si = %Core.SemanticInput{sentence: "unclear", session_id: "self-memory-test", trace: []}

    out =
      Introspection.update_self_model(si,
        persist_self_model: false,
        self_monitor_thresholds: %{
          uncertainty: 0.0,
          stability: -1.0,
          cognitive_load: 2.0,
          recent_errors: 999,
          contradiction_delta: 2.0,
          stuck_loop_repeats: 999
        }
      )

    assert %{stage: :self_memory, decision: :written, meta: trace_meta} =
             Enum.find(out.trace, &(&1.stage == :self_memory))

    assert trace_meta.kind == :self_monitor_warning
    assert :high_uncertainty in trace_meta.warning_kinds

    [%{episode: %{meta: memory_meta, slate: slate}} | _] =
      Brain.Hippocampus.recall_self_memories(["self_monitor_warning"],
        limit: 5,
        ignore_head: :never
      )

    assert memory_meta.self? == true
    assert memory_meta.autobiographical? == true
    assert memory_meta.kind == "self_monitor_warning"
    assert "self" in memory_meta.tags
    assert "self_memory" in memory_meta.tags
    assert "autobiographical" in memory_meta.tags
    assert "self_monitor_warning" in memory_meta.tags

    assert slate.payload.warning_kinds == [:high_uncertainty]
    assert :ask_clarifying_question in slate.payload.recovery_suggestions
    assert slate.payload.session_id == "self-memory-test"
  end

  test "update_self_model recalls prior self-monitor warning memories" do
    Brain.Hippocampus.write_self_memory(:self_monitor_warning, %{
      warning_kinds: [:high_cognitive_load],
      recovery_suggestions: [:reduce_scope],
      focus: :stabilize,
      uncertainty: 0.3,
      cognitive_load: 0.92
    })

    si = %Core.SemanticInput{sentence: "continue", trace: []}

    out =
      Introspection.update_self_model(si,
        persist_self_model: false,
        self_monitor_thresholds: %{
          uncertainty: 2.0,
          stability: -1.0,
          cognitive_load: 2.0,
          recent_errors: 999,
          contradiction_delta: 2.0,
          stuck_loop_repeats: 999
        }
      )

    assert %{source: :hippocampus, memories: [_ | _], recovery_suggestions: suggestions} =
             out.self_memory_recall

    assert :reduce_scope in suggestions
    assert :high_cognitive_load in out.self_memory_recall.warning_kinds

    assert %{stage: :self_memory_recall, decision: :recalled, meta: meta} =
             Enum.find(out.trace, &(&1.stage == :self_memory_recall))

    assert "self_monitor_warning" in meta.cues
    assert :reduce_scope in meta.recovery_suggestions
  end
end
