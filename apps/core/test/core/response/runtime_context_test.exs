defmodule Core.Response.RuntimeContextTest do
  use ExUnit.Case, async: true

  alias Core.Response.RuntimeContext

  test "lifg_runtime_from_snapshot normalizes atom and string keyed snapshots" do
    runtime =
      RuntimeContext.lifg_runtime_from_snapshot(%{
        "running?" => true,
        "state" => %{
          "last" => %{
            "intent" => :question,
            "confidence" => 0.72,
            "choices" => [%{}, %{}],
            "audit" => %{
              "weak_decisions" => 1,
              "fallback_winners" => 2,
              "boundary_drops" => 3
            },
            "guards" => %{
              "missing_candidates" => 4,
              "chargram_violation" => 1
            },
            "meta" => %{
              "acc_conflict" => 0.6
            }
          }
        }
      })

    assert runtime.focused?
    assert runtime.running?
    assert runtime.intent == :question
    assert runtime.confidence == 0.72
    assert runtime.choices_count == 2
    assert runtime.missing_candidates == 4
    assert runtime.weak_decisions == 1
    assert runtime.fallback_winners == 2
    assert runtime.chargram_violations == 1
    assert runtime.boundary_drops == 3
    assert runtime.acc_conflict == 0.6
    assert runtime.degraded?
  end

  test "self_model_log keeps only prompt-safe summary fields" do
    assert RuntimeContext.self_model_log(%{
             v: 2,
             confidence: 0.8,
             uncertainty: 0.1,
             stability: 0.7,
             cognitive_load: 0.4,
             private: "ignored"
           }) == %{
             v: 2,
             confidence: 0.8,
             uncertainty: 0.1,
             stability: 0.7,
             cognitive_load: 0.4
           }
  end

  test "snapshot returns prompt context data as plain maps and lists" do
    snapshot = RuntimeContext.snapshot()

    assert is_list(snapshot.wm_items)
    assert is_map(snapshot.runtime_state)
    assert snapshot.runtime_state.source == :brain
    assert snapshot.runtime_state.phase == :prompt_context
    assert snapshot.runtime_state.status == :ready
    assert is_map(snapshot.runtime_state.wm)
    assert is_map(snapshot.runtime_state.lifg)
    assert is_map(snapshot.runtime_state.self_portrait)
    assert is_binary(snapshot.runtime_state.self_state_summary)
  end
end
