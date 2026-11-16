defmodule Core.Agency.OutcomeEvaluatorTest do
  use ExUnit.Case, async: true

  alias Core.Agency.Command
  alias Core.Agency.Decision
  alias Core.Agency.OutcomeEvaluator

  defp decision do
    %Decision{
      trace_id: "agency|test|self-check",
      selected_action: :self_check,
      expected_outcome: %{kind: :self_state_reviewed},
      reasons: [:endogenous_salience]
    }
  end

  defp command do
    %Command{
      id: "agency|command|self-check",
      type: :run_self_check,
      decision_trace_id: "agency|test|self-check"
    }
  end

  defp executed do
    %{command_id: "agency|command|self-check", type: :run_self_check, status: :executed}
  end

  test "verified self-check needs an executed matching command and a fresh live read" do
    model = %Brain.SelfModel{
      confidence: 0.72,
      uncertainty: 0.28,
      cognitive_load: 0.15,
      updated_at_ms: System.system_time(:millisecond)
    }

    evaluation =
      OutcomeEvaluator.evaluate(decision(), [command()], [executed()],
        snapshot_reader: fn -> model end
      )

    assert evaluation.v == 1
    assert evaluation.status == :verified
    assert evaluation.verified?
    assert evaluation.reason == :live_self_state_observed
    assert evaluation.observation.source == :brain_introspection
    assert evaluation.observation.self_model_v == 1
    assert evaluation.observation.confidence == 0.72

    reflection = OutcomeEvaluator.reflection(decision(), evaluation)
    assert reflection.why_i_did_it == [:endogenous_salience]
    assert reflection.what_worked == [:live_self_state_observed]
    assert reflection.what_failed == []
    assert reflection.verified?
    assert reflection.next_time_adjustment == :maintain_current_strategy
  end

  test "a command execution receipt alone never proves the intended outcome" do
    evaluation =
      OutcomeEvaluator.evaluate(decision(), [command()], [
        Map.put(executed(), :outcome, %{self_state: %{confidence: 1.0}, verified?: true})
      ],
        snapshot_reader: fn -> raise "Brain unavailable" end
      )

    assert evaluation.status == :unverified
    refute evaluation.verified?
    assert evaluation.reason == :snapshot_unavailable
    assert evaluation.next_time_adjustment == :seek_independent_evidence

    reflection = OutcomeEvaluator.reflection(decision(), evaluation)
    assert reflection.what_worked == []
    assert reflection.what_failed == []
    refute reflection.verified?
  end

  test "a different command or decision trace cannot supply the evidence" do
    mismatched = %{command() | decision_trace_id: "agency|other|trace"}

    evaluation =
      OutcomeEvaluator.evaluate(decision(), [mismatched], [executed()],
        snapshot_reader: fn -> raise "should not be called" end
      )

    assert evaluation.status == :unverified
    assert evaluation.reason == :missing_matching_command

    evaluation =
      OutcomeEvaluator.evaluate(decision(), [command()], [
        %{executed() | command_id: "unrelated-command"}
      ],
        snapshot_reader: fn -> raise "should not be called" end
      )

    assert evaluation.status == :unverified
    assert evaluation.reason == :missing_execution_receipt
  end

  test "explicit command failure is recorded, not classified as success" do
    failed = executed() |> Map.put(:status, :failed) |> Map.put(:reason, :runtime_error)

    evaluation =
      OutcomeEvaluator.evaluate(decision(), [command()], [failed],
        snapshot_reader: fn -> raise "should not be called" end
      )

    assert evaluation.status == :failed
    assert evaluation.reason == :command_failed

    reflection = OutcomeEvaluator.reflection(decision(), evaluation)
    assert reflection.what_worked == []
    assert reflection.what_failed == [:command_failed]
    assert reflection.next_time_adjustment == :diagnose_action_failure
  end

  test "deferred execution remains unverified" do
    deferred = %{executed() | status: :deferred}

    evaluation =
      OutcomeEvaluator.evaluate(decision(), [command()], [deferred],
        snapshot_reader: fn -> raise "should not be called" end
      )

    assert evaluation.status == :unverified
    assert evaluation.reason == :command_deferred
  end

  test "an invalid self-model is not accepted as independent evidence" do
    invalid = %Brain.SelfModel{confidence: 2.0, updated_at_ms: 0}

    evaluation =
      OutcomeEvaluator.evaluate(decision(), [command()], [executed()],
        snapshot_reader: fn -> invalid end
      )

    assert evaluation.status == :unverified
    assert evaluation.reason == :invalid_live_snapshot
  end

  test "stale or future-dated self-model observations cannot verify an outcome" do
    now_ms = System.system_time(:millisecond)

    for timestamp <- [now_ms - 60_000, now_ms + 60_000] do
      snapshot = %Brain.SelfModel{updated_at_ms: timestamp}

      evaluation =
        OutcomeEvaluator.evaluate(decision(), [command()], [executed()],
          snapshot_reader: fn -> snapshot end
        )

      assert evaluation.status == :unverified
      assert evaluation.reason == :invalid_live_snapshot
      refute evaluation.verified?
    end
  end

  test "unsupported outcomes cannot be marked verified by an unrelated snapshot" do
    unsupported = %{decision() | expected_outcome: %{kind: :user_clarifies}}

    evaluation =
      OutcomeEvaluator.evaluate(unsupported, [command()], [executed()],
        snapshot_reader: fn -> raise "should not be called" end
      )

    assert evaluation.status == :unverified
    assert evaluation.reason == :unsupported_outcome
    refute evaluation.verified?
  end
end
