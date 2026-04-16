defmodule Brain.IntrospectionTest do
  use ExUnit.Case, async: false

  setup do
    ensure_started(Brain)
    ensure_started(Brain.MoodCore)
    ensure_started(Brain.Meta)
    ensure_started(Brain.SelfPortrait)

    :ok
  end

  defp ensure_started(mod) do
    case Process.whereis(mod) do
      nil -> start_supervised!({mod, []})
      _pid -> :ok
    end
  end

  test "snapshot returns a canonical self model from live Brain processes" do
    model = Brain.Introspection.snapshot()

    assert %Brain.SelfModel{} = model
    assert model.v == 1
    assert is_integer(model.updated_at_ms)

    assert model.confidence >= 0.0 and model.confidence <= 1.0
    assert model.uncertainty >= 0.0 and model.uncertainty <= 1.0
    assert model.stability >= 0.0 and model.stability <= 1.0
    assert model.vigilance >= 0.0 and model.vigilance <= 1.0
    assert model.plasticity >= 0.0 and model.plasticity <= 1.0
    assert model.inhibition >= 0.0 and model.inhibition <= 1.0
    assert model.cognitive_load >= 0.0 and model.cognitive_load <= 1.0
  end

  test "update_from_resolved emits self-model telemetry with version metadata" do
    id = "self-model-update-test-#{System.unique_integer([:positive])}"
    parent = self()

    :ok =
      :telemetry.attach(
        id,
        [:brain, :self_model, :update],
        fn _ev, meas, meta, pid ->
          send(pid, {:self_model_update, meas, meta})
        end,
        parent
      )

    on_exit(fn -> :telemetry.detach(id) end)

    resolved = %{lifg_choices: [%{id: "hello|interjection|0"}]}
    appraisal = %{evidence: %{target: :assistant}}

    assert {:ok, %Brain.SelfModel{}} =
             Brain.Introspection.update_from_resolved(resolved, appraisal)

    assert_receive {:self_model_update, %{count: 1, confidence: confidence}, meta}, 500
    assert is_number(confidence)
    assert meta[:v] == 1
    assert meta[:target] == :assistant
    assert meta[:lifg_choices_count] == 1
  end
end
