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
    si = %Core.SemanticInput{
      sentence: "hello symbrella",
      intent: :greeting,
      confidence: 0.9,
      lifg_choices: [%{id: "hello|interjection|0", score: 0.9}],
      trace: []
    }

    out = Introspection.update_self_model(si, [])

    assert %{} = out.appraisal
    assert %{} = out.mood
    assert %Brain.SelfModel{} = out.self_model
    assert out.self_model.last_appraisal == out.appraisal
    assert out.self_model.last_lifg.choices_count == 1
    assert [%{stage: :self_model, decision: :updated, scores: scores} | _] = out.trace
    assert is_number(scores.confidence)
    assert is_number(scores.uncertainty)
  end

  test "update_self_model can be disabled" do
    si = %Core.SemanticInput{sentence: "hello", trace: []}

    assert Introspection.update_self_model(si, self_model: :off) == si
  end
end
