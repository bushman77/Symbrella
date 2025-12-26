defmodule Brain.AffectiveAppraisalTest do
  use ExUnit.Case, async: true

  alias Brain.AffectiveAppraisal

  test "positive praise yields positive valence and :praise tag" do
    si = %{sentence: "I love this. Thank you!"}

    a = AffectiveAppraisal.appraise(si)
    assert a.valence > 0.4
    assert :praise in a.tags
    assert a.arousal >= 0.05
    assert a.v == 1
  end

  test "insult directed at assistant is negative and targets :assistant" do
    si = %{sentence: "You are stupid."}

    a = AffectiveAppraisal.appraise(si)
    assert a.valence < -0.4
    assert :insult in a.tags
    assert a.evidence.target == :assistant
  end

  test "negation flips simple praise term" do
    si = %{sentence: "This is not good."}

    a = AffectiveAppraisal.appraise(si)
    assert a.valence < 0.0
  end

  test "urgency and punctuation increase arousal" do
    si = %{sentence: "HELP NOW!!!"}

    a = AffectiveAppraisal.appraise(si)
    assert a.arousal >= 0.25
    assert :urgency in a.tags
  end

  test "ranges are always respected" do
    a = AffectiveAppraisal.appraise(%{sentence: "neutral statement"})
    assert a.valence >= -1.0 and a.valence <= 1.0
    assert a.arousal >= 0.0 and a.arousal <= 1.0
    assert a.dominance >= -1.0 and a.dominance <= 1.0
    assert is_list(a.tags)
  end

  test "emits telemetry event with count/meta" do
    id = "affect-appraisal-test-#{System.unique_integer([:positive])}"
    parent = self()

    :ok =
      :telemetry.attach(
        id,
        [:brain, :affect, :appraisal],
        fn _ev, meas, meta, pid ->
          send(pid, {:affect_appraisal, meas, meta})
        end,
        parent
      )

    on_exit(fn -> :telemetry.detach(id) end)

    _a = AffectiveAppraisal.appraise(%{sentence: "You are awesome!"})

    assert_receive {:affect_appraisal, %{count: 1}, meta}, 500
    assert meta[:v] == 1
    assert is_number(meta[:valence])
    assert is_number(meta[:arousal])
    assert is_number(meta[:dominance])
    assert is_list(meta[:tags])
  end
end
