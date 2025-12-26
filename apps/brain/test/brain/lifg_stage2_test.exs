defmodule Brain.LIFG.Stage2Test do
  use ExUnit.Case, async: true

  alias Brain.LIFG.Stage2

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
end
