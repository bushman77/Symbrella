defmodule Brain.LIFGStage2ContractTest do
  use ExUnit.Case, async: true

  alias Brain.LIFG.Stage2

  @moduledoc """
  Contract tests for the LIFG.Stage2 scaffold.

  These tests only assert the **external behaviour** of Stage2.run/2, not its
  internal implementation.

  Contract (scaffold):

    * If there is **no Stage1 event** in `si.trace` (including missing/empty trace),
      `run/2` returns `{:skip, %{si: si_after, reason: :no_stage1_event}}`.
    * If there **is** a Stage1 event in `si.trace`, `run/2` returns
      `{:skip, %{si: si_after, reason: :not_enabled}}`.
    * `run/2` never raises and always returns the `si` payload (possibly
      normalized) inside the `:skip` map.
  """

  test "run/2 is safe and returns :skip with :no_stage1_event when Stage1 has not run" do
    si0 = %{sentence: "test sentence"}

    assert {:skip, %{si: si1, reason: :no_stage1_event}} = Stage2.run(si0)

    assert is_map(si1)
    assert si1[:sentence] == "test sentence"
  end

  test "run/2 accepts arbitrary options and still returns :skip with :no_stage1_event" do
    si0 = %{
      sentence: "test sentence",
      tokens: [%{index: 0, phrase: "test"}],
      trace: []
    }

    opts = [
      max_interpretations: 99,
      global_weight_lex: 0.1,
      garbage: :ok
    ]

    assert {:skip, %{si: si1, reason: :no_stage1_event}} = Stage2.run(si0, opts)

    assert is_map(si1)
    # Ensure we didn't blow away basic structure
    assert si1[:sentence] == "test sentence"
    assert si1[:tokens] == [%{index: 0, phrase: "test"}]
  end

  test "run/2 returns :not_enabled when a Stage1 event is present in the trace" do
    si0 = %{
      sentence: "test sentence",
      trace: [
        %{stage: :lifg_stage1, choices: [], boosts: [], inhibitions: []}
      ]
    }

    assert {:skip, %{si: si1, reason: :not_enabled}} = Stage2.run(si0)

    assert is_map(si1)
    assert si1[:sentence] == "test sentence"
  end
end
