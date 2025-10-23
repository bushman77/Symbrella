defmodule Brain.HippocampusEpisodicPrimingTest do
  use ExUnit.Case, async: false

  @moduletag :hippo_priming

  setup do
    # capture [:brain, :hippo, :priming] events into the test process
    handler_id = "test-hippo-priming-#{System.unique_integer([:positive])}"
    parent = self()

    :ok =
      :telemetry.attach(handler_id, [:brain, :hippo, :priming],
        fn _event, meas, meta, _cfg ->
          send(parent, {:hippo_priming, meas, meta})
        end,
        %{}
      )

    on_exit(fn -> :telemetry.detach(handler_id) rescue _ -> :ok end)
    :ok
  end

  test "success episodes emit priming event" do
    si = %{
      source: :test,
      atl_slate: %{winners: [%{lemma: "ok"}]},
      intent: %{confidence: 0.9}
    }

    # persist: false so we don't require Db; priming: true to enable
    _ = Brain.Hippocampus.Writer.maybe_persist(si, persist: false, priming: true)

    assert_receive {:hippo_priming, meas, meta}, 200
    assert is_map(meas)
    assert meta[:outcome] == :success
  end

  test "failure heuristic emits priming event" do
    si = %{
      source: :test,
      reanalysis: %{gave_up: true}
    }

    _ = Brain.Hippocampus.Writer.maybe_persist(si, persist: false, priming: true)

    assert_receive {:hippo_priming, _meas, meta}, 200
    assert meta[:outcome] == :failure
  end

  test "neutral episodes do not emit priming event" do
    si = %{source: :test}

    _ = Brain.Hippocampus.Writer.maybe_persist(si, persist: false, priming: true)

    refute_receive {:hippo_priming, _meas, _meta}, 200
  end
end

