defmodule Brain.LIFG.TripwireTest do
  use ExUnit.Case, async: true
  alias Brain.LIFG.Stage1

  setup do
    :telemetry.attach(
      "lifg-tripwire-test",
      [:brain, :lifg, :chargram_violation],
      fn event, meas, meta, _config ->
        send(self(), {:telemetry, event, meas, meta})
      end,
      nil
    )

    :telemetry.attach(
      "lifg-boundary-test",
      [:brain, :lifg, :boundary_drop],
      fn event, meas, meta, _config ->
        send(self(), {:telemetry, event, meas, meta})
      end,
      nil
    )

    on_exit(fn ->
      :telemetry.detach("lifg-tripwire-test")
      :telemetry.detach("lifg-boundary-test")
    end)

    :ok
  end

  test "emits telemetry and drops char-gram tokens by default" do
    si = %{
      sentence: "check the bank",
      tokens: [
        %{index: 0, phrase: "ck t", source: :chargram, span: {1, 4}},
        %{index: 1, phrase: "hello", span: {0, 5}},
        %{index: 2, phrase: "new york", mw: true, span: {6, 8}}
      ],
      sense_candidates: %{}
    }

    {:ok, %{audit: audit}} = Stage1.run(si)

    assert audit.token_count == 3
    assert audit.dropped_tokens == 1
    assert audit.kept_tokens == 2

    # Telemetry proves the drop reason
    assert_receive {:telemetry, [:brain, :lifg, :chargram_violation],
                    %{token_index: 0, phrase: "ck t"}, _}, 100
  end

  test "MWEs are kept even if they cross a boundary" do
    si = %{
      sentence: "kick-offmeeting",
      tokens: [%{index: 0, phrase: "kick-off", span: {0, 8}, mw: true}],
      sense_candidates: %{}
    }

    {:ok, %{audit: audit}} = Stage1.run(si)
    assert audit.kept_tokens == 1
    refute_receive {:telemetry, [:brain, :lifg, :boundary_drop], _, _}, 50
  end
end

