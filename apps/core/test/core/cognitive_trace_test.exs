defmodule Core.CognitiveTraceTest do
  use ExUnit.Case, async: true

  alias Core.CognitiveTrace

  @event [:symbrella, :cognitive_trace, :stage]

  setup do
    id = {__MODULE__, self()}
    assert :ok = :telemetry.attach(id, @event, fn _event, measurements, metadata, pid ->
      send(pid, {:observed, measurements, metadata})
    end, self())

    on_exit(fn -> :telemetry.detach(id) end)
    :ok
  end

  test "actual step returns unchanged result and emits correlated execution receipts" do
    result =
      CognitiveTrace.with_trace("turn-abcdef12", fn ->
        CognitiveTrace.step(%{sentence: "private"}, :phase_1_input, fn si ->
          Map.put(si, :processed, true)
        end)
      end)

    assert result == %{sentence: "private", processed: true}

    assert_receive {:observed, %{duration_us: 0}, %{trace_id: "turn-abcdef12",
                                                     stage: :turn, status: :started}}
    assert_receive {:observed, %{duration_us: step_us}, %{trace_id: "turn-abcdef12",
                                                          stage: :phase_1_input, status: :ok,
                                                          evidence: %{}}}}
    assert is_integer(step_us) and step_us >= 0
    assert_receive {:observed, %{duration_us: turn_us}, %{trace_id: "turn-abcdef12",
                                                          stage: :turn, status: :ok}}
    assert is_integer(turn_us) and turn_us >= 0
  end

  test "degraded comprehension remains evidence metadata rather than a stage exception" do
    CognitiveTrace.with_trace("turn-aabbccdd", fn ->
      CognitiveTrace.stage(:comprehension_summary, fn ->
        %{comprehension: %{degraded?: true, understood: [], uncertain: [:a, :b],
                           unresolved: [:x]}}
      end)
    end)

    assert_receive {:observed, _, %{status: :started}}
    assert_receive {:observed, _, %{stage: :comprehension_summary, status: :ok,
                                   evidence: evidence}}

    assert evidence.degraded == true
    assert evidence.understood == 0
    assert evidence.uncertain == 2
    assert evidence.unresolved == 1
    refute Map.has_key?(evidence, :sentence)
  end

  test "exceptions preserve failure and restore trace isolation" do
    assert_raise ArgumentError, "intended", fn ->
      CognitiveTrace.with_trace("turn-aacc0011", fn ->
        CognitiveTrace.stage(:lifg_selection, fn -> raise ArgumentError, "intended" end)
      end)
    end

    assert_receive {:observed, _, %{stage: :turn, status: :started}}
    assert_receive {:observed, _, %{stage: :lifg_selection, status: :error}}
    assert_receive {:observed, _, %{stage: :turn, status: :error}}

    assert :not_traced == CognitiveTrace.stage(:phase_3_lexical, fn -> :not_traced end)
    refute_receive {:observed, _, _}, 25
  end
end
