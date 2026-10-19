defmodule Brain.LIFG.GroundTruthGateTest do
  use ExUnit.Case, async: true
  import Support.TelemetryHelpers

  alias Brain.LIFG.Stage1

  test "Stage1 guard chargram tripwire includes count and version metadata" do
    # Dedicated path prevents concurrent telemetry from another Stage1 run
    # from satisfying this test with the wrong candidate's metadata.
    event = [:test, :lifg, :chargram_violation, :groundtruth_test]

    si = %{
      sentence: "what",
      tokens: [
        %{index: 0, phrase: "ha", span: {1, 3}, n: 1, source: :chargram}
      ],
      sense_candidates: %{}
    }

    {measurements, metadata} =
      capture(event, fn ->
        assert {:ok, %{audit: audit}} = Stage1.run(si, chargram_event: event)
        assert audit.dropped_tokens >= 1
      end)

    assert measurements == %{}
    assert metadata.count == 1
    assert metadata.reason == :chargram
    assert metadata.v == 2
  end

  test "Stage1 guard boundary drop includes count and version metadata" do
    event = [:test, :lifg, :boundary_drop, :groundtruth_test]

    si = %{
      sentence: "what im",
      tokens: [
        %{index: 0, phrase: "hat", span: {1, 4}, n: 1},
        %{index: 1, phrase: "what", span: {0, 4}, n: 1}
      ],
      sense_candidates: %{
        1 => [%{id: "what|pron|1", features: %{pos: "pronoun"}, lemma: "what"}]
      }
    }

    {measurements, metadata} =
      capture(event, fn ->
        assert {:ok, %{choices: choices}} = Stage1.run(si, boundary_event: event)
        assert [%{token_index: 1}] = choices
      end)

    assert measurements == %{}
    assert metadata.count == 1
    assert metadata.reason == :boundary
    assert metadata.v == 2
  end
end
