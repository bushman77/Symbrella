defmodule SymbrellaWeb.Components.Brain.AtlasEvidenceTest do
  use ExUnit.Case, async: true

  alias SymbrellaWeb.Components.Brain.AtlasEvidence

  @topic [:brain, :lifg, :stage1, :winner]
  @nodes [
    %{key: :lifg, modules: [Brain.LIFG], telemetry: [@topic]},
    %{key: :atl, modules: [Brain.ATL], telemetry: []}
  ]

  test "only a received registered telemetry topic is retained" do
    event = %{kind: :telemetry, event: @topic, measurements: %{private: "do not store"}}
    [observed] = AtlasEvidence.capture([], event, @nodes, 1_700_000_000_000)

    assert observed.regions == [:lifg]
    assert observed.topic == inspect(@topic)
    assert observed.at_ms == 1_700_000_000_000
    assert is_binary(observed.seen_at)
    refute Map.has_key?(observed, :measurements)

    assert AtlasEvidence.capture([], %{kind: :message, event: @topic}, @nodes, 0) == []
    assert AtlasEvidence.capture([], %{kind: :telemetry, event: [:unregistered]}, @nodes, 0) == []
  end

  test "observations are bounded and most recent events appear first" do
    events =
      Enum.reduce(1..55, [], fn at_ms, acc ->
        AtlasEvidence.capture(acc, %{kind: :telemetry, event: @topic}, @nodes, at_ms)
      end)

    assert length(events) == 40
    assert hd(events).at_ms == 55
    assert List.last(events).at_ms == 16
  end

  test "BEAM references do not create conceptual relationships for absent modules" do
    nodes = [
      %{key: :missing, modules: [NotARealAtlasModule]},
      %{key: :other, modules: []}
    ]

    assert AtlasEvidence.dependencies(nodes) == []
  end

  test "BEAM references only link distinct registered regions" do
    dependencies = AtlasEvidence.dependencies(@nodes)

    assert Enum.all?(dependencies, fn edge ->
             edge.from in [:lifg, :atl] and
               edge.to in [:lifg, :atl] and
               edge.from != edge.to and
               is_binary(edge.source) and
               is_binary(edge.target) and
               is_binary(edge.call)
           end)
  end
end
