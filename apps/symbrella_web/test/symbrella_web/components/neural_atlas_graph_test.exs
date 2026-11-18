defmodule SymbrellaWeb.Components.Brain.NeuralAtlasGraphTest do
  use SymbrellaWeb.ConnCase, async: true
  import Phoenix.LiveViewTest

  alias SymbrellaWeb.Components.Brain.NeuralAtlasGraph

  @nodes [
    %{key: :lifg, label: "LIFG", subtitle: "Lexical competition",
      description: "Select senses", modules: [Brain.LIFG],
      related: [:atl, :pmtg], telemetry: [[:brain, :lifg, :stage1, :winner]]},
    %{key: :atl, label: "ATL", subtitle: "Integration",
      description: "Integrate", modules: [Brain.ATL],
      related: [:lifg], telemetry: []},
    %{key: :pmtg, label: "pMTG", subtitle: "Retrieval",
      description: "Retrieve", modules: [Brain.PMTG],
      related: [:lifg], telemetry: []}
  ]

  test "stable named region coordinates are independent of registry ordering" do
    first = NeuralAtlasGraph.layout(@nodes)
    reversed = NeuralAtlasGraph.layout(Enum.reverse(@nodes))

    assert first == reversed
    assert Enum.find(first, &(&1.key == :lifg)).x == 390
    assert Enum.all?(first, &(is_binary(&1.left) and is_binary(&1.top)))
  end

  test "conceptual links use only documented regions and dedupe reciprocals" do
    edges = NeuralAtlasGraph.edges(@nodes, :conceptual, [])

    assert length(edges) == 2
    assert Enum.all?(edges, &(&1.directed == false))
    assert Enum.all?(edges, &(&1.from in [:lifg, :atl, :pmtg]))
    assert Enum.all?(edges, &(&1.to in [:lifg, :atl, :pmtg]))
  end

  test "BEAM references are directed and retain unique pairs, never fabricated runtime links" do
    dependencies = [
      %{from: :lifg, to: :atl, call: "resolve/1"},
      %{from: :lifg, to: :atl, call: "merge/1"},
      %{from: :lifg, to: :unregistered, call: "unknown/1"}
    ]

    assert NeuralAtlasGraph.edges(@nodes, :dependencies, dependencies) ==
             [%{from: :lifg, to: :atl, directed: true}]
    assert NeuralAtlasGraph.edges(@nodes, :telemetry, dependencies) == []
    assert NeuralAtlasGraph.edges(@nodes, :bus, dependencies) == []
  end

  test "graph renders keyboard-focusable, selectable HTML buttons above SVG wires" do
    html = render_component(&NeuralAtlasGraph.graph/1, nodes: @nodes, selected: :lifg)

    assert html =~ ~s(id="atlas-network-graph")
    assert html =~ ~s(id="atlas-region-lifg")
    assert html =~ ~s(aria-pressed="true")
    assert html =~ ~s(phx-click="select-region")
    assert html =~ ~s(aria-label="Inspect LIFG cognitive region")
    assert html =~ ~s(<svg)
    assert html =~ "documented links"
    assert html =~ "positions are illustrative"
  end

  test "telemetry highlights only observed nodes without claiming a connection" do
    event = %{regions: [:atl], topic: "[:brain, :atl]", seen_at: "10:00:00 UTC"}

    html = render_component(&NeuralAtlasGraph.graph/1,
      nodes: @nodes, selected: :lifg, layer: :telemetry, events: [event])

    assert html =~ ~s(data-layer="telemetry")
    assert html =~ "1 seen"
    assert html =~ "event-region observations"
    refute html =~ "documented links"
    refute html =~ "compiled references"
  end
end
