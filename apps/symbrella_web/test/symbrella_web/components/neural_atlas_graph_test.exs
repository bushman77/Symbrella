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

  test "registered module satellites are computed from region metadata only" do
    satellites = NeuralAtlasGraph.satellites(@nodes)

    assert length(satellites) == 3
    assert Enum.sort(Enum.map(satellites, & &1.module)) ==
             ["Brain.ATL", "Brain.LIFG", "Brain.PMTG"]
    assert Enum.all?(satellites, &(&1.region in [:lifg, :atl, :pmtg]))
    assert Enum.all?(satellites, &(&1.x != &1.hub_x or &1.y != &1.hub_y))

    no_modules = [%{key: :atl, label: "ATL", subtitle: "", modules: [], related: []}]
    assert NeuralAtlasGraph.satellites(no_modules) == []
  end

  test "visual module spokes come from registry membership, not invented calls" do
    html = render_component(&NeuralAtlasGraph.graph/1, nodes: @nodes, selected: :lifg)

    assert html =~ ~s(data-role="registered-module")
    assert html =~ ~s(data-module="Brain.LIFG")
    assert html =~ ~s(data-region="lifg")
    assert html =~ "spokes = membership"
    refute html =~ ~s(data-edge-kind="beam")
  end

  test "telemetry keeps a labeled conceptual scaffold without fabricating runtime edges" do
    html = render_component(&NeuralAtlasGraph.graph/1,
      nodes: @nodes, selected: :lifg, layer: :telemetry)

    assert html =~ ~s(data-edge-kind="conceptual-context")
    assert html =~ "Faint dotted links = conceptual reference only"
    refute html =~ ~s(data-edge-kind="beam")
    refute html =~ ~s(data-edge-kind="conceptual")
  end

  test "BEAM links render as directed evidence over separate conceptual context" do
    dependencies = [%{from: :lifg, to: :atl, call: "resolve/1"}]

    html = render_component(&NeuralAtlasGraph.graph/1,
      nodes: @nodes, selected: :lifg, layer: :dependencies, dependencies: dependencies)

    assert html =~ ~s(data-edge-kind="beam")
    assert html =~ ~s(data-edge-kind="conceptual-context")
    assert html =~ "compiled references"
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
    assert html =~ "Positions are illustrative"
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

  test "source topology distinguishes documented grouping, configured planners and direct calls" do
    nodes = [
      %{key: :lifg, label: "LIFG", subtitle: "Sense selection",
        related: [:cerebellum], modules: [Brain.LIFG]},
      %{key: :cerebellum, label: "Cerebellum", subtitle: "Calibration",
        related: [:lifg], modules: [Brain.Cerebellum]},
      %{key: :prefrontal, label: "Prefrontal", subtitle: "Planning",
        related: [:lifg], modules: [Brain.Prefrontal]}
    ]

    source_edges = [
      %{from: :lifg, to: :cerebellum, kind: :direct_call,
        source: "Brain.LIFG.Stage1.cereb_calibrate/4"},
      %{from: :prefrontal, to: :lifg, kind: :grouping,
        source: "Brain.Prefrontal.status/0 children"},
      %{from: :missing, to: :lifg, kind: :planner_reference, source: "missing"}
    ]

    result = NeuralAtlasGraph.edges(nodes, :source, source_edges)

    assert length(result) == 2
    assert Enum.any?(result, &(&1.kind == :direct_call and &1.directed))
    assert Enum.any?(result, &(&1.kind == :grouping and not &1.directed))
    refute Enum.any?(result, &(&1.from == :missing))

    html = render_component(&NeuralAtlasGraph.graph/1,
      nodes: nodes, selected: :lifg, layer: :source, sources: source_edges)

    assert html =~ ~s(data-layer="source")
    assert html =~ ~s(data-edge-kind="source-direct_call")
    assert html =~ ~s(data-edge-kind="source-grouping")
    assert html =~ "source-backed relationships"
    assert html =~ "Faint dotted links = conceptual reference only"
    refute html =~ ~s(data-edge-kind="beam")
  end
end
