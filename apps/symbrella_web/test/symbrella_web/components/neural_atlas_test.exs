defmodule SymbrellaWeb.Components.Brain.NeuralAtlasTest do
  use SymbrellaWeb.ConnCase, async: true
  import Phoenix.LiveViewTest

  alias SymbrellaWeb.Components.Brain.NeuralAtlas

  test "registered region cards and inspector are selectable and touch-friendly" do
    nodes = [
      %{
        key: :lifg,
        label: "LIFG",
        subtitle: "Lexical competition",
        description: "Lexical sense selection with evidence guards.",
        modules: [Brain.LIFG],
        telemetry: [[:brain, :lifg, :stage1, :winner]],
        related: [:atl]
      },
      %{
        key: :atl,
        label: "ATL",
        subtitle: "Integration",
        description: "Contextual integration",
        modules: [Brain.ATL],
        telemetry: [],
        related: [:lifg]
      }
    ]

    html = render_component(&NeuralAtlas.panel/1, nodes: nodes, selected: :lifg)

    assert html =~ "Symbrella Neural Atlas"
    assert html =~ "atlas-region-lifg"
    assert html =~ "atlas-region-atl"
    assert html =~ "phx-click=\"select-region\""
    assert html =~ "aria-pressed=\"true\""
    assert html =~ "aria-pressed=\"false\""
    assert html =~ "Lexical sense selection with evidence guards"
    assert html =~ "Brain.LIFG"
    assert html =~ "conceptual navigation"
    assert html =~ "atlas-inspector"
  end

  test "registry metadata drives regions and labels without changing runtime" do
    nodes = NeuralAtlas.regions()

    assert Enum.any?(nodes, &(&1.key == :lifg))
    assert Enum.all?(nodes, &is_atom(&1.key))
    assert Enum.all?(nodes, &is_list(&1.modules))
  end

  test "conceptual links come from the registry and point to registered regions" do
    alias SymbrellaWeb.Region.Registry

    nodes = NeuralAtlas.regions()
    keys = MapSet.new(Enum.map(nodes, & &1.key))
    lifg = Enum.find(nodes, &(&1.key == :lifg))

    assert :atl in Registry.defn(:lifg).related
    assert lifg.related == Registry.defn(:lifg).related

    assert Enum.all?(nodes, fn node ->
             Enum.all?(node.related, &MapSet.member?(keys, &1))
           end)
  end

  test "BEAM reference layer shows static evidence separately from conceptual links" do
    nodes = [
      %{key: :lifg, label: "LIFG", subtitle: "Lexical", description: "Lexical",
        modules: [Brain.LIFG], telemetry: [], related: [:atl]},
      %{key: :atl, label: "ATL", subtitle: "Integration", description: "Integration",
        modules: [Brain.ATL], telemetry: [], related: [:lifg]}
    ]

    dependencies = [
      %{from: :lifg, to: :atl, source: "Brain.LIFG",
        target: "Brain.ATL", call: "resolve/1"}
    ]

    html = render_component(&NeuralAtlas.panel/1,
      nodes: nodes, selected: :lifg, layer: :dependencies, dependencies: dependencies)

    assert html =~ "atlas-layer-dependencies"
    assert html =~ "aria-pressed=\"true\""
    assert html =~ "atlas-outgoing-references"
    assert html =~ "Brain.ATL.resolve/1"
    assert html =~ "Inspect ATL"
    assert html =~ "They do not establish execution order"
    refute html =~ "No matching registered telemetry observed"
  end

  test "live telemetry layer shows only received event topics and receipt times" do
    nodes = [
      %{key: :lifg, label: "LIFG", subtitle: "Lexical", description: "Lexical",
        modules: [Brain.LIFG], telemetry: [[:brain, :lifg]], related: []}
    ]

    event = %{regions: [:lifg], topic: "[:brain, :lifg]",
      at_ms: 1_700_000_000_000, seen_at: "22:13:20 UTC"}

    html = render_component(&NeuralAtlas.panel/1,
      nodes: nodes, selected: :lifg, layer: :telemetry, events: [event])

    assert html =~ "atlas-layer-telemetry"
    assert html =~ "atlas-observed-telemetry"
    assert html =~ "Observed this session"
    assert html =~ "22:13:20 UTC"
    assert html =~ "[:brain, :lifg]"
    assert html =~ "A received event is an observation"
  end
  test "bus layer separates region-attributed events from unattributed messages" do
    nodes = [
      %{key: :lifg, label: "LIFG", subtitle: "Lexical", description: "Lexical",
        modules: [Brain.LIFG], telemetry: [], related: []}
    ]

    bus_events = [
      %{region: :lifg, kind: ":ml_turn", at_ms: 1, seen_at: "00:00:00 UTC"},
      %{region: nil, kind: ":pipeline", at_ms: 2, seen_at: "00:00:00 UTC"}
    ]

    html = render_component(&NeuralAtlas.panel/1,
      nodes: nodes, selected: :lifg, layer: :bus, bus_events: bus_events)

    assert html =~ "atlas-layer-bus"
    assert html =~ "atlas-observed-bus"
    assert html =~ ":ml_turn"
    assert html =~ ":pipeline"
    assert html =~ "no region attribution"
    assert html =~ "not verified function executions"
  end
  test "graph is the primary view while the existing cards remain an optional list" do
    nodes = [
      %{key: :lifg, label: "LIFG", subtitle: "Lexical", description: "Lexical",
        modules: [Brain.LIFG], telemetry: [], related: [:atl]},
      %{key: :atl, label: "ATL", subtitle: "Integration", description: "Integration",
        modules: [Brain.ATL], telemetry: [], related: [:lifg]}
    ]

    graph = render_component(&NeuralAtlas.panel/1, nodes: nodes, selected: :lifg)
    list = render_component(&NeuralAtlas.panel/1,
      nodes: nodes, selected: :lifg, display: :list)

    assert graph =~ ~s(id="atlas-network-graph")
    assert graph =~ ~s(id="atlas-display-graph")
    assert graph =~ ~s(id="atlas-region-lifg")
    assert graph =~ ~s(phx-click="atlas-display")
    refute graph =~ ~s(id="atlas-region-grid")

    assert list =~ ~s(id="atlas-region-grid")
    assert list =~ ~s(id="atlas-region-lifg")
    refute list =~ ~s(id="atlas-network-graph")
    assert list =~ "Registered modules"
  end

end
