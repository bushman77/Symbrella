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
    assert html =~ "atlas-network-graph"
    assert html =~ "atlas-observed-telemetry"
    assert html =~ "1 seen"
    assert html =~ "22:13:20 UTC"
    assert html =~ "[:brain, :lifg]"
    assert html =~ "A received event is an observation"

    list_html = render_component(&NeuralAtlas.panel/1,
      nodes: nodes, selected: :lifg, layer: :telemetry, events: [event], display: :list)

    assert list_html =~ "atlas-region-grid"
    assert list_html =~ "Observed this session"
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


  test "previously isolated regions now expose registered implementation modules" do
    alias SymbrellaWeb.Region.Registry

    expected = %{
      frontal: [Brain.Frontal],
      prefrontal: [Brain.Prefrontal, Core.Brain.Prefrontal],
      fpc: [Brain.FPC],
      dlpfc: [Brain.DLPFC, Brain.DLPFC.Policy],
      vmpfc: [Brain.VmPFC],
      dmpfc: [Brain.DmPFC],
      ofc: [Brain.OFC],
      cerebellum: [Brain.Cerebellum],
      temporal: [Brain.Temporal]
    }

    Enum.each(expected, fn {key, modules} ->
      assert Registry.available?(key), "Region #{key} should be registered"
      assert Registry.defn(key).modules == modules
      assert Enum.all?(modules, &Code.ensure_loaded?/1),
             "Missing compiled module for #{key}"
    end)
  end

  test "source-backed relationships connect implemented regions without claiming execution" do
    alias SymbrellaWeb.Region.Registry

    edges = Registry.source_relations()
    nodes = NeuralAtlas.regions()
    known = MapSet.new(Enum.map(nodes, & &1.key))

    assert Enum.all?(edges, fn edge ->
             edge.kind in [:grouping, :planner_reference, :direct_call, :telemetry_subscription] and
               MapSet.member?(known, edge.from) and
               MapSet.member?(known, edge.to) and
               is_binary(edge.source) and edge.source != ""
           end)

    assert Enum.any?(edges, &(&1.from == :lifg and &1.to == :cerebellum and
                               &1.kind == :direct_call))
    assert Enum.any?(edges, &(&1.from == :dlpfc and &1.to == :thalamus and
                               &1.kind == :telemetry_subscription))
    assert Enum.any?(edges, &(&1.from == :prefrontal and &1.to == :fpc and
                               &1.kind == :planner_reference))

    # The default conceptual view now includes source-backed navigation links,
    # but it does not claim that these links executed in any given chat turn.
    assert :cerebellum in Registry.defn(:lifg).related
    assert :lifg in Registry.defn(:cerebellum).related
    assert :prefrontal in Registry.defn(:frontal).related
    assert :prefrontal in Registry.defn(:dmpfc).related
  end

  test "source topology inspector identifies the relationship kind and source witness" do
    nodes = NeuralAtlas.regions()

    html =
      render_component(&NeuralAtlas.panel/1,
        nodes: nodes, selected: :cerebellum, layer: :source)

    assert html =~ ~s(id="atlas-layer-source")
    assert html =~ ~s(id="atlas-source-relations")
    assert html =~ "direct call"
    assert html =~ "Brain.LIFG.Stage1.cereb_calibrate/4"
    assert html =~ "source-backed"
    assert html =~ "not evidence that execution occurred"
  end
end
