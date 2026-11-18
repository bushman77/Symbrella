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

  test "reconciled Atlas no longer leaves the implemented cognitive regions isolated" do
    alias SymbrellaWeb.Components.Brain.NeuralAtlas

    nodes = NeuralAtlas.regions()
    links = NeuralAtlasGraph.edges(nodes, :conceptual, [])

    connected =
      links
      |> Enum.flat_map(fn edge -> [edge.from, edge.to] end)
      |> MapSet.new()

    for key <- [:frontal, :fpc, :dlpfc, :dmpfc, :vmpfc, :ofc, :cerebellum] do
      assert MapSet.member?(connected, key),
             "Reconciled region #{key} has lost its documented neighbor"
    end
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

  test "mobile viewport keeps a full-size canvas without relying on generated height utilities" do
    html = render_component(&NeuralAtlasGraph.graph/1,
      nodes: @nodes, selected: :lifg, layer: :conceptual)

    assert html =~ ~s(id="atlas-network-viewport")
    assert html =~ ~s(role="region")
    assert html =~ ~s(tabindex="0")
    assert html =~ ~s(aria-label="Scrollable Symbrella cognitive-region network")
    assert html =~ ~s(aria-describedby="atlas-mobile-scroll-hint")
    assert html =~ ~s(id="atlas-mobile-scroll-hint")
    assert html =~ "Swipe sideways to explore the network"

    assert html =~ ~s(id="atlas-network-canvas")
    assert html =~ "position: relative; width: 100%; min-width: 1000px; height: 600px;"
    assert html =~ "max-width: 100%; overflow-x: auto;"
    assert html =~ "-webkit-overflow-scrolling: touch;"
    assert html =~ "pointer-events: none; position: absolute; inset: 0;"
    refute html =~ ~s(min-w-[940px])
  end

  test "node selection and graph geometry retain CSS-independent positioning fallbacks" do
    html = render_component(&NeuralAtlasGraph.graph/1,
      nodes: @nodes, selected: :lifg)

    assert html =~ ~s(id="atlas-region-lifg")
    assert html =~ ~s(phx-click="select-region")
    assert html =~ ~s(aria-pressed="true")
    assert html =~ "position: absolute; left: 39.0%; top: 65.0%; width: 88px;"
    assert html =~ "transform: translate(-50%, -50%);"
    # Tailwind 4's translate property would otherwise compound the inline transform.
    refute html =~ "-translate-x-1/2"
    refute html =~ "-translate-y-1/2"
    assert html =~ ~s(viewBox="0 0 1000 600")
    assert html =~ ~s(data-role="registered-module")
  end

  test "non-collapsing mobile canvas survives every evidence layer" do
    for layer <- [:conceptual, :source, :dependencies, :telemetry, :bus, :trace] do
      html = render_component(&NeuralAtlasGraph.graph/1,
        nodes: @nodes, selected: :lifg, layer: layer)

      assert html =~ ~s(id="atlas-network-viewport")
      assert html =~ ~s(id="atlas-network-canvas")
      assert html =~ "min-width: 1000px; height: 600px;"
      assert html =~ ~s(data-layer="#{layer}")
      assert html =~ ~s(id="atlas-region-lifg")
    end
  end

  @trace_nodes @nodes ++ [
    %{key: :prefrontal, label: "Prefrontal", subtitle: "Planner aggregation",
      description: "Executive", modules: [Brain.Prefrontal],
      related: [:dlpfc, :fpc, :vmpfc, :dmpfc], telemetry: []},
    %{key: :dlpfc, label: "DLPFC", subtitle: "Policy",
      description: "Policy", modules: [Brain.DLPFC.Policy],
      related: [:prefrontal], telemetry: []},
    %{key: :fpc, label: "FPC", subtitle: "Branching",
      description: "Branching", modules: [Brain.FPC],
      related: [:prefrontal], telemetry: []},
    %{key: :vmpfc, label: "VMPFC", subtitle: "Utility",
      description: "Utility", modules: [Brain.VmPFC],
      related: [:prefrontal], telemetry: []},
    %{key: :dmpfc, label: "DMPFC", subtitle: "Confidence",
      description: "Confidence", modules: [Brain.DmPFC],
      related: [:prefrontal], telemetry: []},
    %{key: :salience, label: "Salience", subtitle: "Attention",
      description: "Attention", modules: [Brain.Salience],
      related: [:prefrontal], telemetry: []},
    %{key: :hippocampus, label: "Hippocampus", subtitle: "Memory",
      description: "Memory", modules: [Brain.Hippocampus],
      related: [:atl], telemetry: []}
  ]

  @completed_turn %{
    trace_id: "turn-aabbcc",
    events: [
      %{stage: :lifg_selection, region: :lifg, status: :ok,
        evidence: %{choices: 3}},
      %{stage: :atl_integration, region: :atl, status: :ok,
        evidence: %{slate_present: true}},
      %{stage: :prefrontal_control, region: :prefrontal, status: :ok,
        evidence: %{
          available: true,
          planners: [
            %{region: :dlpfc, status: :produced},
            %{region: :fpc, status: :no_opinion},
            %{region: :vmpfc, status: :failed},
            %{region: :dmpfc, status: :unavailable},
            %{region: :salience, status: :rejected}
          ]
        }},
      %{stage: :action_selection, region: nil, status: :ok, evidence: %{}},
      %{stage: :hippocampus_persist, region: :hippocampus,
        status: :ok, evidence: %{}}
    ]
  }

  test "reported Prefrontal planner invocations become edges, unavailable planners do not" do
    edges = NeuralAtlasGraph.edges(@trace_nodes, :trace, @completed_turn)

    invocations = Enum.filter(edges, &(&1.kind == :planner_invocation))
    assert length(invocations) == 4
    assert Enum.all?(invocations, &(&1.from == :prefrontal and &1.directed))

    assert Enum.any?(invocations, &(&1.to == :dlpfc and &1.status == :produced))
    assert Enum.any?(invocations, &(&1.to == :fpc and &1.status == :no_opinion))
    assert Enum.any?(invocations, &(&1.to == :vmpfc and &1.status == :failed))
    assert Enum.any?(invocations, &(&1.to == :salience and &1.status == :rejected))
    refute Enum.any?(invocations, &(&1.to == :dmpfc))

    # Planner provenance does not imply execution of its recommendation.
    refute Enum.any?(invocations, &Map.has_key?(&1, :live_action_applied))
  end

  test "completed region stages form order-only links, not claimed direct calls" do
    edges = NeuralAtlasGraph.edges(@trace_nodes, :trace, @completed_turn)
    order = Enum.filter(edges, &(&1.kind == :stage_order))

    assert Enum.map(order, &{&1.from, &1.to}) == [
      {:lifg, :atl},
      {:atl, :prefrontal},
      {:prefrontal, :hippocampus}
    ]
    assert Enum.all?(order, &(&1.directed and is_atom(&1.from_stage) and
                                is_atom(&1.to_stage)))
    refute Enum.any?(order, &Map.has_key?(&1, :status))
  end

  test "trace edges require successful source stages and explicit planner reports" do
    empty = %{trace_id: "turn-empty", events: []}
    assert NeuralAtlasGraph.edges(@trace_nodes, :trace, empty) == []
    assert NeuralAtlasGraph.edges(@trace_nodes, :trace, nil) == []

    # A started or failed stage is not a completed-stage edge.
    incomplete = %{trace_id: "turn-incomplete", events: [
      %{stage: :lifg_selection, region: :lifg, status: :started},
      %{stage: :atl_integration, region: :atl, status: :error},
      %{stage: :prefrontal_control, region: :prefrontal, status: :error,
        evidence: %{available: true, planners: [%{region: :dlpfc, status: :produced}]}},
      %{stage: :hippocampus_persist, region: :hippocampus, status: :started}
    ]}

    assert NeuralAtlasGraph.edges(@trace_nodes, :trace, incomplete) == []

    unavailable = %{trace_id: "turn-unavailable", events: [
      %{stage: :prefrontal_control, region: :prefrontal, status: :ok,
        evidence: %{available: false,
          planners: [%{region: :dlpfc, status: :produced}]}}
    ]}
    assert NeuralAtlasGraph.edges(@trace_nodes, :trace, unavailable) == []
  end

  test "observations cannot fabricate links between unrelated registered regions" do
    unknown = %{trace_id: "turn-unknown", events: [
      %{stage: :invented, region: :fpc, status: :ok},
      %{stage: :lifg_selection, region: :lifg, status: :ok},
      %{stage: :atl_integration, region: :unregistered, status: :ok},
      %{stage: :prefrontal_control, region: :prefrontal, status: :ok,
        evidence: %{available: true, planners: [
          %{region: :dmpfc, status: :unavailable},
          %{region: :bad_region, status: :produced}
        ]}}
    ]}

    assert NeuralAtlasGraph.edges(@trace_nodes, :trace, unknown) ==
             [%{kind: :stage_order, from: :lifg, to: :prefrontal,
                directed: true, from_stage: :lifg_selection,
                to_stage: :prefrontal_control}]
    # Conceptual metadata still renders only as faint dotted context.
    html = render_component(&NeuralAtlasGraph.graph/1,
      nodes: @trace_nodes, selected: :lifg, layer: :trace, trace: unknown)
    assert html =~ ~s(data-edge-kind="conceptual-context")
    assert html =~ ~s(data-edge-kind="trace-stage_order")
    refute html =~ ~s(data-edge-kind="trace-planner_invocation")
    refute html =~ ~s(data-edge-kind="beam")
  end

  test "trace graph draws visually distinct observed invocations and stage-order connections" do
    html = render_component(&NeuralAtlasGraph.graph/1,
      nodes: @trace_nodes, selected: :prefrontal,
      layer: :trace, trace: @completed_turn)

    assert html =~ ~s(data-layer="trace")
    assert html =~ ~s(data-edge-kind="trace-planner_invocation")
    assert html =~ ~s(data-edge-kind="trace-stage_order")
    assert html =~ ~s(data-invocation-status="no_opinion")
    assert html =~ "url(#atlas-network-invocation-arrow)"
    assert html =~ ~s(id="atlas-trace-edge-legend")
    assert html =~ "4 reported planner invocations (solid amber)"
    assert html =~ "3 successive completed region stages"
    assert html =~ "order only"
    assert html =~ ~s(id="atlas-network-viewport")
    assert html =~ "min-width: 1000px; height: 600px;"
    refute html =~ ~s(data-edge-kind="beam")
  end

  test "selected-region inspector explains stage order without claiming a direct call" do
    alias SymbrellaWeb.Components.Brain.NeuralAtlas

    html = render_component(&NeuralAtlas.panel/1,
      nodes: @trace_nodes, selected: :atl, layer: :trace,
      traces: [@completed_turn], trace_id: @completed_turn.trace_id)

    assert html =~ ~s(id="atlas-region-trace-connections")
    assert html =~ ~s(data-trace-edge-kind="stage_order")
    assert html =~ "Successive completed region stages"
    assert html =~ "order only, not a direct call"
    assert html =~ "LIFG"
    assert html =~ "Prefrontal"

    empty = %{trace_id: "turn-no-activity", events: []}
    quiet = render_component(&NeuralAtlas.panel/1,
      nodes: @trace_nodes, selected: :atl, layer: :trace,
      traces: [empty, @completed_turn], trace_id: empty.trace_id)

    assert quiet =~ "No observed connections for this region in the selected turn."
    refute quiet =~ ~s(data-trace-edge-kind="stage_order")
    refute quiet =~ ~s(data-trace-edge-kind="planner_invocation")
  end

  test "choosing another turn replaces all active links while keeping conceptual context" do
    quiet = %{trace_id: "turn-quiet", events: [
      %{stage: :lifg_selection, region: :lifg, status: :ok, evidence: %{}}
    ]}

    active_html = render_component(&NeuralAtlasGraph.graph/1,
      nodes: @trace_nodes, selected: :prefrontal,
      layer: :trace, trace: @completed_turn)
    quiet_html = render_component(&NeuralAtlasGraph.graph/1,
      nodes: @trace_nodes, selected: :prefrontal,
      layer: :trace, trace: quiet)

    assert active_html =~ ~s(data-edge-kind="trace-planner_invocation")
    refute quiet_html =~ ~s(data-edge-kind="trace-planner_invocation")
    refute quiet_html =~ ~s(data-edge-kind="trace-stage_order")
    assert quiet_html =~ ~s(data-edge-kind="conceptual-context")
    assert quiet_html =~ "0 reported planner invocations"
    assert quiet_html =~ "0 successive completed region stages"
  end

end
