defmodule SymbrellaWeb.Components.Brain.CognitiveTracePanelTest do
  use SymbrellaWeb.ConnCase, async: true
  import Phoenix.LiveViewTest

  alias SymbrellaWeb.Components.Brain.CognitiveTracePanel
  alias SymbrellaWeb.Components.Brain.NeuralAtlas

  @trace %{
    trace_id: "turn-aabbcc99",
    status: :completed,
    started_at_ms: 1000,
    updated_at_ms: 1200,
    events: [
      %{stage: :turn, status: :started, region: nil, duration_us: 0,
        at_ms: 1000, evidence: %{}},
      %{stage: :lifg_selection, status: :ok, region: :lifg, duration_us: 2520,
        at_ms: 1100, evidence: %{choices: 2}},
      %{stage: :comprehension_summary, status: :ok, region: nil, duration_us: 1550,
        at_ms: 1120, evidence: %{degraded: true, understood: 0,
                                 uncertain: 2, unresolved: 1}},
      %{stage: :turn, status: :ok, region: nil, duration_us: 200_000,
        at_ms: 1200, evidence: %{}}
    ]
  }

  test "renders a correlated timeline without marking degraded comprehension as a runtime error" do
    html = render_component(&CognitiveTracePanel.panel/1, traces: [@trace], trace: @trace)

    assert html =~ ~s(id="atlas-cognitive-trace")
    assert html =~ ~s(id="atlas-trace-steps")
    assert html =~ ~s(data-stage="lifg_selection")
    assert html =~ ~s(data-stage="comprehension_summary")
    assert html =~ "degraded summary: true"
    assert html =~ "uncertain: 2"
    assert html =~ "2.52 ms"
    assert html =~ "Turn status: completed"
    refute html =~ "user message text"
  end

  test "shows an honest empty state before the first instrumented chat turn" do
    html = render_component(&CognitiveTracePanel.panel/1, traces: [], trace: nil)
    assert html =~ ~s(id="atlas-trace-empty")
    refute html =~ ~s(id="atlas-trace-steps")
  end

  test "turn trace mode highlights region operations while retaining the prior graph" do
    nodes = [
      %{key: :lifg, label: "LIFG", subtitle: "Lexical", description: "Lexical",
        modules: [Brain.LIFG], telemetry: [], related: [:atl]},
      %{key: :atl, label: "ATL", subtitle: "Integration", description: "Integration",
        modules: [Brain.ATL], telemetry: [], related: [:lifg]}
    ]

    html = render_component(&NeuralAtlas.panel/1,
      nodes: nodes, selected: :lifg, layer: :trace,
      traces: [@trace], trace_id: @trace.trace_id)

    assert html =~ ~s(id="atlas-layer-trace")
    assert html =~ ~s(data-layer="trace")
    assert html =~ "1 seen"
    assert html =~ "instrumented region stages in the selected turn"
    assert html =~ ~s(id="atlas-region-trace-steps")
    assert html =~ "LIFG · Lexical selection"
    assert html =~ ~s(id="atlas-trace-steps")
    refute html =~ ~s(data-edge-kind="beam")
  end
end
