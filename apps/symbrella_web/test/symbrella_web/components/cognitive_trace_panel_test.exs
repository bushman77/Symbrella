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

  @diagnosis %{
    degraded: true, understood: 0, uncertain: 1, unresolved: 0,
    reasons: [:weak_decision_rate_high, :fallback_rate_high],
    choice_count: 3, unresolved_content_count: 0, verified_lexical_count: 0,
    weak_rate: 0.666667, fallback_rate: 0.333333,
    structural_status: :preferred, sentence_meaning_status: :provisional,
    integration_status: :consistent, feedback_mode: :bounded_reanalysis,
    feedback_applied: false, repair_hint: :review_lexical_evidence,
    curiosity_gap_gate: :needs_unresolved_content
  }

  test "shows detailed selected-turn evidence without equating a grammar parse with understanding" do
    traced =
      Map.update!(@trace, :events, fn events ->
        Enum.map(events, fn
          %{stage: :comprehension_summary} = event ->
            %{event | evidence: @diagnosis}

          event -> event
        end)
      end)

    assert CognitiveTracePanel.diagnosis(traced) == @diagnosis

    html = render_component(&CognitiveTracePanel.panel/1,
      traces: [traced], trace: traced)
    assert html =~ ~s(id="atlas-comprehension-diagnosis")
    assert html =~ ~s(data-degradation-reason="weak_decision_rate_high")
    assert html =~ ~s(data-degradation-reason="fallback_rate_high")
    assert html =~ "High fallback-selection rate"
    assert html =~ "Independently verified lexical constructions"
    assert html =~ ~s(data-diagnosis-field="structural-status")
    assert html =~ ~s(data-diagnosis-field="sentence-meaning-status")
    assert html =~ ~s(data-diagnosis-field="integration-status")
    assert html =~ ~s(data-diagnosis-field="feedback")
    assert html =~ "bounded_reanalysis"
    assert html =~ "66.7%"
    assert html =~ "33.3%"
    assert html =~ "Review weak or fallback lexical evidence"
    assert html =~ "Requires unresolved content"
    assert html =~ "does not retry"
    refute html =~ ~s(id="atlas-comprehension-diagnosis-empty")
  end

  test "older and incomplete turn receipts show no inferred detailed diagnosis" do
    assert CognitiveTracePanel.diagnosis(@trace) == nil
    html = render_component(&CognitiveTracePanel.panel/1,
      traces: [@trace], trace: @trace)
    assert html =~ ~s(id="atlas-comprehension-diagnosis-empty")
    refute html =~ ~s(id="atlas-comprehension-diagnosis")

    incomplete =
      Map.update!(@trace, :events, fn events ->
        Enum.map(events, fn
          %{stage: :comprehension_summary} = event ->
            %{event | evidence: %{reasons: [:fallback_rate_high]}}

          event -> event
        end)
      end)

    assert CognitiveTracePanel.diagnosis(incomplete) == nil
  end

  test "atlas diagnoses only the chosen trace, not the newest unrelated turn" do
    diagnosed =
      Map.update!(@trace, :events, fn events ->
        Enum.map(events, fn
          %{stage: :comprehension_summary} = event ->
            %{event | evidence: @diagnosis}

          event -> event
        end)
      end)
    newer = %{@trace | trace_id: "turn-bb0011"}

    nodes = [
      %{key: :lifg, label: "LIFG", subtitle: "Lexical", description: "Lexical",
        modules: [Brain.LIFG], telemetry: [], related: [:atl]},
      %{key: :atl, label: "ATL", subtitle: "Integration", description: "Integration",
        modules: [Brain.ATL], telemetry: [], related: [:lifg]}
    ]

    chosen = render_component(&NeuralAtlas.panel/1,
      nodes: nodes, selected: :lifg, layer: :trace,
      traces: [newer, diagnosed], trace_id: diagnosed.trace_id)
    assert chosen =~ ~s(id="atlas-comprehension-diagnosis")

    other = render_component(&NeuralAtlas.panel/1,
      nodes: nodes, selected: :lifg, layer: :trace,
      traces: [newer, diagnosed], trace_id: newer.trace_id)
    assert other =~ ~s(id="atlas-comprehension-diagnosis-empty")
    refute other =~ ~s(id="atlas-comprehension-diagnosis")
  end

  test "renders only the selected turn's ordered social speech-act components" do
    social = %{
      stage: :speech_act_binding, region: nil, status: :ok,
      duration_us: 120, at_ms: 1140,
      evidence: %{
        speech_act: :social_update_query,
        social_components: [:greeting, :social_update_query]
      }
    }
    recent = Map.update!(@trace, :events, fn events ->
      List.insert_at(events, -1, social)
    end)
    prior = %{@trace | trace_id: "turn-111bbb"}

    selected = render_component(&NeuralAtlas.panel/1,
      nodes: [
        %{key: :lifg, label: "LIFG", subtitle: "Lexical", description: "Lexical",
          modules: [Brain.LIFG], telemetry: [], related: [:atl]},
        %{key: :atl, label: "ATL", subtitle: "Integration", description: "Integration",
          modules: [Brain.ATL], telemetry: [], related: [:lifg]}
      ],
      selected: :lifg, layer: :trace,
      traces: [prior, recent], trace_id: recent.trace_id)

    assert selected =~ "speech act: social_update_query"
    assert selected =~ "social components: greeting + social_update_query"
    assert selected =~ ~s(data-stage="speech_act_binding")

    # No component evidence is invented in the original, legacy trace.
    legacy = render_component(&CognitiveTracePanel.panel/1,
      trace: prior, traces: [prior])
    refute legacy =~ "social components: greeting + social_update_query"
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
