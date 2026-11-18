defmodule SymbrellaWeb.Components.Brain.ExecutiveTracePanelTest do
  use SymbrellaWeb.ConnCase, async: true
  import Phoenix.LiveViewTest

  alias SymbrellaWeb.Components.Brain.ExecutiveTracePanel
  alias SymbrellaWeb.Components.Brain.NeuralAtlas

  @trace %{
    trace_id: "turn-abc321",
    status: :completed,
    events: [
      %{stage: :prefrontal_control, status: :ok, region: :prefrontal,
        duration_us: 2_000, at_ms: 1_000,
        evidence: %{
          available: true, feedback_quality: :heuristic_proxy,
          controls: %{top_k: 5, branch_budget: 2, utility_prior: 0.43},
          planners: [
            %{region: :dlpfc, status: :produced, reason: :validated,
              proposed_keys: [:top_k], merged_keys: [:top_k],
              superseded_keys: []},
            %{region: :fpc, status: :produced, reason: :validated,
              proposed_keys: [:branch_budget], merged_keys: [:branch_budget],
              superseded_keys: []},
            %{region: :vmpfc, status: :produced, reason: :validated,
              proposed_keys: [:utility_prior], merged_keys: [:utility_prior],
              superseded_keys: []},
            %{region: :dmpfc, status: :unavailable, reason: :module_not_loaded,
              proposed_keys: [], merged_keys: [], superseded_keys: []}
          ]
        }},
      %{stage: :action_selection, status: :ok, region: nil,
        duration_us: 500, at_ms: 1_002,
        evidence: %{
          shadow_status: :evaluated, shadow_reason: :bounded_candidate_modulation,
          live_action: :answer_user,
          shadow_action: :ask_clarifying_question, changed: true,
          applied_keys: [:branch_budget, :utility_prior],
          unused_keys: [:top_k],
          applied_sources: [
            %{key: :branch_budget, region: :fpc},
            %{key: :utility_prior, region: :vmpfc}
          ],
          adjustments: [
            %{action: :answer_user, baseline: 0.5, proposed: 0.45, delta: -0.05},
            %{action: :ask_clarifying_question, baseline: 0.42,
              proposed: 0.50, delta: 0.08}
          ]
        }}
    ]
  }

  test "renders planner execution, merged values, and shadow impact separately" do
    html = render_component(&ExecutiveTracePanel.panel/1, trace: @trace)

    assert html =~ ~s(id="atlas-executive-observability")
    assert html =~ ~s(id="atlas-executive-planners")
    assert html =~ ~s(data-planner-region="fpc")
    assert html =~ ~s(data-planner-region="dmpfc")
    assert html =~ "called · invalid output" or html =~ "unavailable · not called"
    assert html =~ "branch_budget: 2"
    assert html =~ "Used in shadow scoring:"
    assert html =~ ~s(id="atlas-executive-shadow")
    assert html =~ "answer_user"
    assert html =~ "ask_clarifying_question"
    assert html =~ ~s(id="atlas-executive-scores")
    assert html =~ "heuristic_proxy"
    refute html =~ "PRIVATE INPUT"
  end

  test "region inspector shows accurate planner status, not all planned links" do
    report = ExecutiveTracePanel.report(@trace)

    assert ExecutiveTracePanel.planner(report, :fpc).status == :produced
    assert ExecutiveTracePanel.shadow_used(report, :fpc) == [:branch_budget]
    assert ExecutiveTracePanel.shadow_used(report, :dmpfc) == []
    assert ExecutiveTracePanel.merged_values(report,
             ExecutiveTracePanel.planner(report, :dlpfc)) == [%{key: :top_k, value: 5}]

    html = render_component(&ExecutiveTracePanel.inspector/1,
      executive: report, region: :fpc)

    assert html =~ ~s(id="atlas-region-executive")
    assert html =~ "branch_budget: 2"
    assert html =~ "Shadow scoring:"
  end

  test "empty and legacy turn traces do not invent executive observations" do
    no_trace = render_component(&ExecutiveTracePanel.panel/1, trace: nil)
    assert no_trace =~ ~s(id="atlas-executive-empty")
    refute no_trace =~ ~s(id="atlas-executive-shadow")

    legacy = %{
      trace_id: "turn-000001",
      events: [%{stage: :lifg_selection, status: :ok, region: :lifg,
                 duration_us: 100, evidence: %{choices: 1}}]
    }

    old = render_component(&ExecutiveTracePanel.panel/1, trace: legacy)
    assert old =~ ~s(id="atlas-executive-empty")
    refute old =~ ~s(data-planner-region="fpc")
  end

  test "atlas uses only the selected turn's executive receipts" do
    nodes = [
      %{key: :prefrontal, label: "Prefrontal", subtitle: "Executive",
        description: "Executive", modules: [Brain.Prefrontal],
        telemetry: [], related: [:fpc, :dlpfc]},
      %{key: :fpc, label: "FPC", subtitle: "Branching",
        description: "Branching", modules: [Brain.FPC],
        telemetry: [], related: [:prefrontal]},
      %{key: :dlpfc, label: "DLPFC", subtitle: "Policy",
        description: "Policy", modules: [Brain.DLPFC.Policy],
        telemetry: [], related: [:prefrontal]}
    ]

    unrelated = %{trace_id: "turn-111111", status: :completed, events: []}
    current = render_component(&NeuralAtlas.panel/1,
      nodes: nodes, selected: :fpc, layer: :trace,
      traces: [unrelated, @trace], trace_id: @trace.trace_id)

    assert current =~ ~s(data-layer="trace")
    assert current =~ ~s(data-planner-region="fpc")
    assert current =~ "branch_budget: 2"
    assert current =~ "instrumented region stages in the selected turn"
    assert current =~ "reported planner calls"
    refute current =~ ~s(data-edge-kind="beam")

    prior = render_component(&NeuralAtlas.panel/1,
      nodes: nodes, selected: :fpc, layer: :trace,
      traces: [unrelated, @trace], trace_id: unrelated.trace_id)

    assert prior =~ ~s(id="atlas-executive-empty")
    refute prior =~ ~s(data-planner-region="fpc")
  end
end
