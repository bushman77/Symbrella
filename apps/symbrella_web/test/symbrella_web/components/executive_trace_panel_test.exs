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

  test "decision lineage distinguishes selection, reconciliation and command receipts" do
    lineage_event = %{
      stage: :response_planning, region: nil, status: :ok,
      duration_us: 140, at_ms: 1_005,
      evidence: %{
        initial_action: :ask_clarifying_question,
        final_action: :answer_user,
        initial_safety_gate: :approved, final_safety_gate: :approved,
        reconciliation: :reconciled,
        reason: :social_intent_reconciliation,
        shadow_action: :observe_silently, shadow_status: :evaluated,
        shadow_applied_keys: [:utility_prior],
        initial_command_statuses: [:deferred],
        final_command_statuses: [],
        delivery_status: :not_observed
      }
    }

    selected = Map.update!(@trace, :events, &(&1 ++ [lineage_event]))
    report = ExecutiveTracePanel.report(selected)

    assert report.has_lineage?
    assert report.lineage.initial_action == :ask_clarifying_question
    assert report.lineage.final_action == :answer_user
    assert report.lineage.reconciliation == :reconciled
    assert report.lineage.delivery_status == :not_observed

    html = render_component(&ExecutiveTracePanel.panel/1, trace: selected)
    assert html =~ ~s(id="atlas-decision-lineage")
    assert html =~ ~s(data-decision-stage="initial")
    assert html =~ ~s(data-decision-stage="final")
    assert html =~ ~s(data-decision-stage="reconciliation")
    assert html =~ ~s(data-decision-stage="execution")
    assert html =~ "ask_clarifying_question"
    assert html =~ "social_intent_reconciliation"
    assert html =~ "deferred"
    assert html =~ "Response delivery: not_observed"
    assert html =~ "Shadow scoring did not change the authoritative live selection."
    refute html =~ ~s(id="atlas-decision-lineage-empty")

    old = render_component(&ExecutiveTracePanel.panel/1, trace: @trace)
    assert old =~ ~s(id="atlas-decision-lineage-empty")
    refute old =~ ~s(id="atlas-decision-lineage")
  end

  test "incomplete legacy lineage evidence never crashes the inspector" do
    incomplete = %{
      trace_id: "turn-incomplete",
      events: [
        %{stage: :response_planning, status: :ok,
          evidence: %{reconciliation: :reconciled}}
      ]
    }

    report = ExecutiveTracePanel.report(incomplete)
    refute report.has_lineage?

    html = render_component(&ExecutiveTracePanel.panel/1, trace: incomplete)
    assert html =~ ~s(id="atlas-decision-lineage-empty")
    refute html =~ ~s(id="atlas-decision-lineage")
  end

  test "lineage is selected-turn-only even when another turn is newer or expired" do
    event = %{
      stage: :response_planning, status: :ok, region: nil,
      duration_us: 200, at_ms: 1_005,
      evidence: %{
        initial_action: :ask_clarifying_question, final_action: :answer_user,
        reconciliation: :reconciled, reason: :social_intent_reconciliation,
        initial_safety_gate: :approved, final_safety_gate: :approved,
        shadow_action: :observe_silently, shadow_status: :evaluated,
        shadow_applied_keys: [], initial_command_statuses: [],
        final_command_statuses: [], delivery_status: :not_observed
      }
    }
    traced = Map.update!(@trace, :events, &(&1 ++ [event]))
    unrelated = %{trace_id: "turn-newer", status: :completed, events: []}
    nodes = NeuralAtlas.regions()

    active = render_component(&NeuralAtlas.panel/1,
      nodes: nodes, selected: :prefrontal, layer: :trace,
      traces: [unrelated, traced], trace_id: traced.trace_id)
    assert active =~ ~s(id="atlas-decision-lineage")
    assert active =~ "social_intent_reconciliation"

    prior = render_component(&NeuralAtlas.panel/1,
      nodes: nodes, selected: :prefrontal, layer: :trace,
      traces: [unrelated, traced], trace_id: unrelated.trace_id)
    assert prior =~ ~s(id="atlas-decision-lineage-empty")
    refute prior =~ ~s(id="atlas-decision-lineage")

    expired = render_component(&NeuralAtlas.panel/1,
      nodes: nodes, selected: :prefrontal, layer: :trace,
      traces: [traced], trace_id: "turn-expired")
    assert expired =~ ~s(id="atlas-decision-lineage-empty")
    refute expired =~ "social_intent_reconciliation"
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
    assert current =~ ~s(data-edge-kind="trace-planner_invocation")
    assert current =~ ~s(data-trace-edge-kind="planner_invocation")
    assert current =~ "Reported Prefrontal planner invocation"
    assert current =~ ~s(id="atlas-region-trace-connections")
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
    refute prior =~ ~s(data-edge-kind="trace-planner_invocation")

    expired = render_component(&NeuralAtlas.panel/1,
      nodes: nodes, selected: :prefrontal, layer: :trace,
      traces: [@trace, unrelated], trace_id: "turn-expired")

    assert expired =~ ~s(id="atlas-executive-empty")
    assert expired =~ "No observed connections for this region in the selected turn."
    refute expired =~ ~s(data-edge-kind="trace-planner_invocation")
    refute expired =~ ~s(data-trace-edge-kind="planner_invocation")
  end
end
