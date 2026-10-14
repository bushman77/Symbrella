defmodule SymbrellaWeb.BrainReportCopyTest do
  use SymbrellaWeb.ConnCase, async: true

  import Phoenix.LiveViewTest

  alias SymbrellaWeb.BrainHTML
  alias SymbrellaWeb.Components.Brain.Panels

  test "brain dashboard offers one complete clipboard export with every diagnostic section" do
    html =
      render_component(&Panels.brain/1,
        selected: :lifg,
        regions: [:lifg],
        snapshot: %{},
        region_state: %{workspace: []},
        region_status: %{},
        mood: %{levels: %{}, derived: %{}, tone: :neutral},
        wm: %{size: 0, capacity: 12, items: []},
        clock: %{},
        intent: %{}
      )

    assert html =~ ~s(id="brain-report-copy-btn")
    assert html =~ ~s(phx-hook="BrainReportCopy")
    assert html =~ "Copy Full Brain Report"
    assert html =~ ~s(data-brain-report-section="HUD, Clock, Intent, Mood and Self-Model")
    assert html =~ ~s(data-brain-report-section="Selected Region Summary and Snapshot")
    assert html =~ ~s(data-brain-report-section="Complete Blackboard Feed")
    assert html =~ ~s(data-brain-report-section="Working Memory")
    assert html =~ ~s(data-brain-report-section="Live Region State")
    assert html =~ ~s(data-brain-report-section="Module Status")
    assert html =~ ~s(data-brain-report-section="Current Mood and Neuromodulators")
  end

  test "full blackboard export contains events hidden by an active filter" do
    now = System.system_time(:millisecond)

    events = [
      %{tag: :core_response, at_ms: now, env: %{kind: :telemetry, marker: "VISIBLE_REPORT_ITEM"}},
      %{tag: :lifg_score, at_ms: now, env: %{kind: :telemetry, marker: "HIDDEN_FROM_FILTER_BUT_INCLUDED"}}
    ]

    html = render_component(&BrainHTML.blackboard_panel/1,
      events: events,
      filter: "core_response",
      limit: 50
    )

    assert html =~ "showing 1 / 2"
    assert html =~ ~s(id="blackboard-copy-target")
    assert html =~ ~s(id="blackboard-full-copy-target")
    assert html =~ "HIDDEN_FROM_FILTER_BUT_INCLUDED"
  end

  test "unfiltered blackboard export reuses existing full copy target" do
    html = render_component(&BrainHTML.blackboard_panel/1, events: [], filter: "")

    assert html =~ ~s(id="blackboard-copy-target")
    refute html =~ ~s(id="blackboard-full-copy-target")
  end
end
