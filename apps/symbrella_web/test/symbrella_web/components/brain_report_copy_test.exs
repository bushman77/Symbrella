defmodule SymbrellaWeb.BrainReportCopyTest do
  use SymbrellaWeb.ConnCase, async: true

  import Phoenix.LiveViewTest

  alias SymbrellaWeb.BrainHTML
  alias SymbrellaWeb.Components.Brain.Panels

  defmodule TestRuntime do
    def blackboard_history(100), do: [%{kind: :telemetry, evidence: "FULL_UNFILTERED_PAYLOAD"}]
    def cycle_snapshot, do: %{seq: 12}
    def mood_snapshot, do: %{exploration: 0.6}
    def brain_snapshot, do: {:ok, %{active: true}}
    def self_model_snapshot, do: %{uncertainty: 0.2}
    def self_portrait_snapshot, do: %{source: :test}
    def wm_snapshot, do: {:ok, %{wm: ["active memory"]}}
    def region_snapshot(_), do: %{lifg: :active}
    def region_status(_), do: %{status: :up}
    def all_region_status, do: %{lifg: %{status: :up}}
  end

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
    refute html =~ ~s(phx-hook="BrainReportCopy")
    assert html =~ ~s(id="brain-report-view-btn")
    assert html =~ "Copy Full Brain Report"
    assert html =~ "Open Report Text"
    assert html =~ "Download .txt"
    assert html =~ ~s(id="brain-report-download-btn")
    assert html =~ "/brain/report.txt?region=lifg"
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

  test "standalone Brain report handler is served without an asset compilation" do
    priv_dir = :symbrella_web |> :code.priv_dir() |> to_string()
    script = Path.join(priv_dir, "static/assets/brain_report_copy.js")

    assert File.exists?(script)
    source = File.read!(script)
    assert source =~ "brain-report-copy-btn"
    assert source =~ "brain-report-view-btn"
    assert source =~ "Complete Brain"
    assert source =~ "downloadReport"
  end

  test "native plain-text Brain report route is usable without JavaScript", %{conn: conn} do
    conn = get(conn, "/brain/report.txt?region=lifg")

    assert response(conn, 200) =~ "Symbrella - Complete Brain Diagnostics"
    assert response(conn, 200) =~ "Selected region: lifg"
    assert response(conn, 200) =~ "Blackboard history"
    assert get_resp_header(conn, "content-type") == ["text/plain; charset=utf-8"]
    assert get_resp_header(conn, "content-disposition") == ["inline"]
  end

  test "native Brain report downloads a text attachment", %{conn: conn} do
    conn = get(conn, "/brain/report.txt?region=lifg&download=1")

    assert response(conn, 200) =~ "Complete Brain Diagnostics"
    assert [disposition] = get_resp_header(conn, "content-disposition")
    assert disposition =~ "attachment;"
    assert disposition =~ "symbrella-brain-lifg-"
    assert disposition =~ ".txt"
  end

  test "native exporter reads the source blackboard data, not the visible HTML filter" do
    text = SymbrellaWeb.BrainReport.generate(:lifg,
      runtime: TestRuntime,
      captured_at: ~U[2026-10-13 12:00:00Z]
    )

    assert text =~ "Captured UTC: 2026-10-13T12:00:00Z"
    assert text =~ "FULL_UNFILTERED_PAYLOAD"
    assert text =~ "active memory"
    assert text =~ "===== All region process statuses ====="
  end

end
