defmodule SymbrellaWeb.BrainHTMLTest do
  use SymbrellaWeb.ConnCase, async: true
  import Phoenix.LiveViewTest

  alias SymbrellaWeb.BrainHTML

  test "hud_row renders confidence float (0..1) as percent" do
    html =
      render_component(&BrainHTML.hud_row/1,
        intent: %{label: "greet", confidence: 0.7},
        mood: %{levels: %{}, derived: %{}}
      )

    assert html =~ "70.0%"
  end

  test "hud_row renders WM size/capacity" do
    html =
      render_component(&BrainHTML.hud_row/1,
        wm: %{size: 3, capacity: 7},
        mood: %{levels: %{}, derived: %{}}
      )

    assert html =~ "WM"
    assert html =~ "3/7"
  end

  test "hud_row shows self-name hit when attention stamp present" do
    html =
      render_component(&BrainHTML.hud_row/1,
        attention: %{self_name: %{hit?: true, match: "symbrella"}},
        mood: %{levels: %{}, derived: %{}}
      )

    assert html =~ "Self"
    assert html =~ "symbrella"
  end

  test "blackboard_panel renders wrapped ml_turn events" do
    html =
      render_component(&BrainHTML.blackboard_panel/1,
        events: [
          %{
            id: 1,
            at_ms: System.system_time(:millisecond),
            tag: :ml,
            env: %{
              kind: :ml_turn,
              region: :ml,
              turn_id: 42,
              hint: "turn record (ML)",
              turn: %{
                text: "do you think humans can survive a nuclear attack",
                intent: %{label: "ask", confidence: 0.7}
              }
            }
          }
        ],
        filter: "",
        limit: 50
      )

    assert html =~ "Blackboard Feed"
    assert html =~ "showing 1 / 1"
    assert html =~ "ml"
    assert html =~ "turn record"
    refute html =~ "— no events —"
  end

  test "blackboard_panel previews upgraded ml_turn response records" do
    html =
      render_component(&BrainHTML.blackboard_panel/1,
        events: [
          %{
            id: 1,
            at_ms: System.system_time(:millisecond),
            tag: :ml,
            env: %{
              kind: :ml_turn,
              region: :ml,
              turn_id: 43,
              hint: "turn record (ML)",
              turn: %{
                text: "close the loop",
                response: %{
                  assistant_text: "The response loop is now recorded.",
                  response_profile: :brain_explainer,
                  mode: :explainer
                }
              }
            }
          }
        ],
        filter: "",
        limit: 50
      )

    assert html =~ "ml_turn response"
    assert html =~ "brain_explainer"
    assert html =~ "The response loop is now recorded."
  end

  test "hud_row renders self-monitoring lifg payload gap count" do
    html =
      render_component(&BrainHTML.hud_row/1,
        self_portrait: %{
          traits: %{},
          patterns: %{lifg_payload_gaps: 2}
        },
        mood: %{levels: %{}, derived: %{}}
      )

    assert html =~ "lifg_gap"
    assert html =~ "2"
  end

  test "hud_row renders self-model state" do
    html =
      render_component(&BrainHTML.hud_row/1,
        self_model: %Brain.SelfModel{
          confidence: 0.75,
          uncertainty: 0.25,
          stability: 0.8,
          vigilance: 0.6,
          cognitive_load: 0.5,
          active_goals: [%{id: "phase-1"}],
          last_appraisal: %{evidence: %{target: :assistant}},
          last_lifg: %{choices_count: 2},
          continuity: %{
            reboot_restored?: true,
            degraded?: false,
            restore_reason: :restored,
            source_snapshot_v: 1
          }
        },
        mood: %{levels: %{}, derived: %{}}
      )

    assert html =~ "SelfModel"
    assert html =~ "conf"
    assert html =~ "0.750"
    assert html =~ "target"
    assert html =~ "assistant"
    assert html =~ "lifg"
    assert html =~ "2"
    assert html =~ "continuity"
    assert html =~ "restored"
    assert html =~ "reason"
    assert html =~ "v"
  end

  test "mood_panel renders pressure label and recent mood trace" do
    html =
      render_component(&BrainHTML.mood_panel/1,
        mood: %{
          levels: %{da: 0.41, "5ht": 0.52, glu: 0.41, ne: 0.53},
          derived: %{exploration: 0.46, inhibition: 0.52, vigilance: 0.53, plasticity: 0.41},
          tone: :cautious,
          pressure_label: :cautious_emergency_attention,
          mood_trace: [
            %{
              source: :appraisal,
              pressure_label: :cautious_emergency_attention,
              deltas: %{ne: 0.06, "5ht": -0.04}
            }
          ]
        }
      )

    assert html =~ "cautious_emergency_attention"
    assert html =~ "Recent pressure"
    assert html =~ "ne=+0.06"
    assert html =~ "5ht=-0.04"
  end

  test "brain map region registry exposes anatomically anchored overlays" do
    expected =
      ~w(frontal prefrontal dlpfc vmpfc dmpfc fpc lifg ofc acc thalamus basal_ganglia hippocampus temporal pmtg atl salience cerebellum)a

    for key <- expected do
      assert SymbrellaWeb.Region.Registry.available?(key), "#{key} should be drawable"

      defn = SymbrellaWeb.Region.Registry.defn(key)
      assert is_binary(defn.path) and defn.path != ""
      assert {x, y} = defn.anchor
      assert x >= 0 and x <= 516
      assert y >= 0 and y <= 406
      assert defn.tweak == %{dx: 0.0, dy: 0.0, s: 1.0}
    end
  end

  test "brain map region paths come from the shared SVG mask asset" do
    expected =
      ~w(frontal prefrontal dlpfc vmpfc dmpfc fpc lifg ofc acc thalamus basal_ganglia hippocampus temporal pmtg atl salience cerebellum)a

    assert Enum.sort(SymbrellaWeb.Region.Registry.mask_keys()) == Enum.sort(expected)

    assert SymbrellaWeb.Region.Registry.defn(:frontal).path =~ "C82,102"
    assert SymbrellaWeb.Region.Registry.defn(:acc).path =~ "L342,196"
  end

  test "blackboard_panel renders readable self-monitor previews" do
    html =
      render_component(&BrainHTML.blackboard_panel/1,
        events: [
          %{
            id: 1,
            at_ms: System.system_time(:millisecond),
            tag: :self_portrait,
            env: %{
              kind: :telemetry,
              event: [:brain, :self_portrait, :monitor],
              measurements: %{count: 1},
              meta: %{issue: :lifg_pos_anomaly, severity: :warning}
            }
          }
        ],
        filter: "",
        limit: 50
      )

    assert html =~ "self_portrait monitor: lifg_pos_anomaly warning"
    refute html =~ "— no events —"
  end
end
