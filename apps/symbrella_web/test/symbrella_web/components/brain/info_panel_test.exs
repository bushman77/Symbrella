defmodule SymbrellaWeb.Components.Brain.InfoPanelTest do
  use SymbrellaWeb.ConnCase, async: true

  import Phoenix.LiveViewTest

  alias SymbrellaWeb.Components.Brain.InfoPanel

  test "renders safely with missing keys in data + snapshot" do
    html =
      render_component(&InfoPanel.info_panel/1, %{
        selected: :lifg,
        # missing title/summary/modules/telemetry/config
        data: %{},
        # missing module/pid/info/state
        snapshot: %{running?: true},
        full_names: %{}
      })

    assert html =~ "Selected:"
    assert html =~ "Live state"
  end
end
