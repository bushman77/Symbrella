defmodule SymbrellaWeb.Components.HippoPanelTest do
  use SymbrellaWeb.ConnCase, async: true
  import Phoenix.LiveViewTest

  alias SymbrellaWeb.Components.Brain.HippoPanel

  test "renders safely with empty assigns (no KeyError)" do
    html = render_component(&HippoPanel.hippo_panel/1, %{})
    assert html =~ "Hippocampus"
    assert html =~ "no recall events"
  end
end
