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
end
