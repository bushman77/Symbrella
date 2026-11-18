defmodule SymbrellaWeb.Components.Brain.NeuralAtlasTest do
  use SymbrellaWeb.ConnCase, async: true
  import Phoenix.LiveViewTest

  alias SymbrellaWeb.Components.Brain.NeuralAtlas

  test "registered region cards and inspector are selectable and touch-friendly" do
    nodes = [
      %{
        key: :lifg,
        label: "LIFG",
        subtitle: "Lexical competition",
        description: "Lexical sense selection with evidence guards.",
        modules: [Brain.LIFG],
        telemetry: [[:brain, :lifg, :stage1, :winner]],
        related: [:atl]
      },
      %{
        key: :atl,
        label: "ATL",
        subtitle: "Integration",
        description: "Contextual integration",
        modules: [Brain.ATL],
        telemetry: [],
        related: [:lifg]
      }
    ]

    html = render_component(&NeuralAtlas.panel/1, nodes: nodes, selected: :lifg)

    assert html =~ "Symbrella Neural Atlas"
    assert html =~ "atlas-region-lifg"
    assert html =~ "atlas-region-atl"
    assert html =~ "phx-click=\"select-region\""
    assert html =~ "aria-pressed=\"true\""
    assert html =~ "Lexical sense selection with evidence guards"
    assert html =~ "Brain.LIFG"
    assert html =~ "conceptual navigation"
    assert html =~ "atlas-inspector"
  end

  test "registry metadata drives regions and labels without changing runtime" do
    nodes = NeuralAtlas.regions()

    assert Enum.any?(nodes, &(&1.key == :lifg))
    assert Enum.all?(nodes, &is_atom(&1.key))
    assert Enum.all?(nodes, &is_list(&1.modules))
  end
end
