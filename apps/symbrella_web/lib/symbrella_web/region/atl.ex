defmodule SymbrellaWeb.Region.Atl do
  use SymbrellaWeb.Region,
    key: :atl,
    path:
      "M198,285 C216,266 245,257 270,265 C288,271 302,286 306,304 " <>
        "C296,324 270,340 240,340 C214,339 195,326 188,306 " <>
        "C187,298 191,291 198,285 Z",
    colors: {"#06B6D4", "#0891B2"},
    anchor: {238, 306},
    tweak: %{dx: 0, dy: 0, s: 1.00}
end
