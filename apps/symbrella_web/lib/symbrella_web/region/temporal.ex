defmodule SymbrellaWeb.Region.Temporal do
  use SymbrellaWeb.Region,
    key: :temporal,
    path:
      "M220,238 C200,258 200,300 232,320 C266,340 326,338 362,318 C392,298 382,260 350,238 C318,216 258,218 220,238 Z",
    colors: {"#0EA5E9", "#0284C7"},
    anchor: {300, 304},
    tweak: %{dx: -200, dy: -175, s: 1.00}
end
