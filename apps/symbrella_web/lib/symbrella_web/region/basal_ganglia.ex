defmodule SymbrellaWeb.Region.BasalGanglia do
  use SymbrellaWeb.Region,
    key: :basal_ganglia,
    path:
      "M240,218 C260,202 296,202 316,220 C330,234 324,254 306,264 " <>
        "C282,278 246,268 234,246 C228,236 230,226 240,218 Z",
    colors: {"#64748B", "#334155"},
    anchor: {276, 238},
    tweak: %{dx: 0, dy: 0, s: 1.00}
end
