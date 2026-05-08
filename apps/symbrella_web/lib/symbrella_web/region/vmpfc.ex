defmodule SymbrellaWeb.Region.Vmpfc do
  use SymbrellaWeb.Region,
    key: :vmpfc,
    path:
      "M105,180 C128,168 163,170 197,187 C216,198 233,215 240,236 " <>
        "C215,245 189,251 160,244 C130,236 105,219 94,199 " <>
        "C95,190 99,184 105,180 Z",
    colors: {"#22C55E", "#16A34A"},
    anchor: {154, 210},
    tweak: %{dx: 0, dy: 0, s: 1.00}
end
