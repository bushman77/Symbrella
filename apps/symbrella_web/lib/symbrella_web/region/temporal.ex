defmodule SymbrellaWeb.Region.Temporal do
  use SymbrellaWeb.Region,
    key: :temporal,
    path:
      "M217,237 C249,221 303,219 347,237 C376,251 394,277 388,300 " <>
        "C378,330 333,348 285,342 C243,337 209,318 198,292 " <>
        "C190,270 199,250 217,237 Z",
    colors: {"#0EA5E9", "#0284C7"},
    anchor: {300, 304},
    tweak: %{dx: 0, dy: 0, s: 1.00}
end
