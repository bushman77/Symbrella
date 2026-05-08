defmodule SymbrellaWeb.Region.Ofc do
  use SymbrellaWeb.Region,
    key: :ofc,
    path:
      "M131,225 C153,211 185,209 216,220 C239,229 258,246 263,264 " <>
        "C244,273 210,275 174,266 C143,258 121,245 113,234 " <>
        "C117,231 124,228 131,225 Z",
    colors: {"#FB7185", "#E11D48"},
    anchor: {172, 238},
    tweak: %{dx: 0, dy: 0, s: 1.00}
end
