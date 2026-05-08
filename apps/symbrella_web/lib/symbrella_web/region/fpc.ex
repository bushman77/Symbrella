defmodule SymbrellaWeb.Region.Fpc do
  use SymbrellaWeb.Region,
    key: :fpc,
    path:
      "M65,132 C78,106 105,87 133,78 C148,75 160,78 169,86 " <>
        "C154,101 145,120 146,141 C134,158 110,174 86,169 " <>
        "C68,160 58,145 65,132 Z",
    colors: {"#F472B6", "#DB2777"},
    anchor: {108, 146},
    tweak: %{dx: 0, dy: 0, s: 1.00}
end
