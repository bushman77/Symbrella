defmodule SymbrellaWeb.Region.Pmtg do
  use SymbrellaWeb.Region,
    key: :pmtg,

    # pMTG lens ~one-third area (scaled around {342,276}); same anchor/tweak.
    path: "M310.8,258.7 C321.2,252.9 342,252.9 357,258.7 C366.2,262.1 368.6,272.5 366.2,281.8 C363.9,292.2 352.4,299.1 336.2,300.2 C322.4,301.4 313.1,296.8 309.7,287.5 C307.4,281.8 307.4,265.6 310.8,258.7 Z",
    colors: {"#8B5CF6", "#7C3AED"},
    anchor: {342, 276},
    tweak:  %{dx: 30, dy: -100, s: 1.00}
end

