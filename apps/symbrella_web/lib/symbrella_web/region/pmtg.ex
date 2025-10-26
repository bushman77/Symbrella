defmodule SymbrellaWeb.Region.Pmtg do
  use SymbrellaWeb.Region,
    key: :pmtg,

    # Lens hugging posterior STG/MTG. Slight posterior bulge and softer inferior curve.
    path: "M288,246 C306,236 342,236 368,246 C384,252 388,270 384,286 C380,304 360,316 332,318 C308,320 292,312 286,296 C282,286 282,258 288,246 Z",
    colors: {"#8B5CF6", "#7C3AED"},
    anchor: {342, 276},
    tweak:  %{dx: 150, dy: 150, s: 0.40}
end

