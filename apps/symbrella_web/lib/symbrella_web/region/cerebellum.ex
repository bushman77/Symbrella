defmodule SymbrellaWeb.Region.Cerebellum do
  @moduledoc """
  Cerebellum — fixed trace from brain.svg layer3 ("Cerebrellum").
  Left-lateral view. Closed silhouette for proper filling, matching the main outline stroke.
  Now visible in the lower-posterior position with transform-adjusted coords.
  """

  use SymbrellaWeb.Region,
    key: :cerebellum,
    # Closed version of the main path4219 (added Z, minor smoothing for fill)
    path:
      "M 464.40,257.17 " <>  # Start: anterior attachment (post-transform)
      "C 472.04,272.60 468.90,291.66 458.94,305.29 " <>  # Superior curve along tentorium
      "C 449.11,322.02 438.59,339.58 421.88,350.27 " <>  # Lateral descent to lobules
      "C 410.44,356.60 398.30,361.86 385.79,365.58 " <>  # Inferior midline bulge
      "C 373.21,367.73 360.28,366.41 347.75,364.69 " <>  # Anterior inferior roll
      "C 325.31,359.33 309.06,341.07 295.81,323.20 " <>  # Superior anterior rise
      "C 288.71,315.66 287.61,305.41 287.04,295.61 " <>  # Midline superior
      "C 286.48,291.59 285.52,287.58 283.68,283.94 " <>  # Back to start
      "Z",
    colors: {"#F59E0B", "#D97706"},  # Your original amber tones – warm for hindbrain
    anchor: {373, 332},  # Centered on the visible region (post-transform)
    tweak: %{dx: 0, dy: 0, s: 1.00}  # No offsets needed now – fits naturally
end
