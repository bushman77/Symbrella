defmodule SymbrellaWeb.Region.Lifg do
  @moduledoc """
  LIFG (Left Inferior Frontal Gyrus) — union of Brodmann areas 44, 45, and 47.
  Left-lateral view. Boundaries:
    • Roof  → inferior frontal sulcus
    • Floor → upper bank of the Sylvian fissure
    • Posterior → precentral sulcus limit
    • Anterior → frontal pole / pars orbitalis influence
  """

  use SymbrellaWeb.Region,
    key: :lifg,
    # Cleaned & anatomically refined path (2025 version)
    path:
      "M170.5,219.5 " <>
      "C183,208 196,204 208,204 " <>          # superior contour along inferior frontal sulcus
      "C225,204.5 238,207 244,213 " <>        # posterior rise toward BA44
      "L244,217 " <>                           # short vertical segment (posterior boundary)
      "C238,228 229,236 212,241 " <>           # descent into Sylvian fissure (floor)
      "L188,241 " <>                           # inferior edge (upper bank of lateral fissure)
      "C180,233 173,225 170.5,219.5 " <>       # smooth anterior return with pars triangularis dip
      "Z",
    colors: {"#F43F5E", "#E11D48"},
    anchor: {206, 222},
    tweak: %{dx: -90, dy: -10, s: 1.00}
end

