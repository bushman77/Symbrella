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
    # superior contour along inferior frontal sulcus
    # posterior rise toward BA44
    # short vertical segment (posterior boundary)
    # descent into Sylvian fissure (floor)
    # inferior edge (upper bank of lateral fissure)
    # smooth anterior return with pars triangularis dip
    path:
      "M170.5,219.5 " <>
        "C183,208 196,204 208,204 " <>
        "C225,204.5 238,207 244,213 " <>
        "L244,217 " <>
        "C238,228 229,236 212,241 " <>
        "L188,241 " <>
        "C180,233 173,225 170.5,219.5 " <>
        "Z",
    colors: {"#F43F5E", "#E11D48"},
    anchor: {206, 222},
    tweak: %{dx: -90, dy: -10, s: 1.00}
end
