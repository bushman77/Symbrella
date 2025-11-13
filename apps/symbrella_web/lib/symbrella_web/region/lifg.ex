defmodule SymbrellaWeb.Region.Lifg do
  @moduledoc """
  LIFG (left inferior frontal gyrus) — union of BA44/45/47 from a left-lateral view.
  The path hugs the inferior frontal sulcus (roof), the Sylvian fissure (floor),
  and the ascending/horizontal rami forming the pars triangularis.
  """

  use SymbrellaWeb.Region,
    key: :lifg,
    # Union outline of BA44/45/47 (left IFG), already centered near {206,222}.
    path:
      "M169.2,220.2 C182.0,209.8 194.0,205.8 206.0,203.8 C218.0,204.2 230.8,205.0 242.8,202.6 L242.8,214.2 C240.0,225.0 235.2,230.2 212.0,241.4 L187.2,241.4 C176.4,227.8 169.2,220.2 169.2,220.2 Z",
    colors: {"#F43F5E", "#E11D48"},
    anchor: {206, 222},
    tweak: %{dx: -90, dy: -10, s: 1.00}
end
