defmodule Brain.WM.Focus do
  @moduledoc """
  Compatibility wrapper for the canonical WM admission boundary.
  """

  alias Brain.WM.Admission

  @type wm_item :: map()

  @spec run(map(), list() | map(), map() | keyword()) ::
          {[wm_item()], non_neg_integer(), non_neg_integer()}
  def run(state, cands_or_si, opts), do: Admission.run(state, cands_or_si, opts)
end
