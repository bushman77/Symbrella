defmodule Brain.Frontal do
  @moduledoc """
  Frontal lobe dashboard facade.

  This is not an OTP region process. It is a pure grouping node for the `/brain`
  map so the broad frontal overlay can report its relationship to concrete
  frontal/prefrontal regions without pretending to have independent state.
  """

  @doc "Return compact dashboard status for the pure frontal grouping region."
  @spec status() :: map()
  def status do
    %{
      region: :frontal,
      status: :available,
      mode: :pure_group,
      children: [:prefrontal, :lifg, :ofc, :acc, :dlpfc, :vmpfc, :dmpfc, :fpc],
      note: "Frontal is a visual/grouping region; child regions carry the active control state."
    }
  end
end
