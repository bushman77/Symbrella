defmodule Brain.Index do
  @moduledoc false
  alias Db.BrainCell

  @spec merge_active(map(), [BrainCell.t()], [String.t()]) :: map()
  def merge_active(state, cells, started_ids \\ []) do
    active_map =
      Enum.reduce(cells, state.active_cells || %{}, fn %BrainCell{id: id} = cell, acc ->
        Map.put(acc, id, %{cell: cell, status: :active})
      end)

    ids = Map.keys(active_map)

    state
    |> Map.put(:active_cells, active_map)
    |> Map.put(:active_cell_ids, ids)
    |> Map.put(:active_cell_count, length(ids))
    |> Map.put(:last_started_ids, started_ids)
  end
end
