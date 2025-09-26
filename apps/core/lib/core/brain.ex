defmodule Core.Brain do
  @moduledoc """
  Adapter/wrapper for the Brain OTP app. Keep Core in charge of orchestration.
  """
  alias Db.BrainCell, as: Row

  @default_payload %{delta: 0.12, decay: 0.98}

  @doc "Inject Brain’s active_cells map into the SI (calls Brain GenServer)."
  def stm(si) do
    GenServer.call(Brain, {:stm, si})
  end

  @doc """
  Activate a list of rows or ids.
  Accepts [%Db.BrainCell{} | id] and forwards to Brain.
  """
  def activate_cells(rows_or_ids, opts \\ []) do
    payload = Map.merge(@default_payload, Map.new(opts))
    GenServer.cast(Brain, {:activate_cells, rows_or_ids, payload})
    :ok
  end

  @doc "Convenience: activate whatever Db.ltm put into si.cells."
  def activate_from_si(%{cells: cells}) when is_list(cells) do
    activate_cells(cells)
  end

  def activate_from_si(_), do: :ok
end

