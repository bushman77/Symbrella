defmodule Core.BrainAdapter do
  @moduledoc """
  Thin boundary so Core can start/fire cells and read Brain state
  without sprinkling Brain.* calls everywhere.
  """

  alias Brain.Cell

  # Snapshot passthrough (already used by you)
  def snapshot(), do: GenServer.call(Brain, :snapshot)

  # Start or return the running cell for a given id/attrs.
  # Accepts a map so we can start cells without a preloaded DB schema.
  @spec start_or_lookup_cell(map()) :: {:ok, pid()} | {:error, term()}
  def start_or_lookup_cell(%{id: id} = attrs) when is_binary(id) do
    case Registry.lookup(Brain.Registry, id) do
      [{pid, _}] ->
        {:ok, pid}

      [] ->
        # Start under Brain.CellSup; Brain.Cell.start_link/1 accepts a map
        # and casts it into %Db.BrainCell{} internally.
        case DynamicSupervisor.start_child(Brain.CellSup, {Cell, attrs}) do
          {:ok, pid} -> {:ok, pid}
          {:error, {:already_started, pid}} -> {:ok, pid}
          other -> other
        end
    end
  end

  # Fire by pid (Brain.Cell.fire/2 expects pid)
  def fire(pid, amount \\ 0.1) when is_pid(pid), do: Cell.fire(pid, amount)
end

