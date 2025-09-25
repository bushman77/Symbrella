defmodule Brain do
  use GenServer
  require Logger
  alias Db.BrainCell, as: Row

  @name __MODULE__
  @cell_timeout 2_000

  # ——— Public API (started by Symbrella.Application) ———
  def start_link(_args), do: GenServer.start_link(__MODULE__, :ok, name: @name)

  # ——— GenServer ———

  @impl true
  def init(:ok) do
    {:ok,
     %{
       history: [],
       active_cells: %{},
       attention: MapSet.new(),
       activation_log: []
     }}
  end

  # ——— handle_call ———

  @impl true
  def handle_call({:stm, si}, _from, state) do
    reply_si =
      if map_size(state.active_cells) == 0 do
        si
      else
        %{si | active_cells: state.active_cells}
      end

    {:reply, reply_si, state}
  end

  @impl true
  def handle_call(:snapshot, _from, state), do: {:reply, state, state}

  # Optional: route a call to a single cell
  @impl true
  def handle_call({:cell, id, req}, _from, state) do
    case Registry.lookup(Brain.Registry, id) do
      [{pid, _}] -> {:reply, GenServer.call(pid, req, @cell_timeout), state}
      [] -> {:reply, {:error, :not_found}, state}
    end
  end

  # ——— handle_cast (grouped to avoid warnings) ———

  @impl true
  def handle_cast({:activate_cells, items, payload}, state) do
    Enum.each(items, &ensure_start_and_cast(&1, payload))
    {:noreply, state}
  end

  @impl true
  def handle_cast({:activation_report, id, a}, state) do
    {:noreply, %{state | active_cells: Map.put(state.active_cells, id, a)}}
  end

  # Optional: route a cast to a single cell
  @impl true
  def handle_cast({:cell, id, msg}, state) do
    case Registry.lookup(Brain.Registry, id) do
      [{pid, _}] -> GenServer.cast(pid, msg)
      [] -> :ok
    end
    {:noreply, state}
  end

  # ——— Helpers ———

  # Accepts a DB row…
  defp ensure_start_and_cast(%Row{id: id} = row, payload) do
    ensure_started(id, row)
    GenServer.cast(via(id), {:activate, payload})
  end

  # …or just an id (if your cell supports lazy hydration)
  defp ensure_start_and_cast(id, payload) when is_binary(id) do
    ensure_started(id, id)
    GenServer.cast(via(id), {:activate, payload})
  end

  defp ensure_started(id, arg) do
    case Registry.lookup(Brain.Registry, id) do
      [] ->
        case DynamicSupervisor.start_child(Brain.CellSup, {Brain.Cell, arg}) do
          {:ok, _pid} -> :ok
          {:error, {:already_started, _pid}} -> :ok
          other -> Logger.warning("Brain: start_child(#{id}) -> #{inspect(other)}")
        end

      _ ->
        :ok
    end
  end

  defp via(id), do: {:via, Registry, {Brain.Registry, id}}
end

