defmodule Brain do
  @moduledoc """
  Central coordinator for cells. Keeps a lightweight STM view (active cells, attention, history).
  Started by your application supervisor.
  """

  use GenServer
  require Logger
  alias Db.BrainCell, as: Row

  @name __MODULE__
  @cell_timeout 2_000

  # ─────────────────────────── Public API ───────────────────────────

  def start_link(_args), do: GenServer.start_link(__MODULE__, :ok, name: @name)

  @doc "Blocking: short-term memory snapshot merge."
  def stm(si) when is_map(si), do: GenServer.call(@name, {:stm, si})

  @doc "Non-blocking: activate a batch of rows or ids (payload e.g., %{delta: 1})."
  def activate_cells(rows_or_ids, payload \\ %{delta: 1}),
    do: GenServer.cast(@name, {:activate_cells, rows_or_ids, payload})

  @doc "Optional: route a call to a single cell (e.g., {:status})."
  def cell_status(id), do: GenServer.call(@name, {:cell, id, :status})

  @doc "Optional: route a cast to a single cell (e.g., :activate, :stop)."
  def cell_cast(id, msg), do: GenServer.cast(@name, {:cell, id, msg})

  @doc "State snapshot (debug/testing)."
  def snapshot, do: GenServer.call(@name, :snapshot)

  @doc "Registry via tuple for a cell id."
  def via(id), do: {:via, Registry, {Brain.Registry, id}}

  # ─────────────────────────── GenServer ────────────────────────────

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
    # Always surface current active_cells into the SI
    {:reply, Map.put(si, :active_cells, state.active_cells), state}
  end

  @impl true
  def handle_call(:snapshot, _from, state), do: {:reply, state, state}

  @impl true
  def handle_call({:cell, id, req}, _from, state) do
    case Registry.lookup(Brain.Registry, id) do
      [{pid, _}] -> {:reply, GenServer.call(pid, req, @cell_timeout), state}
      [] -> {:reply, {:error, :not_found}, state}
    end
  end

  # ——— handle_cast ———

  # Accept a list of DB rows or ids
  @impl true
  def handle_cast({:activate_cells, rows_or_ids, payload}, state) when is_list(rows_or_ids) do
    Enum.each(rows_or_ids, fn
      %Row{} = row -> ensure_start_and_cast(row, payload)
      id when is_binary(id) -> ensure_start_and_cast(id, payload)
      other -> Logger.warning("Brain.activate_cells: unknown item #{inspect(other)}")
    end)

    {:noreply, state}
  end

  # Accept a SemanticInput with :db_cells list
  @impl true
  def handle_cast({:activate_cells, %{} = si, payload}, state) do
    rows = Map.get(si, :db_cells, [])
    Enum.each(rows, fn
      %Row{} = row -> ensure_start_and_cast(row, payload)
      id when is_binary(id) -> ensure_start_and_cast(id, payload)
      other -> Logger.warning("Brain.activate_cells(si): unknown item #{inspect(other)}")
    end)

    {:noreply, state}
  end

  @impl true
  def handle_cast({:activation_report, id, a}, state) when is_binary(id) and is_number(a) do
    {:noreply, %{state | active_cells: Map.put(state.active_cells, id, a)}}
  end

  @impl true
  def handle_cast({:cell, id, msg}, state) do
    case Registry.lookup(Brain.Registry, id) do
      [{pid, _}] -> GenServer.cast(pid, msg)
      [] -> :ok
    end

    {:noreply, state}
  end

  # ─────────────────────────── Helpers ──────────────────────────────

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
end

