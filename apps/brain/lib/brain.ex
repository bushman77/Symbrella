defmodule Brain do
  use GenServer
  require Logger

  @name __MODULE__
  @history_max 50

  # ——— Public API ———

  def start_link(_args), do: GenServer.start_link(__MODULE__, :ok, name: @name)

  # ——— GenServer ———

  @impl true
  def init(:ok) do
    {:ok,
     %{
       history: [],
       active_cells: %{},
       attention: MapSet.new(),
       activation_log: [],
     }}
  end

  def handle_cast({:activate_cells, cells}, state) do
   
    {:noreply, state}
  end

  def handle_call({:stm, si}, _from, state) do
    Map.keys(state.active_cells)
    |> case do
      [] -> {:reply, si, state}
      _ -> 
        {:replay,
          %{si| active_cells: state.active_cells
        }, 
        state}
    end
  end

  def handle_call(:snapahot, _from, state) do
    {:reply, state, state}
  end

end
