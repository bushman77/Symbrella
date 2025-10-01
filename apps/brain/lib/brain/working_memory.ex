
defmodule Brain.WorkingMemory do
  @moduledoc ~S"""
  DLPFC-like working set for focus/priming (Option C: Hydrating Cells).

  State:
    - focus: MapSet of cell ids
    - priming: map id => float boost

  API:
    - focus_put(ids)
    - focus_get()
    - prime(ids, boost)
  """
  use GenServer

  @type id :: String.t()
  @type ids :: [id]

  # --- Public API ---
  def start_link(opts \\ []),
    do: GenServer.start_link(__MODULE__, %{focus: MapSet.new(), priming: %{}}, Keyword.merge([name: __MODULE__], opts))

  def focus_put(ids) when is_list(ids),
    do: GenServer.cast(__MODULE__, {:focus_put, ids})

  def focus_get(),
    do: GenServer.call(__MODULE__, :focus_get)

  def prime(ids, boost \\ 0.5) when is_list(ids) and is_number(boost),
    do: GenServer.cast(__MODULE__, {:prime, ids, boost})

  # --- GenServer callbacks ---
  @impl true
  def init(state), do: {:ok, state}

  @impl true
  def handle_cast({:focus_put, ids}, %{focus: focus} = state) do
    new_focus = Enum.reduce(ids, focus, &MapSet.put(&2, &1))
    {:noreply, %{state | focus: new_focus}}
  end

  @impl true
  def handle_cast({:prime, ids, boost}, state) do
    priming = Enum.reduce(ids, state.priming, fn id, acc -> Map.update(acc, id, boost, &(&1 + boost)) end)
    {:noreply, %{state | priming: priming}}
  end

  @impl true
  def handle_call(:focus_get, _from, state),
    do: {:reply, state.focus, state}
end

