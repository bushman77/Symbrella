# apps/brain/lib/brain/amygdala.ex
defmodule Brain.Amygdala do
  @moduledoc ~S"""
  Amygdala (salience / threat / novelty). Emits salience bumps that bias gates.
  API:
    - salience(signals) :: {:bump, ids, amount}
  Stub: echoes a fixed bump of 0.2 over any ids in signals[:ids].
  """
  use GenServer

  @type signal :: map()

  # --- Public API ---
  def start_link(opts \\ []),
    do: GenServer.start_link(__MODULE__, %{}, Keyword.merge([name: __MODULE__], opts))

  def salience(signals) when is_map(signals),
    do: GenServer.call(__MODULE__, {:salience, signals})

  # --- GenServer callbacks ---
  @impl true
  def init(state), do: {:ok, state}

  @impl true
  def handle_call({:salience, %{ids: ids}}, _from, state) when is_list(ids) do
    {:reply, {:bump, ids, 0.2}, state}
  end

  @impl true
  def handle_call({:salience, _}, _from, state),
    do: {:reply, {:bump, [], 0.0}, state}
end

