defmodule Brain.BasalGanglia do
  @moduledoc ~S"""
  Basal Ganglia (action/sense gating & selection).

  API:
    - gate(channel, value) :: :allow | :suppress
    - select(candidates, ctx) :: {winner, scores}
  """
  use GenServer

  @type candidate :: any()
  @type ctx :: map()

  # --- Public API ---
  def start_link(opts \\ []),
    do: GenServer.start_link(__MODULE__, %{}, Keyword.merge([name: __MODULE__], opts))

  def gate(channel, value),
    do: GenServer.call(__MODULE__, {:gate, channel, value})

  def select(candidates, ctx \\ %{}) when is_list(candidates),
    do: GenServer.call(__MODULE__, {:select, candidates, ctx})

  # --- GenServer callbacks ---
  @impl true
  def init(state), do: {:ok, state}

  @impl true
  def handle_call({:gate, _channel, _value}, _from, state),
    do: {:reply, :allow, state}

  @impl true
  def handle_call({:select, candidates, _ctx}, _from, state) do
    winner = List.first(candidates)
    scores = Enum.with_index(candidates, fn c, i -> {c, 1.0 - i * 0.01} end)
    {:reply, {winner, scores}, state}
  end
end

