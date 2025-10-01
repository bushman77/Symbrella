defmodule Brain.Neuromod do
  @moduledoc ~S"""
  Neuromodulation (DA/NE/5-HT tone). Centralized gain control.

  API:
    - modulate(%{da: float, ne: float, "5ht": float}) :: :ok
    - last_tone() :: map
  """

  use GenServer

  @type tone :: %{optional(:da) => float, optional(:ne) => float, optional(:"5ht") => float}

  # --- Public API ---
  def start_link(opts \\ []),
    do: GenServer.start_link(__MODULE__, %{tone: %{}}, Keyword.merge([name: __MODULE__], opts))

  def modulate(tone) when is_map(tone),
    do: GenServer.cast(__MODULE__, {:modulate, tone})

  def last_tone(),
    do: GenServer.call(__MODULE__, :last_tone)

  # --- GenServer callbacks ---
  @impl true
  def init(state), do: {:ok, state}

  @impl true
  def handle_cast({:modulate, tone}, state),
    do: {:noreply, %{state | tone: Map.merge(state.tone, tone)}}

  @impl true
  def handle_call(:last_tone, _from, state),
    do: {:reply, state.tone, state}
end

