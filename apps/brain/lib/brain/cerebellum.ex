defmodule Brain.Cerebellum do
  @moduledoc ~S"""
  Cerebellum (fast predictor / error smoothing).

  API:
    - predict(seq, ctx) :: {:noop, seq} (stub)
  """
  use GenServer

  @type ctx :: map()

  # --- Public API ---
  def start_link(opts \\ []),
    do: GenServer.start_link(__MODULE__, %{}, Keyword.merge([name: __MODULE__], opts))

  def predict(seq, ctx \\ %{}),
    do: GenServer.call(__MODULE__, {:predict, seq, ctx})

  # --- GenServer callbacks ---
  @impl true
  def init(state), do: {:ok, state}

  @impl true
  def handle_call({:predict, seq, _ctx}, _from, state) do
    {:reply, {:noop, seq}, state}
  end
end

