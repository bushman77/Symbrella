defmodule Brain.Thalamus do
  @moduledoc ~S"""
  Thalamus (router). Centralizes topic/payload routing between brain regions.

  API:
    - route(topic, payload, ctx) :: :ok

  Stub: No actual fanout; returns :ok.
  """

  use GenServer

  @type ctx :: map()

  # --- Public API ---
  def start_link(opts \\ []),
    do: GenServer.start_link(__MODULE__, %{}, Keyword.merge([name: __MODULE__], opts))

  def route(topic, payload, ctx \\ %{}),
    do: GenServer.cast(__MODULE__, {:route, topic, payload, ctx})

  # --- GenServer callbacks ---
  @impl true
  def init(state), do: {:ok, state}

  @impl true
  def handle_cast({:route, _topic, _payload, _ctx}, state) do
    # TODO: forward to subscribers once subscription model is in
    {:noreply, state}
  end
end

