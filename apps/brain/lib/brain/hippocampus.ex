defmodule Brain.Hippocampus do
  @moduledoc ~S"""
  Hippocampus (episodic memory). Episode write/recall (norms Jaccard + recency).
  API:
    - episode_write(si, ctx) :: {:ok, :stored} (stub)
    - episode_recall(cues) :: [] (stub)
  """
  use GenServer

  @type si :: map()
  @type ctx :: map()

  def start_link(opts \\ []), do: GenServer.start_link(__MODULE__, %{}, Keyword.merge([name: __MODULE__], opts))
  def episode_write(si, ctx \\ %{}), do: GenServer.call(__MODULE__, {:episode_write, si, ctx})
  def episode_recall(cues), do: GenServer.call(__MODULE__, {:episode_recall, cues})

  @impl true
  def init(state), do: {:ok, state}

  @impl true
  def handle_call({:episode_write, _si, _ctx}, _from, state), do: {:reply, {:ok, :stored}, state}
  @impl true
  def handle_call({:episode_recall, _cues}, _from, state), do: {:reply, [], state}
end
