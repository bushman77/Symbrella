defmodule Core.BrainAdapter do
  @moduledoc """
  Runtime-only bridge to the Brain process (no compile-time deps).
  Talks to the registered process name (Elixir.Brain) via GenServer messages.
  """

  @brain :"Elixir.Brain"
  @timeout 2_000

  @type cell_id :: binary()
  @type cell_item :: cell_id | map() | struct()

  @doc """
  Activate a list of cells (rows, maps with :id, or ids). Payload is optional.
  Fire-and-forget cast; returns :ok.
  """
  @spec activate_cells([cell_item()], map()) :: :ok
  def activate_cells(items, payload \\ %{})
  def activate_cells([], _payload), do: :ok
  def activate_cells(items, payload) when is_list(items) and is_map(payload) do
    GenServer.cast(@brain, {:activate_cells, items, payload})
  end

  @doc "Fetch a full snapshot of Brain state."
  @spec snapshot() :: map()
  def snapshot, do: GenServer.call(@brain, :snapshot, @timeout)

  @doc "Call a specific neuron by id with a request (routed by Brain)."
  @spec cell_call(cell_id(), term()) :: term()
  def cell_call(id, req), do: GenServer.call(@brain, {:cell, id, req}, @timeout)

  @doc "Cast a message to a specific neuron by id."
  @spec cell_cast(cell_id(), term()) :: :ok
  def cell_cast(id, msg), do: GenServer.cast(@brain, {:cell, id, msg})
end

