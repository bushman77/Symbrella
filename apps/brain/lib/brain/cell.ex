defmodule Brain.Cell do
  @moduledoc """
  One cell process. Registers under Brain.Registry by its id.
  Accepts {:activate, %{delta: int}} casts and reports back to Brain.
  """

  use GenServer
  alias Db.BrainCell, as: Row

  # ───────────────────────── Public ─────────────────────────

  # Accepts either a %Db.BrainCell{} row or a binary id
  def start_link(arg) do
    id = extract_id(arg)
    GenServer.start_link(__MODULE__, %{id: id, data: arg, activation: 0},
      name: Brain.via(id)
    )
  end

  # ──────────────────────── GenServer ───────────────────────

  @impl true
  def init(state),
    do: {:ok, state}

  @impl true
  def handle_call(:status, _from, %{id: id, activation: a} = st) do
    {:reply, {:ok, %{id: id, activation: a}}, st}
  end

  @impl true
  def handle_cast({:activate, payload}, %{id: id, activation: a} = st) do
    delta = Map.get(payload, :delta, 1)
    a2 = a + delta

    # Report back so Brain.active_cells fills in
    GenServer.cast(Brain, {:activation_report, id, a2})

    {:noreply, %{st | activation: a2}}
  end

  # ──────────────────────── Helpers ─────────────────────────

  defp extract_id(%Row{id: id}), do: id
  defp extract_id(id) when is_binary(id), do: id
  defp extract_id(other), do: raise ArgumentError, "Unsupported cell arg: #{inspect(other)}"
end

