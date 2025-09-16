defmodule Brain do
  use GenServer
  require Logger

  @name __MODULE__
  @history_max 50

  alias Core
  alias Brain.Activator
  alias Brain.Index

  # ——— Public API ———

  def start_link(_args), do: GenServer.start_link(__MODULE__, :ok, name: @name)

  @doc "Blocking chat entrypoint."
  def chat(text) when is_binary(text), do: GenServer.call(@name, {:chat, text})

  @doc "Snapshot of minimal state."
  def snapshot, do: GenServer.call(@name, :snapshot)

  # ——— GenServer ———

  @impl true
  def init(:ok) do
    {:ok,
     %{
       history: [],
       active_cells: %{},
       active_cell_ids: [],
       active_cell_count: 0,
       attention: MapSet.new(),
       activation_log: [],
       token_counts: %{},
       turn_seq: 0
     }}
  end

  @impl true
  def handle_call({:chat, text}, _from, state) do
    si = Core.resolve_input(text, :console)
    tokens = Map.get(si, :tokens, [])
    token_count = length(tokens)

    # 1) Ensure + activate all POS variants for every token (soft activation)
    {cells, started_ids} = Activator.activate_all_pos(tokens, only_mw: false)

    # 2) Merge active set
    state1 = Index.merge_active(state, cells, started_ids)

    # 3) Log history with real wall clock ms
    at_ms = System.system_time(:millisecond)

    entry = %{
      text: text,
      active_ids: state1.active_cell_ids,
      at_ms: at_ms,
      token_count: token_count
    }

    history = [entry | state1.history] |> Enum.take(@history_max)

    token_counts =
      Map.update(state1.token_counts, token_count, 1, &(&1 + 1))

    new_state =
      state1
      |> Map.put(:history, history)
      |> Map.put(:token_counts, token_counts)
      |> Map.update!(:turn_seq, &(&1 + 1))

    {:reply, si, new_state}
  end

  def handle_call(:snapshot, _from, state) do
    {:reply,
     %{
       history: state.history,
       token_counts: state.token_counts,
       turn_seq: state.turn_seq,
       active_cell_count: state.active_cell_count,
       active_cell_ids: state.active_cell_ids
     }, state}
  end
end
