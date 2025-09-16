defmodule Brain do
  use GenServer
  require Logger

  @name __MODULE__
  @history_max 50

  alias Core
  alias Db
  alias Db.BrainCell
  alias Brain.{Detector, MWEGuard, Index}

  # ——— Public API ———

  def start_link(_args), do: GenServer.start_link(__MODULE__, :ok, name: @name)
  def chat(text) when is_binary(text), do: GenServer.call(@name, {:chat, text})
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

# --- Public API (add near your other public functions) ---
@doc """
Fetch a single active brain cell by id from current state.

Returns:
  - {:ok, %Db.BrainCell{}}
  - {:error, :not_active}   # id not in the current active set
"""
@spec fetch_active_cell(String.t()) :: {:ok, Db.BrainCell.t()} | {:error, :not_active}
def fetch_active_cell(id) when is_binary(id),
  do: GenServer.call(__MODULE__, {:fetch_active_cell, id})

# --- GenServer handler (add with your other handle_call clauses) ---
@impl true
def handle_call({:fetch_active_cell, id}, _from, state) do
  case state.active_cells do
    %{^id => %{cell: %Db.BrainCell{} = cell}} ->
      {:reply, {:ok, cell}, state}

    _ ->
      {:reply, {:error, :not_active}, state}
  end
end

  @impl true
  def handle_call({:chat, text}, _from, state) do
    # 1) Tokenize pure user input
    si0 = Core.resolve_input(text, :console)
    tokens = Map.get(si0, :tokens, [])
    token_count = length(tokens)

    # 2) Braincell detector (running processes from Registry)
    running_ids = Detector.running_ids_for_tokens(tokens)

    # 3) Parse DB for any matching norms
    norms =
      tokens
      |> Enum.map(& &1.phrase)
      |> Enum.map(&normize/1)
      |> Enum.uniq()

    db_cells = Db.Lexicon.fetch_by_norms(norms)

    # Also fetch DB rows for running ids not already loaded
    db_ids = MapSet.new(Enum.map(db_cells, & &1.id))
    missing_running =
      running_ids
      |> Enum.reject(&MapSet.member?(db_ids, &1))

    running_cells =
      case missing_running do
        [] -> []
        ids -> Db.Lexicon.fetch_by_ids(ids)
      end

    cells = db_cells ++ running_cells

    # 4) NegCache guard for MWE remote probes (no remote call here)
    {guarded_mwe, pending_mwe} = MWEGuard.partition_missing_mwes(tokens, db_cells)

    # Merge active set
    state1 = Index.merge_active(state, cells, running_ids)

    # History
    at_ms = System.system_time(:millisecond)

    entry = %{
      text: text,
      token_count: token_count,
      active_ids: state1.active_cell_ids,
      at_ms: at_ms
    }

    history = [entry | state1.history] |> Enum.take(@history_max)
    token_counts = Map.update(state1.token_counts, token_count, 1, &(&1 + 1))

    new_state =
      state1
      |> Map.put(:history, history)
      |> Map.put(:token_counts, token_counts)
      |> Map.update!(:turn_seq, &(&1 + 1))

    # Enrich SI with stage outputs
    si =
      si0
      |> Map.put(:running_cell_ids, running_ids)
      |> Map.put(:db_cells, cells)
      |> Map.put(:negcache_guarded, guarded_mwe)
      |> Map.put(:mwe_pending, pending_mwe)

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

  defp normize(p) do
    p
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/\s+/, " ")
  end
end
