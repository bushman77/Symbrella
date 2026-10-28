defmodule Core.Discourse.Ledger.Owner do
  @moduledoc """
  Serialized owner of conversation-scoped semantic ledger records.

  ETS is protected: readers can inspect committed snapshots, but only this
  process can append, trim, or reset a session. No raw transcripts, LIFG
  slates, system prompts, or conversation-wide cognition traces are stored.
  This first version is intentionally in-memory, not restart-persistent.
  """
  use GenServer

  @table :core_discourse_ledger
  @default_keep 4096
  @maximum_keep 16_384

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(_opts) do
    :ets.new(@table, [:named_table, :protected, :set, {:read_concurrency, true}])
    {:ok, %{}}
  end

  @spec append(String.t(), map()) :: {:ok, pos_integer()} | {:error, atom()}
  def append(session, record) when is_binary(session) and is_map(record) do
    GenServer.call(__MODULE__, {:append, session, record})
  end

  @spec entries(String.t()) :: [map()]
  def entries(session) when is_binary(session) do
    case :ets.lookup(@table, session) do
      [{^session, _last_seq, entries}] when is_list(entries) -> entries
      _ -> []
    end
  rescue
    ArgumentError -> []
  end

  @doc "Clear only the specified session, for explicit reset and isolated tests."
  @spec clear(String.t()) :: :ok
  def clear(session) when is_binary(session) do
    GenServer.call(__MODULE__, {:clear, session})
  end

  @impl true
  def handle_call({:append, session, record}, _from, state) do
    {sequence, previous} =
      case :ets.lookup(@table, session) do
        [{^session, last, existing}] -> {last + 1, existing}
        [] -> {1, []}
      end

    # Newest-first. Keep the per-session sequence monotonic even after trim.
    entry = Map.put(record, :seq, sequence)
    keep = retention()
    entries = [entry | Enum.take(previous, keep - 1)]
    true = :ets.insert(@table, {session, sequence, entries})
    {:reply, {:ok, sequence}, state}
  end

  @impl true
  def handle_call({:clear, session}, _from, state) do
    :ets.delete(@table, session)
    {:reply, :ok, state}
  end

  defp retention do
    case Application.get_env(:core, Core.Discourse.Ledger, []) do
      config when is_list(config) ->
        case Keyword.get(config, :max_entries_per_session, @default_keep) do
          count when is_integer(count) and count > 0 ->
            min(count, @maximum_keep)
          _ -> @default_keep
        end

      _ -> @default_keep
    end
  end
end
