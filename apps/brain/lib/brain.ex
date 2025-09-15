defmodule Brain do
  @moduledoc false
  use GenServer
  require Logger

  @name __MODULE__
  @registry Brain.Registry
  @history_max 50
  @activation_log_max 100

  # ───────────── Public API ─────────────

  def start_link(_args),
    do: GenServer.start_link(__MODULE__, :ok, name: @name)

  @doc "Blocking chat entrypoint."
  def chat(text) when is_binary(text),
    do: GenServer.call(@name, {:chat, text})

  @doc "Minimal state snapshot + active cells summary."
  def snapshot,
    do: GenServer.call(@name, :snapshot)

  @doc "List of active cell ids (registry-first, falls back to state map)."
  def active_list,
    do: GenServer.call(@name, :active_list)

  @doc "Register a running cell (id -> pid)."
  def register_active(id, pid) when is_binary(id) and is_pid(pid),
    do: GenServer.cast(@name, {:register_active, id, pid})

  @doc "Optional: register an activation event (kept light)."
  def register_activation(id) when is_binary(id),
    do: GenServer.cast(@name, {:activation, id, now_ms()})

  # ——— inspection helpers ———

  @doc "Return the full state for a running cell id, or nil if not running."
  def cell(id) when is_binary(id),
    do: GenServer.call(@name, {:cell, id})

  @doc "Return states for a list of cell ids. Keeps input order; filters out missing."
  def cells(ids) when is_list(ids),
    do: GenServer.call(@name, {:cells, ids})

  @doc "Fetch all RUNNING cells for a given word (e.g., \"hello\" → all hello|POS|i, and \"w:hello\")."
  def cells_for_word(word) when is_binary(word),
    do: GenServer.call(@name, {:cells_for_word, word})

  @doc """
  Peek selected fields from a running cell (default: runtime-relevant).
  Returns a map or :not_running.
  """
  def peek(
        id,
        fields \\ [
          :id,
          :word,
          :pos,
          :type,
          :activation,
          :modulated_activation,
          :dopamine,
          :serotonin,
          :last_substance,
          :last_dose_at
        ]
      )
      when is_binary(id) and is_list(fields),
      do: GenServer.call(@name, {:peek, id, fields})

  @doc "Kill a single cell by id (if running)."
  def kill(id) when is_binary(id) do
    case Registry.lookup(@registry, id) do
      [{pid, _}] -> Process.exit(pid, :normal); :ok
      _ -> :not_found
    end
  end

  @doc "Stop all runtime fallback cells (ids like \"w:*\")."
  def prune_runtime do
    ids = registry_keys()
    Enum.each(ids, fn id ->
      if String.starts_with?(id, "w:") do
        kill(id)
      end
    end)

    :ok
  end

  # ───────────── GenServer ─────────────

  @impl true
  def init(:ok) do
    {:ok,
     %{
       attention: MapSet.new(),
       activation_log: [],
       active_cells: %{},          # id => %{pid, ref, since}
       llm_ctx: nil,
       llm_model: nil,
       llm_ctx_updated_at: nil,
       llm_max: 8192,
       turn_seq: 0,
       history: [],
       token_counts: %{}
     }}
  end

  @impl true
  def handle_call({:chat, raw_text}, _from, state) do
    si  = Core.resolve_input(raw_text)
    # prefer sense fan-out; skip MWE fallback by default to avoid w:"hello there"
    si2 = Brain.Activator.run(si, impulse: 0.3, mwe_fallback: :skip)

    tokens = si2.tokens || []

    words =
      tokens
      |> Enum.map(& &1.phrase)
      |> Enum.filter(&is_binary/1)

    token_counts =
      Enum.reduce(words, state.token_counts, fn w, acc -> Map.update(acc, w, 1, &(&1 + 1)) end)

    act_ids =
      si2.active_cells
      |> Enum.map(fn
        %{id: id} -> id
        other -> other
      end)

    entry = %{
      text: raw_text,
      active_ids: act_ids,
      at_ms: now_ms(),
      token_count: length(tokens)
    }

    hist = [entry | state.history] |> Enum.take(@history_max)

    {:reply, "reply",
     %{
       state
       | history: hist,
         token_counts: token_counts,
         turn_seq: state.turn_seq + 1
     }}
  end

  @impl true
  def handle_call(:snapshot, _from, state) do
    # union of registry-derived ids and internal map (belt & suspenders)
    reg_ids = registry_keys()
    map_ids = Map.keys(state.active_cells)
    active_ids = (reg_ids ++ map_ids) |> Enum.uniq()

    view = %{
      history: Enum.take(state.history, 5),
      token_counts: state.token_counts,
      turn_seq: state.turn_seq,
      active_cell_count: length(active_ids),
      active_cell_ids: active_ids
    }

    {:reply, view, state}
  end

  @impl true
  def handle_call(:active_list, _from, state) do
    reg_ids = registry_keys()
    map_ids = Map.keys(state.active_cells)
    {:reply, (reg_ids ++ map_ids) |> Enum.uniq(), state}
  end

  # ——— runtime cell inspection calls ———

  @impl true
  def handle_call({:cell, id}, _from, state) do
    {:reply, fetch_cell_state(id), state}
  end

  @impl true
  def handle_call({:cells, ids}, _from, state) do
    states =
      ids
      |> Enum.map(&fetch_cell_state/1)
      |> Enum.filter(& &1)

    {:reply, states, state}
  end

  @impl true
  def handle_call({:cells_for_word, word}, _from, state) do
    ids =
      registry_keys()
      |> Enum.filter(&id_matches_word?(&1, word))

    states =
      ids
      |> Enum.map(&fetch_cell_state/1)
      |> Enum.filter(& &1)

    {:reply, states, state}
  end

  @impl true
  def handle_call({:peek, id, fields}, _from, state) do
    case fetch_cell_state(id) do
      nil -> {:reply, :not_running, state}
      struct -> {:reply, Map.take(Map.from_struct(struct), fields), state}
    end
  end

  @impl true
  def handle_cast({:register_active, id, pid}, state) do
    ref = Process.monitor(pid)
    entry = %{pid: pid, ref: ref, since: now_ms()}
    {:noreply, put_in(state.active_cells[id], entry)}
  end

  @impl true
  def handle_cast({:activation, id, ts}, state) do
    new_log =
      [%{id: id, at: ts} | state.activation_log]
      |> Enum.take(@activation_log_max)

    {:noreply, %{state | activation_log: new_log}}
  end

  @impl true
  def handle_info({:cell_started, {id, pid}}, state)
      when is_binary(id) and is_pid(pid) do
    ref = Process.monitor(pid)
    entry = %{pid: pid, ref: ref, since: now_ms()}
    {:noreply, put_in(state.active_cells[id], entry)}
  end

  @impl true
  def handle_info({:DOWN, ref, :process, _pid, _reason}, state) do
    {dead_id, _} =
      Enum.find(state.active_cells, fn {_id, m} -> match?(%{ref: ^ref}, m) end) || {nil, nil}

    active =
      if dead_id, do: Map.delete(state.active_cells, dead_id), else: state.active_cells

    {:noreply, %{state | active_cells: active}}
  end

  @impl true
  def handle_info(msg, state) do
    Logger.debug("Brain ignored message: #{inspect(msg)}")
    {:noreply, state}
  end

  # ───────────── Helpers ─────────────

  defp fetch_cell_state(id) when is_binary(id) do
    case Registry.lookup(@registry, id) do
      [{pid, _}] -> Brain.Cell.get_state(pid)   # returns %Db.BrainCell{}
      _ -> nil
    end
  end

  # Matches "hello|POS|n" or "w:hello"
  defp id_matches_word?(id, word) do
    String.starts_with?(id, word <> "|") or id == "w:" <> word
  end

  defp registry_keys do
    try do
      Registry.select(@registry, [{{{:"$1", :_, :_}, :_}, [], [:"$1"]}])
    rescue
      _ -> []
    catch
      _, _ -> []
    end
  end

  defp now_ms do
    try do
      System.monotonic_time(:millisecond)
    rescue
      _ -> System.system_time(:millisecond)
    end
  end
end

