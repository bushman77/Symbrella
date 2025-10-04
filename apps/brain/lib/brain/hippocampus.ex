defmodule Brain.Hippocampus do
  @moduledoc """
  Hippocampus — episodic buffer (stub).

  • `encode/1` (no-op pass-through) ingests the current slate into a rolling window.
  • `snapshot/0` returns server state for observability.
  • `recall/2` is a safe stub (returns [] for now).
  • `configure/1` updates runtime opts (e.g., :window_keep).

  This is intentionally minimal; wire up Db-backed persistence/recall later.
  """

  # Brings in start_link/1, child_spec/1, status/1, etc.
  use Brain, region: :hippocampus
  @name __MODULE__

  @type episode :: %{
          ts_ms: non_neg_integer(),
          slate: map(),
          meta: map()
        }

  # ───────────── Public API (pure-ish) ─────────────

  @doc """
  Encode (ingest) a slate and return it unchanged (pass-through).

  Safe if the server isn't running: simply returns `slate`.
  """
  @spec encode(map()) :: map()
  def encode(%{} = slate), do: encode(slate, %{})

  @doc """
  Encode with meta. Persists to the rolling window when server is up.
  """
  @spec encode(map(), map()) :: map()
  def encode(%{} = slate, %{} = meta) do
    case Process.whereis(@name) do
      nil -> slate
      _pid ->
        # best-effort; ignore failures
        _ = GenServer.call(@name, {:ingest, slate, meta})
        slate
    end
  end

  @doc "Peek current Hippocampus state (window, last)."
  @spec snapshot() :: map()
  def snapshot, do: GenServer.call(@name, :snapshot)

  @doc "Update runtime options (e.g., `window_keep`)."
  @spec configure(keyword()) :: :ok
  def configure(opts) when is_list(opts), do: GenServer.cast(@name, {:configure, opts})

  @doc "Clear window/last."
  @spec reset() :: :ok
  def reset, do: GenServer.call(@name, :reset)

  @doc """
  Stub recall: returns an empty list for now.

  Later: accept cues (lemmas, ids, features) and query real storage.
  """
  @spec recall(any(), keyword()) :: []
  def recall(_cues, _opts \\ []), do: []

  # ───────────── GenServer callbacks/state ─────────────

@impl true
def init(opts) do
  # accept :window_keep or :keep (alias), with env fallback
  keep =
    Keyword.get(opts, :window_keep,
      Keyword.get(opts, :keep, Application.get_env(:brain, :hippo_window_keep, 200))
    )

  {:ok,
   %{
     region: :hippocampus,
     opts: Map.merge(%{window_keep: keep}, Map.new(opts)),  # reflect resolved value
     window_keep: keep,
     window: [],
     last: nil
   }}
end

@impl true
def handle_cast({:configure, opts}, state) do
  # same aliasing on configure
  keep = Keyword.get(opts, :window_keep, Keyword.get(opts, :keep, state.window_keep))

  {:noreply,
   state
   |> Map.put(:window_keep, keep)
   |> Map.update!(:opts, &Map.merge(&1, %{window_keep: keep} |> Map.merge(Map.new(opts))))}
end


  @impl true
  def handle_call({:ingest, slate, meta}, _from, state) when is_map(slate) and is_map(meta) do
    ts = System.system_time(:millisecond)
    ep = %{ts_ms: ts, slate: slate, meta: meta}

    :telemetry.execute([:brain, :hippocampus, :ingest], %{count: 1}, %{ts_ms: ts})

    window =
      [{ts, ep} | state.window]
      |> Enum.take(state.window_keep)

    {:reply, :ok, %{state | window: window, last: ep}}
  end

  @impl true
  def handle_call(:snapshot, _from, state), do: {:reply, state, state}

  @impl true
  def handle_call(:reset, _from, state) do
    {:reply, :ok, %{state | last: nil, window: []}}
  end
end

