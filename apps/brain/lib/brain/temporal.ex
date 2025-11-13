defmodule Brain.Temporal do
  @moduledoc """
  Temporal cortex service (stub).

  Purpose (v0):
  - Maintain a small, rolling window of recent context items (tokens/events).
  - Offer a cheap snapshot via `status/0` for UI/diagnostics.
  - Emit telemetry on push/prune to observe flow without heavy deps.

  Notes:
  - No DB or external deps (keeps `db ← brain ← core ← web` acyclic).
  - Time-based pruning uses monotonic time; count-based guard enforces an upper bound.

  Telemetry (measurement map always includes `count` after operation):
    [:brain, :temporal, :pushed]
    [:brain, :temporal, :pruned]
    [:brain, :temporal, :status]
  """

  use GenServer

  @type item :: String.t() | map()
  @type ms :: non_neg_integer()

  defmodule Entry do
    @moduledoc false
    defstruct [:item, :at_ms]
  end

  defmodule State do
    @moduledoc false
    defstruct window: [],
              opts: %{
                # max items kept (count guard)
                window_keep: 300,
                # soft TTL (5 min) for time pruning
                half_life_ms: 300_000,
                # max items returned by status
                recall_limit: 50
              },
              last_at_ms: nil
  end

  # -- Public API -------------------------------------------------------------

  @doc """
  Start the Temporal server.

  Options:
    :name          - process name (default: #{__MODULE__})
    :window_keep   - max items to retain (default: 300)
    :half_life_ms  - time-based prune horizon (default: 300_000)
    :recall_limit  - max items returned by `status/0` (default: 50)
  """
  def start_link(opts \\ []) do
    name = Keyword.get(opts, :name, __MODULE__)

    init_opts =
      %{
        window_keep: Keyword.get(opts, :window_keep, 300),
        half_life_ms: Keyword.get(opts, :half_life_ms, 300_000),
        recall_limit: Keyword.get(opts, :recall_limit, 50)
      }

    GenServer.start_link(__MODULE__, init_opts, name: name)
  end

  @doc """
  Push a context item (string or map) into the rolling window.

  Non-blocking cast. Returns :ok immediately.
  """
  @spec push(item()) :: :ok
  def push(item), do: GenServer.cast(__MODULE__, {:push, item})

  @doc """
  Clear the current window.
  """
  @spec clear() :: :ok
  def clear, do: GenServer.cast(__MODULE__, :clear)

  @doc """
  Update runtime options (same keys as start_link/1).
  """
  @spec configure(keyword()) :: :ok
  def configure(opts), do: GenServer.cast(__MODULE__, {:configure, opts})

  @doc """
  Snapshot current state for UI/diagnostics.

  Returns:
    %{
      window_count: non_neg_integer(),
      last_at_ms: integer() | nil,
      opts: map(),
      window: [item()]   # up to recall_limit most-recent-first
    }
  """
  @spec status() :: map()
  def status, do: GenServer.call(__MODULE__, :status, 150)

  # -- GenServer --------------------------------------------------------------

  @impl GenServer
  def init(init_opts) do
    state = %State{opts: init_opts}
    {:ok, state}
  end

  @impl GenServer
  def handle_cast({:push, item}, %State{} = st) do
    now = monotonic_ms()
    entry = %Entry{item: item, at_ms: now}

    st =
      st
      |> put_in([Access.key(:window)], [entry | st.window])
      |> put_in([Access.key(:last_at_ms)], now)
      |> prune_window()

    exec_telemetry([:brain, :temporal, :pushed], %{count: length(st.window)}, %{at_ms: now})
    {:noreply, st}
  end

  @impl GenServer
  def handle_cast(:clear, %State{} = st) do
    st = %State{st | window: [], last_at_ms: nil}
    exec_telemetry([:brain, :temporal, :pruned], %{count: 0}, %{reason: :clear})
    {:noreply, st}
  end

  @impl GenServer
  def handle_cast({:configure, opts}, %State{} = st) do
    merged =
      st.opts
      |> Map.merge(%{
        window_keep: Keyword.get(opts, :window_keep, st.opts.window_keep),
        half_life_ms: Keyword.get(opts, :half_life_ms, st.opts.half_life_ms),
        recall_limit: Keyword.get(opts, :recall_limit, st.opts.recall_limit)
      })

    {:noreply, %State{st | opts: merged} |> prune_window()}
  end

  @impl GenServer
  def handle_call(:status, _from, %State{} = st) do
    limit = st.opts.recall_limit
    window = st.window |> Enum.take(limit) |> Enum.map(& &1.item)

    snapshot = %{
      window_count: length(st.window),
      last_at_ms: st.last_at_ms,
      opts: st.opts,
      window: window
    }

    exec_telemetry([:brain, :temporal, :status], %{count: snapshot.window_count}, %{})
    {:reply, snapshot, st}
  end

  # -- Internals --------------------------------------------------------------

  defp prune_window(%State{} = st) do
    now = monotonic_ms()
    ttl = st.opts.half_life_ms

    # Time-based prune (soft)
    pruned_time =
      Enum.filter(st.window, fn %Entry{at_ms: at} ->
        is_integer(at) and now - at <= ttl
      end)

    # Count-based cap (hard)
    keep = st.opts.window_keep

    final =
      if length(pruned_time) > keep do
        Enum.take(pruned_time, keep)
      else
        pruned_time
      end

    if length(final) != length(st.window) do
      exec_telemetry([:brain, :temporal, :pruned], %{count: length(final)}, %{reason: :prune})
    end

    %State{st | window: final}
  end

  defp monotonic_ms, do: System.monotonic_time(:millisecond)

  defp exec_telemetry(event, measurements, meta) do
    :telemetry.execute(event, measurements, meta)
  end
end
