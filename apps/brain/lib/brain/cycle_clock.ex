defmodule Brain.CycleClock do
  @moduledoc """
  CycleClock: emits `[:brain, :cycle, :tick]` telemetry on a steady interval so
  regions can coordinate time-based behavior (decay, scheduling, HUD refresh).

  ## Telemetry
  Event:       [:brain, :cycle, :tick]
  Measurements %{dt_ms, hz, seq}
  Metadata:    %{interval_ms, phase}

  Configure via `config :brain, Brain.CycleClock, hz: 20`
  """

  use GenServer
  @default_hz 20

  # ────────────────────────────────────────────────────────────────────────────
  # Public API (read-only helpers; no process starts here)
  # ────────────────────────────────────────────────────────────────────────────

  def start_link(opts \\ []),
    do: GenServer.start_link(__MODULE__, opts, name: __MODULE__)

  @doc """
  Return `{:ok, pid}` if the clock is running, else `{:error, :not_started}`.

  Note: This does NOT start the clock (root supervisor does that).
  """
  @spec ensure_started() :: {:ok, pid()} | {:error, :not_started}
  def ensure_started do
    case Process.whereis(__MODULE__) do
      pid when is_pid(pid) -> {:ok, pid}
      _ -> {:error, :not_started}
    end
  end

  @doc """
  Read a snapshot of the clock state. Safe if the process isn't started.
  Returns a map with keys: :hz, :interval_ms, :seq, :last_ts, :dt_ms, :phase
  """
  @spec snapshot() :: map()
  def snapshot do
    case Process.whereis(__MODULE__) do
      pid when is_pid(pid) ->
        GenServer.call(__MODULE__, :snapshot)

      _ ->
        hz = configured_or_default_hz()

        %{
          hz: hz,
          interval_ms: max(trunc(1000 / hz), 1),
          seq: 0,
          last_ts: System.monotonic_time(:millisecond),
          dt_ms: 0,
          phase: 0.0
        }
    end
  end

  @doc "Current target frequency (Hz), or configured default if not started."
  def hz, do: snapshot().hz

  @doc "Planned tick interval in ms, or derived from configured default if not started."
  def interval_ms, do: snapshot().interval_ms

  @doc "Tick sequence counter (starts at 0)."
  def seq, do: snapshot().seq

  @doc "Fractional phase in [0.0, 1.0) within a 1s window."
  def phase, do: snapshot().phase

  # ────────────────────────────────────────────────────────────────────────────
  # GenServer
  # ────────────────────────────────────────────────────────────────────────────

  @impl true
  def init(opts) do
    cfg = Application.get_env(:brain, __MODULE__, [])
    hz = opts[:hz] || cfg[:hz] || @default_hz
    hz = if hz <= 0, do: @default_hz, else: hz

    interval_ms = max(trunc(1000 / hz), 1)
    now = System.monotonic_time(:millisecond)

    state = %{hz: hz, interval_ms: interval_ms, last_ts: now, seq: 0}
    Process.send_after(self(), :tick, interval_ms)
    {:ok, state}
  end

  @impl true
  def handle_info(:tick, %{hz: hz, interval_ms: interval, last_ts: last, seq: seq} = st) do
    now = System.monotonic_time(:millisecond)
    dt = max(now - last, 0)
    seq = seq + 1
    phase = rem(seq, hz) / hz

    :telemetry.execute(
      [:brain, :cycle, :tick],
      %{dt_ms: dt, hz: hz, seq: seq},
      %{interval_ms: interval, phase: phase}
    )

    Process.send_after(self(), :tick, interval)
    {:noreply, %{st | last_ts: now, seq: seq}}
  end

  @impl true
  def handle_call(
        :snapshot,
        _from,
        %{hz: hz, interval_ms: interval, last_ts: last, seq: seq} = st
      ) do
    now = System.monotonic_time(:millisecond)
    dt = max(now - last, 0)
    phase = if hz > 0, do: rem(seq, hz) / hz, else: 0.0

    reply = %{
      hz: hz,
      interval_ms: interval,
      seq: seq,
      last_ts: last,
      dt_ms: dt,
      phase: phase
    }

    {:reply, reply, st}
  end

  # ────────────────────────────────────────────────────────────────────────────
  # Internals
  # ────────────────────────────────────────────────────────────────────────────

  defp configured_or_default_hz do
    case Application.get_env(:brain, __MODULE__, [])[:hz] do
      nil -> @default_hz
      x when is_integer(x) and x > 0 -> x
      _ -> @default_hz
    end
  end
end
