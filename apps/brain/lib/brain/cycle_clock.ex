defmodule Brain.CycleClock do
  @moduledoc """
  Global, lightweight phase clock that emits a repeating cycle and telemetry.

  • Phases default to [:internal, :external, :integrate, :idle]
  • Tick interval defaults to 750 ms (≈1.33 Hz), configurable via env:
      config :brain, Brain.CycleClock, phases: [:internal, :external, :integrate, :idle], tick_ms: 750
  • Telemetry on each tick:
      event:    [:brain, :cycle, :tick]
      measures: %{i: non_neg_integer(), dt_ms: float, hz: float}
      meta:     %{phase: atom()}
  • Self-starting: any public call ensures the clock is running (no supervisor change required).
  """

  use GenServer

  @name __MODULE__
  @phases_default [:internal, :external, :integrate, :idle]
  @tick_ms_default 750

  # --------- Public API ---------

  @doc "Ensure the clock is running (idempotent)."
  def ensure_started do
    case Process.whereis(@name) do
      nil  -> GenServer.start(@name, %{}, name: @name)
      _pid -> :ok
    end
  end

  @doc "Current phase atom (starts the clock if needed)."
  def phase do
    ensure_started()
    :persistent_term.get({@name, :phase}, :idle)
  end

  @doc "All configured phases."
  def phases do
    ensure_started()
    :persistent_term.get({@name, :phases}, @phases_default)
  end

  @doc "Approximate current cycle frequency in Hz (EWMA)."
  def hz do
    ensure_started()
    :persistent_term.get({@name, :hz}, 0.0)
  end

  @doc "Manually advance one tick (useful for tests)."
  def tick, do: send(@name, :tick)

  # --------- GenServer ---------

  @impl true
  def start(_, _opts \\ []) do
    GenServer.start(__MODULE__, %{}, name: @name)
  end

  @impl true
  def start_link(_opts), do: GenServer.start_link(__MODULE__, %{}, name: @name)

  @impl true
  def init(_) do
    cfg     = Application.get_env(:brain, __MODULE__, [])
    phases  = Keyword.get(cfg, :phases, @phases_default)
    tick_ms = max(10, Keyword.get(cfg, :tick_ms, @tick_ms_default))

    put(:phases, phases)
    put(:phase, :idle)
    put(:hz, 0.0)

    now = System.monotonic_time(:millisecond)
    :timer.send_interval(tick_ms, :tick)
    {:ok, %{i: 0, last_ms: now, tick_ms: tick_ms, phases: phases, ewma_ms: tick_ms * 1.0}}
  end

  @impl true
  def handle_info(:tick, %{i: i, last_ms: last, tick_ms: tick_ms, phases: phases, ewma_ms: ewma} = st) do
    now   = System.monotonic_time(:millisecond)
    dt    = max(1.0, now - last)
    # EWMA of period
    alpha = 0.2
    ewma2 = (1.0 - alpha) * ewma + alpha * dt
    hz    = 1000.0 / ewma2

    idx   = rem(i, length(phases))
    phase = Enum.at(phases, idx)

    put(:phase, phase)
    put(:hz, hz)

    :telemetry.execute([:brain, :cycle, :tick], %{i: i, dt_ms: dt * 1.0, hz: hz}, %{phase: phase})
    {:noreply, %{st | i: i + 1, last_ms: now, ewma_ms: ewma2}}
  end

  # --------- internals ---------
  defp put(key, val), do: :persistent_term.put({@name, key}, val)
end

