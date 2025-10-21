defmodule Brain.MoodCore do
  @moduledoc """
  MoodCore: neuromodulator state with half-life decay, clock-coupled via telemetry.

  Keys:
    :da   (dopamine)
    :"5ht"(serotonin)
    :glu  (glutamate)
    :ne   (norepinephrine)

  Emits `[:brain, :mood, :update]` with raw levels and derived indices:
  :exploration, :inhibition, :vigilance, :plasticity.
  """

  use GenServer
  @ln2 :math.log(2)
  @event [:brain, :mood, :update]

  # Public API
  def start_link(_opts \\ []), do: GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
  def snapshot(),              do: GenServer.call(__MODULE__, :snapshot)
  def bump(deltas),            do: GenServer.cast(__MODULE__, {:bump, deltas})

  @doc """
  Subscribe current process to mood updates. Receives:
      {:mood_update, measurements_map, meta_map}
  """
  def subscribe() do
    id = "moodcore-subscriber-#{System.unique_integer([:positive])}"
    :telemetry.attach(id, @event, &__MODULE__.handle_mood_event/4, self())
    {:ok, id}
  end

  def handle_mood_event(_ev, meas, meta, pid) when is_pid(pid) do
    send(pid, {:mood_update, meas, meta})
  end

  # GenServer

  @impl true
  def init(_) do
    cfg   = Application.get_env(:brain, __MODULE__, [])
    clock = cfg[:clock] || :cycle

    levels =
      Map.merge(%{da: 0.35, "5ht": 0.50, glu: 0.40, ne: 0.50}, cfg[:init] || %{})

    state = %{
      levels: levels,
      baseline: levels,
      half_life_ms: cfg[:half_life_ms] || 12_000,
      last_ts: now_ms(),
      clock: clock
    }

    case clock do
      :cycle ->
        id = "moodcore-on-cycle-#{System.unique_integer([:positive])}"
        :ok = :telemetry.attach(id, [:brain, :cycle, :tick], &__MODULE__.on_cycle_tick/4, %{pid: self()})
        {:ok, Map.put(state, :telemetry_id, id)}

      :self ->
        Process.send_after(self(), :tick, 500)
        {:ok, state}
    end
  end

  # Bridge from CycleClock
  def on_cycle_tick(_e, meas, _meta, %{pid: pid}) when is_pid(pid),
    do: send(pid, {:tick, (meas[:dt_ms] || 0)})

  @impl true
  def handle_info({:tick, _dt_ms}, state) do
    # Decay on cycle-driven tick
    {:noreply, decay(state)}
  end

  @impl true
  def handle_info(:tick, state) do
    # Self-clock mode tick (rarely used)
    state = decay(state)
    Process.send_after(self(), :tick, 500)
    {:noreply, state}
  end

  @impl true
  def handle_cast({:bump, deltas}, state) do
    new_state =
      put_levels(state, fn v, k -> clamp(v + (Map.get(deltas, k, 0.0) || 0.0)) end)
      |> emit(:bump, 0)

    {:noreply, new_state}
  end

  @impl true
  def handle_call(:snapshot, _from, state), do: {:reply, state, state}

  def terminate(_reason, %{telemetry_id: id}) when is_binary(id) do
    :telemetry.detach(id)
    :ok
  end

  def terminate(_reason, _state), do: :ok

  # Internals

  defp decay(%{levels: lv, baseline: bl, half_life_ms: hl, last_ts: last} = st) do
    now = now_ms()
    dt  = max(now - last, 0)
    fac = :math.exp(-@ln2 * dt / max(hl, 1.0))

    new_lv =
      Map.new(lv, fn {k, v} ->
        b = Map.get(bl, k, 0.5)
        {k, clamp(b + (v - b) * fac)}
      end)

    st = %{st | levels: new_lv, last_ts: now}
    emit(st, :tick, dt)
  end

  defp emit(%{levels: lv} = st, source, dt_ms) do
    da  = Map.get(lv, :da, 0.5)
    s5  = Map.get(lv, :"5ht", 0.5)
    glu = Map.get(lv, :glu, 0.5)
    ne  = Map.get(lv, :ne, 0.5)

    exploration = 0.6 * da + 0.4 * ne
    inhibition  = s5
    vigilance   = ne
    plasticity  = 0.5 * da + 0.5 * glu

    :telemetry.execute(
      @event,
      %{da: da, "5ht": s5, glu: glu, ne: ne,
        exploration: exploration, inhibition: inhibition,
        vigilance: vigilance, plasticity: plasticity},
      %{source: source, dt_ms: dt_ms}
    )

    st
  end

  defp put_levels(st, f),
    do: %{st | levels: Map.new(st.levels, fn {k, v} -> {k, f.(v, k)} end)}

  defp clamp(x), do: min(1.0, max(0.0, x))
  defp now_ms(), do: System.monotonic_time(:millisecond)
end

