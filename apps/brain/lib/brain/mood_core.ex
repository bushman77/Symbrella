defmodule Brain.MoodCore do
  @moduledoc """
  MoodCore: neuromodulator state with half-life decay, receptor-style bumps,
  and derived mood indices emitted via telemetry.

  Neuromodulators (raw levels, 0.0..1.0):
    :da    (dopamine)
    :"5ht" (serotonin)
    :glu   (glutamate)
    :ne    (norepinephrine)

  Derived mood indices:
    :exploration, :inhibition, :vigilance, :plasticity

  Telemetry:
    [:brain, :mood, :update]
    [:brain, :mood, :saturation]
    [:brain, :mood, :shock]

  Config (:brain, Brain.MoodCore) — all fields optional:
    baseline:           %{da: 0.35, "5ht": 0.50, glu: 0.40, ne: 0.50} | keyword | nil
    half_life_ms:       %{da: 12_000, "5ht": 9_000, glu: 45_000, ne: 6_000} | pos_integer | "12000"
    max_delta_per_tick: 0.08
    saturation_ticks:   30
    shock_threshold:    0.25
    clock:              :cycle | :self   (default :cycle)
    init:               map/keyword (merged on top of baseline)
  """

  use GenServer

  @ln2 :math.log(2)

  @event_update      [:brain, :mood, :update]
  @event_saturation  [:brain, :mood, :saturation]
  @event_shock       [:brain, :mood, :shock]

  # ---------- Public API ----------

  def start_link(_opts \\ []), do: GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
  def snapshot(),              do: GenServer.call(__MODULE__, :snapshot)

  @doc """
  Bump neuromodulators by deltas map. Keys may be atoms or strings: "da","5ht","glu","ne".
  Deltas are clamped to +/- max_delta_per_tick per component.
  """
  def bump(deltas), do: GenServer.cast(__MODULE__, {:bump, deltas, :external})

  @doc """
  Subscribe current process to mood updates. Receives:
      {:mood_update, measurements_map, meta_map}
  """
  def subscribe() do
    id = "moodcore-subscriber-#{System.unique_integer([:positive])}"
    :telemetry.attach(id, @event_update, &__MODULE__.handle_mood_event/4, self())
    {:ok, id}
  end

  def handle_mood_event(_ev, meas, meta, pid) when is_pid(pid) do
    send(pid, {:mood_update, meas, meta})
  end

  # Convenience receptor-style applicators
  def apply_dopamine(k \\ 0.03),       do: bump(%{da: +k})
  def apply_serotonin(k \\ 0.03),      do: bump(%{"5ht" => +k})
  def apply_norepinephrine(k \\ 0.03), do: bump(%{ne: +k})
  def apply_glutamate(k \\ 0.03),      do: bump(%{glu: +k})

  # ---------- GenServer ----------

  @impl true
  def init(_opts) do
    # Accept ANY shape (keyword, map, integer, string, nil)
    cfg = Application.get_env(:brain, __MODULE__, []) |> normalize_cfg()

    baseline =
      merge_defaults(%{da: 0.35, "5ht": 0.50, glu: 0.40, ne: 0.50},
        get_map_like(cfg, :baseline))

    init_lv =
      merge_defaults(baseline,
        get_map_like(cfg, :init))

    half_life_ms =
      resolve_half_life(
        %{da: 12_000, "5ht": 9_000, glu: 45_000, ne: 6_000},
        Keyword.get(cfg, :half_life_ms, nil)
      )

    state = %{
      levels:     coerce_keys(init_lv),
      baseline:   coerce_keys(baseline),
      half_life_ms: coerce_keys(half_life_ms),
      max_delta_per_tick: to_float(Keyword.get(cfg, :max_delta_per_tick, 0.08), 0.08),
      saturation_ticks:   to_pos_int(Keyword.get(cfg, :saturation_ticks, 30), 30),
      shock_threshold:    to_float(Keyword.get(cfg, :shock_threshold, 0.25), 0.25),
      last_ts:    now_ms(),
      last_dt_ms: 0,
      last_levels: coerce_keys(init_lv),
      sat_counts: %{da: 0, "5ht": 0, glu: 0, ne: 0},
      clock:
        case Keyword.get(cfg, :clock, :cycle) do
          :cycle -> :cycle
          :self  -> :self
          _      -> :cycle
        end
    }

    case state.clock do
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
  def handle_info({:tick, _dt_ms}, state), do: {:noreply, decay_and_emit(state, :tick)}

  @impl true
  def handle_info(:tick, state) do
    st = decay_and_emit(state, :tick)
    Process.send_after(self(), :tick, 500)
    {:noreply, st}
  end

  @impl true
  def handle_cast({:bump, deltas, source}, state) do
    deltas = sanitize_deltas(deltas, state.max_delta_per_tick)

    new_state =
      state
      |> put_levels(fn v, k -> clamp(v + Map.get(deltas, k, 0.0)) end)
      |> emit(:bump, 0, source)

    {:noreply, new_state}
  end

  @impl true
  def handle_call(:snapshot, _from, state), do: {:reply, decorate_snapshot(state), state}

  def terminate(_reason, %{telemetry_id: id}) when is_binary(id) do
    :telemetry.detach(id)
    :ok
  end
  def terminate(_reason, _state), do: :ok

  # ---------- Internals ----------

  defp decay_and_emit(%{levels: lv, baseline: bl, half_life_ms: hl, last_ts: last} = st, source) do
    now = now_ms()
    dt  = max(now - last, 0)

    new_lv =
      Map.new(lv, fn {k, v} ->
        h = max(Map.get(hl, k, 12_000), 1.0)
        b = Map.get(bl, k, 0.5)
        fac = :math.pow(0.5, dt / h)
        {k, clamp(b + (v - b) * fac)}
      end)

    st = %{st | levels: new_lv, last_ts: now, last_dt_ms: dt}
    emit(st, source, dt, :decay)
  end

  defp emit(%{levels: lv, last_levels: prev, saturation_ticks: sat_n, shock_threshold: shock_thr} = st, source, dt_ms, cause \\ :other) do
    # Raw neuros
    da  = Map.get(lv, :da, 0.5)
    s5  = Map.get(lv, :"5ht", 0.5)
    glu = Map.get(lv, :glu, 0.5)
    ne  = Map.get(lv, :ne, 0.5)

    # Derived mood indices
    exploration = 0.6 * da + 0.4 * ne
    inhibition  = s5
    vigilance   = ne
    plasticity  = 0.5 * da + 0.5 * glu

    meas = %{
      da: da, "5ht": s5, glu: glu, ne: ne,
      exploration: exploration, inhibition: inhibition,
      vigilance: vigilance, plasticity: plasticity
    }

    meta = %{source: source, cause: cause, dt_ms: dt_ms}
    :telemetry.execute(@event_update, meas, meta)

    # Saturation detector
    sat_counts =
      Enum.reduce([:da, :"5ht", :glu, :ne], st.sat_counts, fn k, acc ->
        v = Map.fetch!(lv, k)
        stuck? = v == 0.0 or v == 1.0
        Map.put(acc, k, if(stuck?, do: acc[k] + 1, else: 0))
      end)

    if Enum.any?(sat_counts, fn {_k, n} -> n >= sat_n end) do
      :telemetry.execute(@event_saturation, meas, Map.put(meta, :ticks, sat_counts))
    end

    # Shock detector (L2-norm of delta across neuros)
    dx = :math.sqrt(
      (:math.pow(da  - Map.get(prev, :da,  da), 2) +
       :math.pow(s5  - Map.get(prev, :"5ht", s5), 2) +
       :math.pow(glu - Map.get(prev, :glu, glu), 2) +
       :math.pow(ne  - Map.get(prev, :ne,  ne), 2))
    )

    if dx > shock_thr do
      :telemetry.execute(@event_shock, Map.put(meas, :delta_norm, dx), meta)
    end

    %{st | last_levels: lv, sat_counts: sat_counts}
  end

  defp decorate_snapshot(%{levels: lv, last_dt_ms: dt} = st) do
    da  = Map.get(lv, :da, 0.5)
    s5  = Map.get(lv, :"5ht", 0.5)
    glu = Map.get(lv, :glu, 0.5)
    ne  = Map.get(lv, :ne, 0.5)

    mood = %{
      exploration: 0.6 * da + 0.4 * ne,
      inhibition:  s5,
      vigilance:   ne,
      plasticity:  0.5 * da + 0.5 * glu
    }

    Map.merge(st, %{mood: mood, dt_ms: dt})
  end

  defp put_levels(st, f),
    do: %{st | levels: Map.new(st.levels, fn {k, v} -> {k, f.(v, k)} end)}

  # ---------- small helpers ----------

  defp clamp(x),   do: min(1.0, max(0.0, x))
  defp now_ms(),   do: System.monotonic_time(:millisecond)

  defp coerce_keys(map) do
    map
    |> Enum.map(fn
      {"da", v}   -> {:da, v}
      {:da, v}    -> {:da, v}
      {"5ht", v}  -> {:"5ht", v}
      {:"5ht", v} -> {:"5ht", v}
      {"glu", v}  -> {:glu, v}
      {:glu, v}   -> {:glu, v}
      {"ne", v}   -> {:ne, v}
      {:ne, v}    -> {:ne, v}
      {k, v}      -> {k, v}
    end)
    |> Map.new()
  end

  defp sanitize_deltas(deltas, cap) do
    deltas
    |> coerce_keys()
    |> Enum.map(fn {k, v} -> {k, clamp_delta(v, cap)} end)
    |> Map.new()
  end

  defp clamp_delta(v, cap) when is_number(v) do
    c = abs(cap || 0.08)
    max(-c, min(+c, v))
  end
  defp clamp_delta(_, cap), do: 0.0 |> clamp_delta(cap)

  # ---------- defensive config helpers ----------

defp normalize_cfg(cfg) when is_list(cfg) do
  if Keyword.keyword?(cfg), do: cfg, else: []
end

defp normalize_cfg(%{} = cfg), do: Map.to_list(cfg)
defp normalize_cfg(v) when is_integer(v), do: [half_life_ms: v]

defp normalize_cfg(v) when is_binary(v) do
  case Integer.parse(String.trim(v)) do
    {n, _} -> [half_life_ms: n]
    _      -> []
  end
end

defp normalize_cfg(_), do: []

  # Pull a map/keyword value from cfg and turn it into a map with our neuro keys
  defp get_map_like(cfg, key) do
    case Keyword.get(cfg, key, %{}) do
      %{} = m   -> coerce_keys(m)
      l when is_list(l) -> l |> Map.new() |> coerce_keys()
      _         -> %{}
    end
  end

  # Merge a defaults map with an override map (override only known keys)
  defp merge_defaults(defaults, overrides) do
    overrides = coerce_keys(overrides || %{})
    defaults
    |> Enum.map(fn {k, v} -> {k, Map.get(overrides, k, v)} end)
    |> Map.new()
  end

  # Accept per-neuro map OR a single integer/string and produce a full map
  defp resolve_half_life(defaults, val) when is_map(val) do
    merge_defaults(defaults, val)
  end
defp resolve_half_life(defaults, val) when is_list(val) do
  resolve_half_life(defaults, Map.new(val))
end

  defp resolve_half_life(_defaults, val) do
    # number/string → broadcast to all neuros; fallback to sane defaults if invalid
    n =
      case val do
        i when is_integer(i) and i > 0 -> i
        f when is_float(f) and f > 0.0 -> trunc(f)
        s when is_binary(s) ->
          case Integer.parse(String.trim(s)) do
            {k, _} when k > 0 -> k
            _ -> nil
          end
        _ -> nil
      end

    if n do
      %{da: n, "5ht": n, glu: n, ne: n}
    else
      %{da: 12_000, "5ht": 9_000, glu: 45_000, ne: 6_000}
    end
  end

  defp to_pos_int(v, default) when is_integer(v) and v > 0,  do: v
  defp to_pos_int(v, default) when is_float(v)   and v > 0,  do: trunc(v)
  defp to_pos_int(v, default) when is_binary(v) do
    case Integer.parse(String.trim(v)) do
      {n, _} when n > 0 -> n
      _ -> default
    end
  end
  defp to_pos_int(_, default), do: default

  defp to_float(v, default) when is_number(v), do: v * 1.0
  defp to_float(v, default) when is_binary(v) do
    case Float.parse(String.trim(v)) do
      {n, _} -> n
      _ -> default * 1.0
    end
  end
  defp to_float(_, default), do: default * 1.0
end

