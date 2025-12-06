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
    [:brain, :mood, :appraisal_applied]    (NEW)

  Config (:brain, Brain.MoodCore) — all fields optional:
    baseline:           %{da: 0.35, "5ht": 0.50, glu: 0.40, ne: 0.50} | keyword | nil
    half_life_ms:       %{da: 12_000, "5ht": 9_000, glu: 45_000, ne: 6_000} | pos_integer | "12000"
    max_delta_per_tick: 0.08
    saturation_ticks:   30
    shock_threshold:    0.25
    clock:              :cycle | :self   (default :cycle)
    init:               map/keyword (merged on top of baseline)

  Tone mapping:
    Uses config :brain, :mood_tone (see config/mood.exs) to classify
    indices → :warm | :cool | :deescalate | :neutral.

  Latents (optional):
    If Brain.AffectLatents.compute/2 is available, we attach TRCS-style
    latents (e.g. :threat, :reward, :control, :safety) into telemetry
    meas/meta under :latents.

  Appraisal hook (NEW):
    apply_appraisal/1 applies small bounded deltas derived from V/A/D + tags.
    This is an engineering control correlate (not a biological simulation).
  """

  use GenServer

  @event_update [:brain, :mood, :update]
  @event_saturation [:brain, :mood, :saturation]
  @event_shock [:brain, :mood, :shock]
  @event_appraisal_applied [:brain, :mood, :appraisal_applied]

  # ---------- Public API ----------

  def start_link(_opts \\ []), do: GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
  def snapshot(), do: GenServer.call(__MODULE__, :snapshot)

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
  def apply_dopamine(k \\ 0.03), do: bump(%{da: +k})
  def apply_serotonin(k \\ 0.03), do: bump(%{"5ht" => +k})
  def apply_norepinephrine(k \\ 0.03), do: bump(%{ne: +k})
  def apply_glutamate(k \\ 0.03), do: bump(%{glu: +k})

  @doc "Reset neuromodulators back to baseline (clears saturation counters)."
  def reset(), do: GenServer.call(__MODULE__, :reset)

  @doc """
  Configure MoodCore at runtime. Accepts the same shapes as application config.
  Supported keys: :baseline, :half_life_ms, :max_delta_per_tick, :saturation_ticks,
  :shock_threshold, :clock, :init (merged immediately).
  """
  def configure(opts), do: GenServer.call(__MODULE__, {:configure, opts})

  @doc "Detach a previously registered subscriber id returned from subscribe/0."
  def detach(id) when is_binary(id), do: :telemetry.detach(id)

  # -------- NEW PUBLIC HOOKS (additive) --------

  @doc """
  Intent → neuro deltas (scaled by confidence). Call this right after you set last_intent.

      Brain.MoodCore.apply_intent(:abuse, 0.98)
  """
  def apply_intent(intent, confidence \\ 1.0) when is_atom(intent) and is_number(confidence) do
    GenServer.cast(__MODULE__, {:apply_intent, intent, confidence})
  end

  @doc """
  Apply an affective appraisal map (V/A/D + tags). Safe no-op for non-maps.

  Expected shape (minimum):
      %{valence: v, arousal: a, dominance: d, tags: [...]}
  """
  def apply_appraisal(%{} = appraisal) do
    GenServer.cast(__MODULE__, {:apply_appraisal, appraisal})
  end

  def apply_appraisal(_), do: :ok

  @doc """
  Lightweight arousal/plasticity nudge from recent activation load (active_cells map or list).
  Safe to call from LIFG/ATL ticks.
  """
  def register_activation(active_cells) do
    GenServer.cast(__MODULE__, {:activation, active_cells})
  end

  @doc """
  Optional WM density nudge (pass your WM list). Keeps things reactive without spikes.
  """
  def update_wm(wm_list) when is_list(wm_list) do
    GenServer.cast(__MODULE__, {:wm, wm_list})
  end

  # ---------- GenServer ----------

  @impl true
  def init(_opts) do
    # Accept ANY shape (keyword, map, integer, string, nil)
    cfg = Application.get_env(:brain, __MODULE__, []) |> normalize_cfg()

    baseline =
      merge_defaults(
        %{da: 0.35, "5ht": 0.50, glu: 0.40, ne: 0.50},
        get_map_like(cfg, :baseline)
      )

    init_lv =
      merge_defaults(
        baseline,
        get_map_like(cfg, :init)
      )

    half_life_ms =
      resolve_half_life(
        %{da: 12_000, "5ht": 9_000, glu: 45_000, ne: 6_000},
        Keyword.get(cfg, :half_life_ms, nil)
      )

    state = %{
      levels: coerce_keys(init_lv),
      baseline: coerce_keys(baseline),
      half_life_ms: coerce_keys(half_life_ms),
      max_delta_per_tick: to_float(Keyword.get(cfg, :max_delta_per_tick, 0.08), 0.08),
      saturation_ticks: to_pos_int(Keyword.get(cfg, :saturation_ticks, 30), 30),
      shock_threshold: to_float(Keyword.get(cfg, :shock_threshold, 0.25), 0.25),
      last_ts: now_ms(),
      last_dt_ms: 0,
      last_levels: coerce_keys(init_lv),
      sat_counts: %{da: 0, "5ht": 0, glu: 0, ne: 0},
      clock:
        case Keyword.get(cfg, :clock, :cycle) do
          :cycle -> :cycle
          :self -> :self
          _ -> :cycle
        end
    }

    case state.clock do
      :cycle ->
        id = "moodcore-on-cycle-#{System.unique_integer([:positive])}"

        :ok =
          :telemetry.attach(id, [:brain, :cycle, :tick], &__MODULE__.on_cycle_tick/4, %{
            pid: self()
          })

        {:ok, Map.put(state, :telemetry_id, id)}

      :self ->
        Process.send_after(self(), :tick, 500)
        {:ok, state}
    end
  end

  # Bridge from CycleClock
  def on_cycle_tick(_e, meas, _meta, %{pid: pid}) when is_pid(pid),
    do: send(pid, {:tick, meas[:dt_ms] || 0})

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

  # ---- NEW CASTS ----

  @impl true
  def handle_cast({:apply_intent, intent, conf}, st) do
    deltas = intent_to_deltas(intent, conf)
    deltas = sanitize_deltas(deltas, st.max_delta_per_tick)

    st =
      st
      |> put_levels(fn v, k -> clamp(v + Map.get(deltas, k, 0.0)) end)
      |> emit({:intent, intent}, 0, :bump)

    {:noreply, st}
  end

  @impl true
  def handle_cast({:apply_appraisal, appraisal}, st) when is_map(appraisal) do
    {raw_deltas, app_meta} = appraisal_to_deltas(appraisal)
    deltas = sanitize_deltas(raw_deltas, st.max_delta_per_tick)

    st2 =
      st
      |> put_levels(fn v, k -> clamp(v + Map.get(deltas, k, 0.0)) end)
      |> emit(:appraisal, 0, :bump)

    :telemetry.execute(
      @event_appraisal_applied,
      %{
        count: 1,
        delta_da: Map.get(deltas, :da, 0.0) * 1.0,
        delta_5ht: Map.get(deltas, :"5ht", 0.0) * 1.0,
        delta_glu: Map.get(deltas, :glu, 0.0) * 1.0,
        delta_ne: Map.get(deltas, :ne, 0.0) * 1.0
      },
      app_meta
      |> Map.put(:dt_ms, 0)
      |> Map.put(:source, :appraisal)
      |> Map.put(:cause, :appraisal)
    )

    {:noreply, st2}
  rescue
    _ -> {:noreply, st}
  catch
    _, _ -> {:noreply, st}
  end

  @impl true
  def handle_cast({:activation, cells}, st) do
    deltas = activation_to_deltas(cells) |> sanitize_deltas(st.max_delta_per_tick)

    st =
      st
      |> put_levels(fn v, k -> clamp(v + Map.get(deltas, k, 0.0)) end)
      |> emit(:activation, 0, :bump)

    {:noreply, st}
  end

  @impl true
  def handle_cast({:wm, wm_list}, st) do
    deltas = wm_to_deltas(wm_list) |> sanitize_deltas(st.max_delta_per_tick)

    st =
      st
      |> put_levels(fn v, k -> clamp(v + Map.get(deltas, k, 0.0)) end)
      |> emit(:wm, 0, :bump)

    {:noreply, st}
  end

  @impl true
  def handle_call(:snapshot, _from, state), do: {:reply, decorate_snapshot(state), state}

  @impl true
  def handle_call(:reset, _from, st) do
    lv = st.baseline

    st =
      st
      |> Map.put(:levels, lv)
      |> Map.put(:last_levels, lv)
      |> Map.put(:sat_counts, %{da: 0, "5ht": 0, glu: 0, ne: 0})
      |> Map.put(:last_ts, now_ms())
      |> Map.put(:last_dt_ms, 0)

    {:reply, decorate_snapshot(st), st}
  end

  @impl true
  def handle_call({:configure, opts}, _from, st) do
    cfg = normalize_cfg(opts || [])

    new =
      st
      |> then(fn s ->
        case get_map_like(cfg, :baseline) do
          %{} = bl when map_size(bl) > 0 -> %{s | baseline: bl}
          _ -> s
        end
      end)
      |> then(fn s ->
        case Keyword.get(cfg, :half_life_ms, nil) do
          nil -> s
          v -> %{s | half_life_ms: resolve_half_life(s.half_life_ms, v) |> coerce_keys()}
        end
      end)
      |> then(fn s ->
        %{
          s
          | max_delta_per_tick:
              to_float(
                Keyword.get(cfg, :max_delta_per_tick, s.max_delta_per_tick),
                s.max_delta_per_tick
              ),
            saturation_ticks:
              to_pos_int(
                Keyword.get(cfg, :saturation_ticks, s.saturation_ticks),
                s.saturation_ticks
              ),
            shock_threshold:
              to_float(Keyword.get(cfg, :shock_threshold, s.shock_threshold), s.shock_threshold)
        }
      end)
      |> then(fn s ->
        case Keyword.get(cfg, :clock, s.clock) do
          :cycle -> %{s | clock: :cycle}
          :self -> %{s | clock: :self}
          _ -> s
        end
      end)
      |> then(fn s ->
        # Optional init merge applied immediately (like a controlled bump without events)
        case get_map_like(cfg, :init) do
          %{} = init when map_size(init) > 0 ->
            lv = merge_defaults(s.levels, init)
            %{s | levels: lv, last_levels: lv}

          _ ->
            s
        end
      end)

    {:reply, decorate_snapshot(new), new}
  end

  @impl true
  def terminate(_reason, %{telemetry_id: id}) when is_binary(id) do
    :telemetry.detach(id)
    :ok
  end

  def terminate(_reason, _state), do: :ok

  # ---------- Internals ----------

  defp decay_and_emit(%{levels: lv, baseline: bl, half_life_ms: hl, last_ts: last} = st, source) do
    now = now_ms()
    dt = max(now - last, 0)

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

  # keep a convenience 3-arity (no default on the 4-arity head)
  defp emit(
         %{levels: lv, last_levels: prev, saturation_ticks: sat_n, shock_threshold: shock_thr} =
           st,
         source,
         dt_ms,
         cause
       ) do
    # Raw neuros
    da = Map.get(lv, :da, 0.5)
    s5 = Map.get(lv, :"5ht", 0.5)
    glu = Map.get(lv, :glu, 0.5)
    ne = Map.get(lv, :ne, 0.5)

    # Derived mood indices
    exploration = 0.6 * da + 0.4 * ne
    inhibition = s5
    vigilance = ne
    plasticity = 0.5 * da + 0.5 * glu

    # Bundle into a mood struct + tone so UI can see the "shape"
    mood = %{
      exploration: exploration,
      inhibition: inhibition,
      vigilance: vigilance,
      plasticity: plasticity
    }

    tone = choose_tone(mood)

    meas0 = %{
      da: da,
      "5ht": s5,
      glu: glu,
      ne: ne,
      exploration: exploration,
      inhibition: inhibition,
      vigilance: vigilance,
      plasticity: plasticity,
      tone: tone
    }

    meta0 =
      %{source: source, cause: cause, dt_ms: dt_ms}
      |> Map.put(:mood, %{levels: lv, derived: mood, tone: tone})

    # Optional TRCS latents
    latents = maybe_compute_latents(meas0, meta0)

    meas = maybe_put_latents(meas0, latents)
    meta = maybe_put_latents(meta0, latents)

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
    dx =
      :math.sqrt(
        :math.pow(da - Map.get(prev, :da, da), 2) +
          :math.pow(s5 - Map.get(prev, :"5ht", s5), 2) +
          :math.pow(glu - Map.get(prev, :glu, glu), 2) +
          :math.pow(ne - Map.get(prev, :ne, ne), 2)
      )

    if dx > shock_thr do
      :telemetry.execute(@event_shock, Map.put(meas, :delta_norm, dx), meta)
    end

    %{st | last_levels: lv, sat_counts: sat_counts}
  end

  defp decorate_snapshot(%{levels: lv, last_dt_ms: dt} = st) do
    da = Map.get(lv, :da, 0.5)
    s5 = Map.get(lv, :"5ht", 0.5)
    glu = Map.get(lv, :glu, 0.5)
    ne = Map.get(lv, :ne, 0.5)

    mood = %{
      exploration: 0.6 * da + 0.4 * ne,
      inhibition: s5,
      vigilance: ne,
      plasticity: 0.5 * da + 0.5 * glu
    }

    tone_hint = choose_tone(mood)

    base =
      %{mood: mood, tone_hint: tone_hint, dt_ms: dt}
      |> maybe_put_latents(maybe_compute_latents(%{levels: lv, mood: mood}, %{}))

    Map.merge(st, base)
  end

  defp put_levels(st, f),
    do: %{st | levels: Map.new(st.levels, fn {k, v} -> {k, f.(v, k)} end)}

  # ---------- Appraisal mapping (NEW) ----------

  defp appraisal_to_deltas(appraisal) when is_map(appraisal) do
    v = to_float_num(Map.get(appraisal, :valence, 0.0), 0.0) |> clamp11()
    a = to_float_num(Map.get(appraisal, :arousal, 0.0), 0.0) |> clamp01()
    d = to_float_num(Map.get(appraisal, :dominance, 0.0), 0.0) |> clamp11()

    tags = appraisal |> Map.get(:tags, []) |> coerce_tags()

    praise? = MapSet.member?(tags, :praise)
    insult? = MapSet.member?(tags, :insult)
    threat? = MapSet.member?(tags, :threat)
    urgency? = MapSet.member?(tags, :urgency)
    uncertain? = MapSet.member?(tags, :uncertainty)
    question? = MapSet.member?(tags, :question)

    # Deltas are intentionally small; max_delta_per_tick remains the hard cap.
    da =
      0.06 * v +
        0.02 * a +
        (if praise?, do: 0.03, else: 0.0) +
        (if insult?, do: -0.02, else: 0.0)

    s5 =
      0.07 * v +
        0.05 * d +
        (if praise?, do: 0.015, else: 0.0) +
        (if insult?, do: -0.05, else: 0.0) +
        (if threat?, do: -0.03, else: 0.0)

    ne =
      0.08 * a +
        (if threat?, do: 0.05, else: 0.0) +
        (if urgency?, do: 0.03, else: 0.0) +
        (if d < -0.2, do: 0.02, else: 0.0)

    glu =
      0.03 * a +
        (if uncertain?, do: 0.05, else: 0.0) +
        (if question?, do: 0.02, else: 0.0)

    meta = %{
      appraisal: %{
        v: Map.get(appraisal, :v, 1),
        valence: v,
        arousal: a,
        dominance: d,
        tags: MapSet.to_list(tags) |> Enum.sort()
      }
    }

    {%{da: da, "5ht": s5, ne: ne, glu: glu}, meta}
  end

  defp appraisal_to_deltas(_), do: {%{}, %{}}

  defp coerce_tags(tags) when is_list(tags) do
    tags
    |> Enum.map(fn
      t when is_atom(t) -> t
      t when is_binary(t) ->
        t |> String.trim() |> String.downcase() |> String.replace(~r/\s+/, "_") |> String.replace("-", "_") |> String.to_atom()

      _ ->
        nil
    end)
    |> Enum.reject(&is_nil/1)
    |> MapSet.new()
  end

  defp coerce_tags(_), do: MapSet.new()

  defp to_float_num(v, _default) when is_number(v), do: v * 1.0

  defp to_float_num(v, default) when is_binary(v) do
    case Float.parse(String.trim(v)) do
      {n, _} -> n
      _ -> default * 1.0
    end
  end

  defp to_float_num(_, default), do: default * 1.0

  defp clamp11(x) when is_number(x), do: min(1.0, max(-1.0, x))
  defp clamp01(x) when is_number(x), do: min(1.0, max(0.0, x))

  # ---------- Intent / Load mapping (new) ----------

  defp intent_to_deltas(:abuse, conf) do
    # Stress/hostility → ↑NE (vigilance), ↓5HT (inhibition), mild ↓DA (reward)
    k = clamp_conf(conf)
    %{ne: +0.25 * k, "5ht": -0.20 * k, da: -0.05 * k}
  end

  defp intent_to_deltas(:gratitude, conf) do
    k = clamp_conf(conf)
    %{"5ht": +0.12 * k, da: +0.15 * k, ne: -0.05 * k}
  end

  defp intent_to_deltas(:greeting, conf) do
    k = clamp_conf(conf)
    %{"5ht": +0.08 * k, ne: -0.02 * k}
  end

  defp intent_to_deltas(:question, conf) do
    # Curiosity → mild ↑DA (exploration) and ↑NE (attention)
    k = clamp_conf(conf)
    %{da: +0.07 * k, ne: +0.07 * k}
  end

  defp intent_to_deltas(:help, conf), do: intent_to_deltas(:question, conf)
  defp intent_to_deltas(:instruction, c), do: intent_to_deltas(:question, c)

  defp intent_to_deltas(_other, conf) do
    k = clamp_conf(conf)
    # Default: tiny calming + slight plasticity
    %{"5ht": +0.03 * k, glu: +0.03 * k}
  end

  defp activation_to_deltas(cells) do
    n =
      case cells do
        %{} -> map_size(cells)
        list when is_list(list) -> length(list)
        _ -> 0
      end

    load = min(1.0, n / 120.0)
    # Gentle: attention + learning + a hint of exploration
    %{ne: +0.08 * load, glu: +0.04 * load, da: +0.03 * load}
  end

  defp wm_to_deltas(wm_list) do
    density = min(1.0, (wm_list |> length()) / 7.0)
    %{ne: +0.05 * density, glu: +0.03 * density}
  end

  # ---------- Tone selection (revised again – brain truth for tone_hint) ----------

  defp choose_tone(%{vigilance: vig, inhibition: inh, exploration: exp}) do
    cfg = Application.get_env(:brain, :mood_tone, [])
    neutral_band = Keyword.get(cfg, :neutral_band, 0.10)

    warm_cfg = Keyword.get(cfg, :warm, [])
    cool_cfg = Keyword.get(cfg, :cool, [])
    deesc_cfg = Keyword.get(cfg, :deescalate, [])

    # De-escalate “hot” gate: high vigilance + low inhibition
    hot_vig_min = deesc_cfg[:vigilance_min] || 0.85
    hot_inh_max = deesc_cfg[:inhibition_max] || 0.60

    warm_exp_min = warm_cfg[:exploration_min] || 0.65
    warm_vig_max = warm_cfg[:vigilance_max] || 0.85

    cool_inh_min = cool_cfg[:inhibition_min] || 0.70

    exp = (exp || 0.5) * 1.0
    inh = (inh || 0.5) * 1.0
    vig = (vig || 0.5) * 1.0

    e_dev = exp - 0.5
    i_dev = inh - 0.5
    v_dev = vig - 0.5

    # “How far from the middle are we on any axis?”
    spread =
      [e_dev, i_dev, v_dev]
      |> Enum.map(&abs/1)
      |> Enum.max()

    cond do
      # 1) Clearly “hot” / conflict:
      #    very high vigilance AND not enough inhibition → deescalate.
      vig >= hot_vig_min and inh <= hot_inh_max ->
        :deescalate

      # 2) Friendly / engaged:
      #    exploration above mid, inhibition not too low, vigilance not spiking.
      exp >= warm_exp_min and inh >= 0.50 and vig <= warm_vig_max ->
        :warm

      # 3) Cautious / inhibited (cool):
      inh >= cool_inh_min and exp <= 0.50 and vig <= warm_vig_max ->
        :cool

      # 4) If everything is very close to the center, call it neutral.
      spread < neutral_band ->
        :neutral

      # 5) Fallback: dominant positive axis, but bias toward deescalate
      #    when vigilance is the biggest excursion.
      true ->
        {axis, dev} =
          [deescalate: v_dev, warm: e_dev, cool: i_dev]
          |> Enum.max_by(fn {_k, d} -> abs(d) end)

        cond do
          # If the strongest axis is below center or tiny, stay neutral.
          dev <= 0.0 or abs(dev) < neutral_band ->
            :neutral

          axis == :deescalate and vig >= hot_vig_min ->
            :deescalate

          axis == :warm and exp >= warm_exp_min ->
            :warm

          axis == :cool and inh >= cool_inh_min ->
            :cool

          true ->
            :neutral
        end
    end
  end

  defp choose_tone(_), do: :neutral

  # ---------- small helpers ----------

  defp maybe_compute_latents(meas, meta) do
    try do
      if function_exported?(Brain.AffectLatents, :compute, 2) do
        Brain.AffectLatents.compute(meas, meta)
      else
        %{}
      end
    rescue
      _ -> %{}
    end
  end

  defp maybe_put_latents(map, %{} = latents) when map_size(latents) == 0, do: map
  defp maybe_put_latents(map, latents), do: Map.put(map, :latents, latents)

  defp clamp_conf(c) when is_number(c), do: min(1.0, max(0.0, c))
  defp clamp(x), do: min(1.0, max(0.0, x))
  defp now_ms(), do: System.monotonic_time(:millisecond)

  defp coerce_keys(map) do
    map
    |> Enum.map(fn
      {"da", v} -> {:da, v}
      {:da, v} -> {:da, v}
      {"5ht", v} -> {:"5ht", v}
      {:"5ht", v} -> {:"5ht", v}
      {"glu", v} -> {:glu, v}
      {:glu, v} -> {:glu, v}
      {"ne", v} -> {:ne, v}
      {:ne, v} -> {:ne, v}
      {k, v} -> {k, v}
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
      _ -> []
    end
  end

  defp normalize_cfg(_), do: []

  # Pull a map/keyword value from cfg and turn it into a map with our neuro keys
  defp get_map_like(cfg, key) do
    case Keyword.get(cfg, key, %{}) do
      %{} = m -> coerce_keys(m)
      l when is_list(l) -> l |> Map.new() |> coerce_keys()
      _ -> %{}
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
        i when is_integer(i) and i > 0 ->
          i

        f when is_float(f) and f > 0.0 ->
          trunc(f)

        s when is_binary(s) ->
          case Integer.parse(String.trim(s)) do
            {k, _} when k > 0 -> k
            _ -> nil
          end

        _ ->
          nil
      end

    if n do
      %{da: n, "5ht": n, glu: n, ne: n}
    else
      %{da: 12_000, "5ht": 9_000, glu: 45_000, ne: 6_000}
    end
  end

  defp to_pos_int(v, _default) when is_integer(v) and v > 0, do: v
  defp to_pos_int(v, _default) when is_float(v) and v > 0, do: trunc(v)

  defp to_pos_int(v, default) when is_binary(v) do
    case Integer.parse(String.trim(v)) do
      {n, _} when n > 0 -> n
      _ -> default
    end
  end

  defp to_pos_int(_, default), do: default

  defp to_float(v, _default) when is_number(v), do: v * 1.0

  defp to_float(v, default) when is_binary(v) do
    case Float.parse(String.trim(v)) do
      {n, _} -> n
      _ -> default * 1.0
    end
  end

  defp to_float(_, default), do: default * 1.0
end

