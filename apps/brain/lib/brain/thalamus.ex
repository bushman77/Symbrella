# apps/brain/lib/brain/thalamus.ex
defmodule Brain.Thalamus do
  @moduledoc """
  Thalamus — region relay for curiosity proposals.

  Subscribes:
    • [:curiosity, :proposal]         (from Curiosity)
    • [:brain, :ofc, :value]          (valuation from OFC/vmPFC)
    • [:brain, :acc, :conflict]       (global conflict / uncertainty)
    • [:brain, :mood, :update]        (derived mood indices from MoodCore)

  Flow per proposal:
    1) Snapshot WM/Attention via `Brain.snapshot_wm/0`
    2) Blend proposal score with latest OFC value (configurable weight)
    3) Attenuate by latest ACC conflict (configurable alpha)
    4) Apply **mood bias** to the score (bounded)
    5) Gate with `Brain.BasalGanglia.decide/4`
    6) Emit decision; DLPFC owns launching

  Emits:
    • [:brain, :thalamus, :curiosity, :decision]
      measurements: %{score}
      metadata: %{
        decision, source, probe,
        ofc_blended?: boolean, ofc_value: float | nil, ofc_weight: float,
        acc_applied?: boolean, acc_conflict: float | nil, acc_alpha: float,
        mood_applied?: boolean, mood_factor: float,
        mood_snapshot: %{exploration:, inhibition:, vigilance:, plasticity:},
        mood_weights: %{expl:, inhib:, vigil:, plast:}, mood_cap: float,
        v: 3
      }
  """

  use Brain, region: :thalamus
  require Logger

  @cur_handler_prefix  "brain-thalamus-curiosity-"
  @ofc_handler_prefix  "brain-thalamus-ofc-"
  @acc_handler_prefix  "brain-thalamus-acc-"
  @mood_handler_prefix "brain-thalamus-mood-"

  @ofc_cache_max 64

  # ── Public API ──────────────────────────────────────────────────────────────

  @doc """
  Update runtime parameters on the already-running Thalamus.

  Accepts a keyword or map with keys:
    * :ofc_weight       — 0.0..1.0
    * :acc_alpha        — 0.0..1.0
    * :mood_weights     — %{expl: float, inhib: float, vigil: float, plast: float}
    * :mood_cap         — 0.0..1.0 (max multiplicative influence per decision)

  Example:
      :ok = Brain.Thalamus.set_params(ofc_weight: 0.8, acc_alpha: 0.2,
                                      mood_weights: %{expl: 0.06, inhib: -0.08, vigil: -0.03, plast: 0.04},
                                      mood_cap: 0.15)
  """
  @spec set_params(server :: pid() | atom(), opts :: Keyword.t() | map()) :: :ok
  def set_params(server \\ __MODULE__, opts) when is_list(opts) or is_map(opts) do
    GenServer.call(server, {:set_params, opts})
  end

  @doc """
  Read the *effective* current parameters (after defaults + clamping).
  Returns a map like:
    %{
      ofc_weight: float(),
      acc_alpha: float(),
      mood_weights: %{expl: float, inhib: float, vigil: float, plast: float},
      mood_cap: float()
    }
  """
  @spec get_params(server :: pid() | atom()) :: map()
  def get_params(server \\ __MODULE__) do
    GenServer.call(server, :get_params)
  end

  # ── Region lifecycle ────────────────────────────────────────────────────────

  @impl GenServer
  def init(opts) do
    opts_kw = normalize_opts(opts)

    state = %{
      region: :thalamus,
      opts: opts_kw,
      stats: %{},
      ofc_cache: %{},   # probe_id(string) => value(float 0..1)
      ofc_order: [],    # recency list to bound the cache
      acc_conflict: nil,
      acc_last_ms: nil,
      mood: nil,        # %{exploration:, inhibition:, vigilance:, plasticity:}
      mood_last_ms: nil
    }

    cur_id  = unique(@cur_handler_prefix)
    ofc_id  = unique(@ofc_handler_prefix)
    acc_id  = unique(@acc_handler_prefix)
    mood_id = unique(@mood_handler_prefix)

    :ok = :telemetry.attach(cur_id,  [:curiosity, :proposal],   &__MODULE__.on_curiosity/4,   %{pid: self()})
    :ok = :telemetry.attach(ofc_id,  [:brain, :ofc, :value],    &__MODULE__.on_ofc_value/4,   %{pid: self()})
    :ok = :telemetry.attach(acc_id,  [:brain, :acc, :conflict], &__MODULE__.on_acc_conflict/4, %{pid: self()})
    :ok = :telemetry.attach(mood_id, [:brain, :mood, :update],  &__MODULE__.on_mood_update/4, %{pid: self()})

    {:ok,
      state
      |> Map.put(:cur_handler, cur_id)
      |> Map.put(:ofc_handler, ofc_id)
      |> Map.put(:acc_handler, acc_id)
      |> Map.put(:mood_handler, mood_id)}
  end

  @impl GenServer
  def terminate(_reason, %{cur_handler: cur, ofc_handler: ofc, acc_handler: acc, mood_handler: mood}) do
    if is_binary(cur),  do: :telemetry.detach(cur)
    if is_binary(ofc),  do: :telemetry.detach(ofc)
    if is_binary(acc),  do: :telemetry.detach(acc)
    if is_binary(mood), do: :telemetry.detach(mood)
    :ok
  end
  def terminate(_reason, _state), do: :ok

  # ── Telemetry → GenServer bridges ──────────────────────────────────────────

  @doc false
  def on_curiosity(_ev, measurements, metadata, %{pid: pid}) when is_pid(pid),
    do: send(pid, {:curiosity_proposal, measurements, metadata})
  def on_curiosity(_, _, _, _), do: :ok

  @doc false
  def on_ofc_value(_ev, measurements, metadata, %{pid: pid}) when is_pid(pid),
    do: send(pid, {:ofc_value, measurements, metadata})
  def on_ofc_value(_, _, _, _), do: :ok

  @doc false
  def on_acc_conflict(_ev, measurements, _metadata, %{pid: pid}) when is_pid(pid),
    do: send(pid, {:acc_conflict, measurements})
  def on_acc_conflict(_, _, _, _), do: :ok

  @doc false
  def on_mood_update(_ev, measurements, meta, %{pid: pid}) when is_pid(pid) do
    send(pid, {:mood_update, measurements, meta})
  end
  def on_mood_update(_, _, _, _), do: :ok

  # ── GenServer calls ────────────────────────────────────────────────────────

  @impl GenServer
  def handle_call({:set_params, opts_in}, _from, state) do
    opts_norm = normalize_opts(opts_in)
    new_opts = Keyword.merge(state.opts, opts_norm)
    {:reply, :ok, %{state | opts: new_opts}}
  end

  @impl GenServer
  def handle_call(:get_params, _from, state) do
    reply = %{
      ofc_weight:    cfg_ofc_weight(state.opts),
      acc_alpha:     cfg_acc_alpha(state.opts),
      mood_weights:  cfg_mood_weights(state.opts),
      mood_cap:      cfg_mood_cap(state.opts)
    }
    {:reply, reply, state}
  end

  # ── Region message handling ────────────────────────────────────────────────

  @impl GenServer
  def handle_info({:mood_update, meas, meta}, state) do
    mood = %{
      exploration: get_num(meas, :exploration, 0.5) |> clamp01(),
      inhibition:  get_num(meas, :inhibition,  0.5) |> clamp01(),
      vigilance:   get_num(meas, :vigilance,   0.5) |> clamp01(),
      plasticity:  get_num(meas, :plasticity,  0.5) |> clamp01()
    }
    {:noreply, %{state | mood: mood, mood_last_ms: System.system_time(:millisecond)}}
  end

  @impl GenServer
  def handle_info({:ofc_value, meas, meta}, state) do
    value   = get_num(meas, :value, 0.0) |> clamp01()
    probe_id = meta_get(meta, :probe_id, "curiosity|probe|unknown") |> to_string()
    {cache2, order2} = put_ofc_value(state.ofc_cache, state.ofc_order, probe_id, value, @ofc_cache_max)
    {:noreply, %{state | ofc_cache: cache2, ofc_order: order2}}
  end

  @impl GenServer
  def handle_info({:acc_conflict, meas}, state) do
    c = get_num(meas, :conflict, 0.0) |> clamp01()
    {:noreply, %{state | acc_conflict: c, acc_last_ms: System.system_time(:millisecond)}}
  end

  @impl GenServer
  def handle_info({:curiosity_proposal, meas, meta}, state) do
    # 1) Probe + base score
    base_score = get_num(meas, :score, 0.0)

    probe0 =
      meta_get(meta, :probe, %{})
      |> Map.put_new(:score, base_score)

    probe_id =
      (probe0[:id] || probe0["id"] || "curiosity|probe|unknown")
      |> to_string()

    # 2) OFC blend (convex combination)
    ofc_val = Map.get(state.ofc_cache, probe_id)
    w_ofc   = cfg_ofc_weight(state.opts)

    {probe1, blended_score, blended?} =
      case ofc_val do
        v when is_number(v) ->
          b = clamp01((1.0 - w_ofc) * base_score + w_ofc * v)
          {Map.put(probe0, :score, b), b, true}

        _ ->
          {probe0, base_score, false}
      end

    # 3) ACC brake (apply only if alpha > 0 and conflict > 0)
    alpha = cfg_acc_alpha(state.opts)
    c     = state.acc_conflict

    {probe2, post_acc_score, acc_applied?} =
      if is_number(c) and alpha > 0.0 and c > 0.0 do
        c1 = clamp01(c)
        s2 = clamp01(blended_score * (1.0 - alpha * c1))
        {Map.put(probe1, :score, s2), s2, true}
      else
        {probe1, blended_score, false}
      end

    # 4) Mood bias (bounded multiplicative factor around 1.0)
    {mood_factor, mood_applied?, mood_snapshot, mood_weights, mood_cap} =
      compute_mood_factor(state)

    final_score =
      if mood_applied? do
        clamp01(post_acc_score * mood_factor)
      else
        post_acc_score
      end

    probe = Map.put(probe2, :score, final_score)

    # 5) Gate
    %{wm: wm, cfg: cfg, attention: attn} = Brain.snapshot_wm()
    {decision, s} = Brain.BasalGanglia.decide(wm, probe, attn, cfg)

    # 6) Emit
    :telemetry.execute(
      [:brain, :thalamus, :curiosity, :decision],
      %{score: s},
      %{
        decision:     decision,
        source:       probe[:source] || probe["source"],
        probe:        probe,
        ofc_blended?: blended?,
        ofc_value:    ofc_val,
        ofc_weight:   w_ofc,
        acc_applied?: acc_applied?,
        acc_conflict: state.acc_conflict,
        acc_alpha:    alpha,
        mood_applied?: mood_applied?,
        mood_factor:   mood_factor,
        mood_snapshot: mood_snapshot,
        mood_weights:  mood_weights,
        mood_cap:      mood_cap,
        v: 3
      }
    )

    {:noreply, state}
  end

  @impl GenServer
  def handle_info(_msg, state), do: {:noreply, state}

  # ── Mood math ──────────────────────────────────────────────────────────────

  defp compute_mood_factor(%{mood: nil} = _state) do
    {1.0, false, nil, nil, cfg_mood_cap([])}
  end

  defp compute_mood_factor(%{mood: mood, opts: opts}) do
    # Deviation from neutral 0.5 (centered in [-0.5, +0.5])
    dx = %{
      expl:  (Map.get(mood, :exploration, 0.5) - 0.5),
      inhib: (Map.get(mood, :inhibition,  0.5) - 0.5),
      vigil: (Map.get(mood, :vigilance,   0.5) - 0.5),
      plast: (Map.get(mood, :plasticity,  0.5) - 0.5)
    }

    w = cfg_mood_weights(opts)
    cap = cfg_mood_cap(opts)

    # Signed weighted sum (small magnitudes). Positive increases score (loosens gate).
    raw =
      dx.expl  * (w.expl  || 0.0) +
      dx.inhib * (w.inhib || 0.0) +
      dx.vigil * (w.vigil || 0.0) +
      dx.plast * (w.plast || 0.0)

    # Bound the influence: factor in [1 - cap, 1 + cap]
    factor =
      raw
      |> (fn r -> 1.0 + max(-cap, min(cap, r)) end).()

    {factor, true, mood, w, cap}
  end

  # ── Helpers ────────────────────────────────────────────────────────────────

  defp unique(prefix),
    do:
      prefix <>
        Integer.to_string(:erlang.unique_integer([:positive])) <>
        "-" <> Integer.to_string(System.system_time(:microsecond))

  defp put_ofc_value(cache, order, key, val, max)
       when is_map(cache) and is_list(order) and is_binary(key) and is_number(val) do
    order1 = [key | Enum.reject(order, &(&1 == key))]

    {cache1, order2} =
      if length(order1) > max do
        {drop_key, rest} = {List.last(order1), Enum.slice(order1, 0, max)}
        {Map.delete(cache, drop_key), rest}
      else
        {cache, order1}
      end

    {Map.put(cache1, key, val), order2}
  end

  defp get_num(map, key, default) do
    case {Map.get(map, key), Map.get(map, to_string(key))} do
      {v, _} when is_integer(v) -> v * 1.0
      {v, _} when is_float(v)   -> v
      {_, v} when is_integer(v) -> v * 1.0
      {_, v} when is_float(v)   -> v
      _                         -> default * 1.0
    end
  end

  defp meta_get(meta, key, default \\ nil) do
    case {Map.get(meta, key), Map.get(meta, to_string(key))} do
      {nil, nil} -> default
      {v, _}     -> v
      {_, v}     -> v
    end
  end

  defp clamp01(x) when is_number(x), do: max(0.0, min(1.0, x * 1.0))
  defp clamp01(_), do: 0.0

  # ── Config (no guards with remote calls) ───────────────────────────────────

  defp cfg_ofc_weight(opts) do
    get_opt(opts, :ofc_weight, Application.get_env(:brain, :thalamus_ofc_weight, 0.5))
    |> to_float_01()
  end

  defp cfg_acc_alpha(opts) do
    get_opt(opts, :acc_alpha, Application.get_env(:brain, :thalamus_acc_alpha, 0.35))
    |> to_float_01()
  end

  defp cfg_mood_cap(opts) do
    cap =
      get_opt(opts, :mood_cap, Application.get_env(:brain, :thalamus_mood_cap, 0.15))

    cap
    |> case do
      v when is_number(v) -> max(0.0, min(1.0, v))
      _ -> 0.15
    end
  end

  defp cfg_mood_weights(opts) do
    default = Application.get_env(:brain, :thalamus_mood_weights, %{expl: 0.05, inhib: -0.07, vigil: -0.03, plast: 0.04})
    val = get_opt(opts, :mood_weights, default)

    %{
      expl:  to_small(val[:expl]  || val["expl"]  || 0.05),
      inhib: to_small(val[:inhib] || val["inhib"] || -0.07),
      vigil: to_small(val[:vigil] || val["vigil"] || -0.03),
      plast: to_small(val[:plast] || val["plast"] || 0.04)
    }
  end

  defp to_small(x) when is_number(x), do: x * 1.0
  defp to_small(_), do: 0.0

  # Accepts keyword or map; normalize_opts ensures keyword in state
  defp get_opt(opts, key, default) when is_list(opts), do: Keyword.get(opts, key, default)
  defp get_opt(%{} = opts, key, default),           do: Map.get(opts, key, default)
  defp get_opt(_opts, _key, default),               do: default

  defp to_float_01(x) when is_number(x), do: clamp01(x * 1.0)
  defp to_float_01(_), do: 0.0

  # Don’t use Keyword.keyword?/1 in guards; do it inside.
  defp normalize_opts(opts) when is_list(opts) do
    if Keyword.keyword?(opts), do: opts, else: []
  end
  defp normalize_opts(%{} = opts), do: Map.to_list(opts)
  defp normalize_opts(_), do: []
end

