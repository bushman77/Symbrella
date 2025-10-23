defmodule Brain.ACC do
  @moduledoc """
  ACC — conflict/uncertainty monitor, modulated by mood, with decay.

  Listens:
    • [:curiosity, :proposal]   (optional fields: uncertainty, risk, entropy)
    • [:brain, :mood, :update]  (Expl/Inhib/Vigil/Plast snapshot)
    • [:brain, :cycle, :tick]   (for clock-coupled decay)

  Emits:
    • [:brain, :acc, :conflict]
      measurements: %{conflict: 0.0..1.0}
      metadata: %{cause: :tick | :proposal, mood_snapshot: map() | nil, weights: map() | nil, halflife_ms: non_neg_integer(), v: 2}

  Public API:
    set_params/2, get_params/1
  """

  use Brain, region: :acc
  require Logger

  @cur_handler_prefix  "brain-acc-curiosity-"
  @mood_handler_prefix "brain-acc-mood-"
  @clk_handler_prefix  "brain-acc-clock-"

  @impl GenServer
  def init(opts) do
    opts_kw = normalize_opts(opts)

    state = %{
      region: :acc,
      opts: opts_kw,
      mood: nil,
      conflict: 0.0,
      last_ts: now_ms(),
      # predefine so map updates can never crash
      cur_handler: nil,
      mood_handler: nil,
      clk_handler: nil
    }

    cur_id  = unique(@cur_handler_prefix)
    mood_id = unique(@mood_handler_prefix)
    clk_id  = unique(@clk_handler_prefix)

    :ok = :telemetry.attach(cur_id,  [:curiosity, :proposal],  &__MODULE__.on_curiosity/4,  %{pid: self()})
    :ok = :telemetry.attach(mood_id, [:brain, :mood, :update], &__MODULE__.on_mood_update/4, %{pid: self()})
    :ok = :telemetry.attach(clk_id,  [:brain, :cycle, :tick],  &__MODULE__.on_tick/4,        %{pid: self()})

    {:ok,
      state
      |> Map.put(:cur_handler, cur_id)
      |> Map.put(:mood_handler, mood_id)
      |> Map.put(:clk_handler, clk_id)
    }
  end

  @impl GenServer
  def terminate(_reason, %{cur_handler: cur, mood_handler: mood, clk_handler: clk}) do
    if is_binary(cur),  do: :telemetry.detach(cur)
    if is_binary(mood), do: :telemetry.detach(mood)
    if is_binary(clk),  do: :telemetry.detach(clk)
    :ok
  end
  def terminate(_, _), do: :ok

  # ---- Public API -----------------------------------------------------------

  @doc """
  Adjust weights/half-life/caps live.

  Keys:
    :halflife_ms       (default 8_000)
    :proposal_alpha    (default 0.6)
    :weights           %{vigil: +0.35, inhib: +0.30, expl: -0.20, plast: -0.10}
    :cap_per_proposal  (default 0.20)
  """
  def set_params(server \\ __MODULE__, opts) when is_list(opts) or is_map(opts),
    do: GenServer.call(server, {:set_params, opts})

  @doc "Returns effective params."
  def get_params(server \\ __MODULE__), do: GenServer.call(server, :get_params)

  # ---- Telemetry bridges ----------------------------------------------------

  def on_curiosity(_e, meas, meta, %{pid: pid}) when is_pid(pid),
    do: send(pid, {:proposal, meas, meta})
  def on_curiosity(_,_,_,_), do: :ok

  def on_mood_update(_e, meas, meta, %{pid: pid}) when is_pid(pid),
    do: send(pid, {:mood, meas, meta})
  def on_mood_update(_,_,_,_), do: :ok

  def on_tick(_e, meas, _meta, %{pid: pid}) when is_pid(pid),
    do: send(pid, {:tick, (meas[:dt_ms] || 0)})
  def on_tick(_,_,_,_), do: :ok

  # ---- GenServer callbacks --------------------------------------------------

  @impl GenServer
  def handle_call({:set_params, opts_in}, _from, state) do
    opts_norm = normalize_opts(opts_in)
    {:reply, :ok, %{state | opts: Keyword.merge(state.opts, opts_norm)}}
  end

  @impl GenServer
  def handle_call(:get_params, _from, state) do
    {:reply, effective_params(state.opts), state}
  end

  @impl GenServer
  def handle_info({:mood, meas, _meta}, state) do
    mood = %{
      exploration: get_num(meas, :exploration, 0.5) |> clamp01(),
      inhibition:  get_num(meas, :inhibition,  0.5) |> clamp01(),
      vigilance:   get_num(meas, :vigilance,   0.5) |> clamp01(),
      plasticity:  get_num(meas, :plasticity,  0.5) |> clamp01()
    }
    {:noreply, %{state | mood: mood}}
  end

  @impl GenServer
  def handle_info({:tick, _dt_ms}, state) do
    %{halflife_ms: hl} = effective_params(state.opts)

    now = now_ms()
    dt  = max(now - state.last_ts, 0)
    fac = :math.pow(0.5, dt / max(hl, 1.0))

    conflict = 0.5 + (state.conflict - 0.5) * fac
    st = %{state | conflict: conflict, last_ts: now}

    :telemetry.execute(
      [:brain, :acc, :conflict],
      %{conflict: conflict},
      %{cause: :tick, mood_snapshot: state.mood, weights: nil, halflife_ms: hl, v: 2}
    )

    {:noreply, st}
  end

  @impl GenServer
  def handle_info({:proposal, meas, _meta}, state) do
    %{
      proposal_alpha: alpha,
      cap_per_proposal: cap,
      weights: w,
      halflife_ms: hl
    } = effective_params(state.opts)

    # If uncertainty/entropy are absent, risk becomes the fallback (possibly 0.0).
    obs =
      (get_num(meas, :uncertainty, nil) ||
       get_num(meas, :entropy,    nil) ||
       get_num(meas, :risk,       0.0))
      |> clamp01()

    dx = if state.mood do
      %{
        vigil: (state.mood.vigilance   - 0.5),
        inhib: (state.mood.inhibition  - 0.5),
        expl:  (state.mood.exploration - 0.5),
        plast: (state.mood.plasticity  - 0.5)
      }
    else
      %{vigil: 0.0, inhib: 0.0, expl: 0.0, plast: 0.0}
    end

    mood_term =
      dx.vigil * w.vigil +
      dx.inhib * w.inhib +
      dx.expl  * w.expl  +
      dx.plast * w.plast

    target = clamp01(obs + max(-cap, min(cap, mood_term)))
    conflict = clamp01((1.0 - alpha) * state.conflict + alpha * target)

    st = %{state | conflict: conflict, last_ts: now_ms()}

    :telemetry.execute(
      [:brain, :acc, :conflict],
      %{conflict: conflict},
      %{cause: :proposal, mood_snapshot: state.mood, weights: w, halflife_ms: hl, v: 2}
    )

    {:noreply, st}
  end

  @impl GenServer
  def handle_info(_, state), do: {:noreply, state}

  # ---- Params & helpers -----------------------------------------------------

  defp effective_params(opts) do
    %{
      halflife_ms:       to_ms(get_opt(opts, :halflife_ms,       Application.get_env(:brain, :acc_halflife_ms, 8_000))),
      proposal_alpha:    to_small(get_opt(opts, :proposal_alpha,  Application.get_env(:brain, :acc_proposal_alpha, 0.6))),
      cap_per_proposal:  to_cap(get_opt(opts, :cap_per_proposal,  Application.get_env(:brain, :acc_cap_per_proposal, 0.20))),
      weights:           to_w(get_opt(opts, :weights,            Application.get_env(:brain, :acc_weights, %{vigil: 0.35, inhib: 0.30, expl: -0.20, plast: -0.10})))
    }
  end

  defp to_w(val) do
    %{
      vigil: to_small(val[:vigil] || val["vigil"] || 0.35),
      inhib: to_small(val[:inhib] || val["inhib"] || 0.30),
      expl:  to_small(val[:expl]  || val["expl"]  || -0.20),
      plast: to_small(val[:plast] || val["plast"] || -0.10)
    }
  end

  defp to_small(x) when is_number(x), do: x * 1.0
  defp to_small(_), do: 0.0

  defp to_cap(x) when is_number(x), do: max(0.0, min(1.0, x * 1.0))
  defp to_cap(_), do: 0.20

  defp to_ms(x) when is_integer(x) and x >= 0, do: x
  defp to_ms(x) when is_float(x) and x >= 0.0, do: trunc(x)
  defp to_ms(_), do: 8_000

  defp get_opt(opts, key, default) when is_list(opts), do: Keyword.get(opts, key, default)
  defp get_opt(%{} = opts, key, default),           do: Map.get(opts, key, default)
  defp get_opt(_, _, default),                      do: default

  # ---- Hardened numeric getter (nil-safe default + string/boolean coercion) ----
  #
  # Returns:
  #   - float() when a numeric can be coerced
  #   - nil when both the source and default are non-numeric and default is nil
  #
  defp get_num(map, key, default) do
    val =
      case map do
        %{} ->
          case {Map.get(map, key), Map.get(map, to_string(key))} do
            {v, _} when is_integer(v) -> v * 1.0
            {v, _} when is_float(v)   -> v
            {v, _} when is_boolean(v) -> if v, do: 1.0, else: 0.0
            {v, _} when is_binary(v)  ->
              case Float.parse(v) do
                {f, _} -> f
                :error -> :nope
              end
            {_, v} when is_integer(v) -> v * 1.0
            {_, v} when is_float(v)   -> v
            {_, v} when is_boolean(v) -> if v, do: 1.0, else: 0.0
            {_, v} when is_binary(v)  ->
              case Float.parse(v) do
                {f, _} -> f
                :error -> :nope
              end
            _ -> :nope
          end

        _ -> :nope
      end

    case val do
      :nope ->
        coerce_default(default)

      num when is_number(num) ->
        num * 1.0
    end
  end

  # default may be number | boolean | numeric string | nil | anything
  defp coerce_default(nil), do: nil
  defp coerce_default(true), do: 1.0
  defp coerce_default(false), do: 0.0
  defp coerce_default(d) when is_integer(d), do: d * 1.0
  defp coerce_default(d) when is_float(d),   do: d
  defp coerce_default(d) when is_binary(d) do
    case Float.parse(d) do
      {f, _} -> f
      :error -> nil
    end
  end
  defp coerce_default(_), do: nil

  defp clamp01(x) when is_number(x), do: max(0.0, min(1.0, x * 1.0))
  defp clamp01(_), do: 0.0

  defp unique(prefix),
    do:
      prefix <>
        Integer.to_string(:erlang.unique_integer([:positive])) <>
        "-" <> Integer.to_string(System.system_time(:microsecond))

  defp now_ms(), do: System.monotonic_time(:millisecond)

  defp normalize_opts(opts) when is_list(opts) do
    if Keyword.keyword?(opts), do: opts, else: []
  end
  defp normalize_opts(%{} = opts), do: Map.to_list(opts)
  defp normalize_opts(_), do: []
end

