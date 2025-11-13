defmodule Brain.OFC do
  @moduledoc """
  OFC — value estimation for proposals, gently shaped by mood.

  Listens:
    • [:curiosity, :proposal]   (base inputs: score/risk/novelty/etc.)
    • [:brain, :mood, :update]  (Expl/Inhib/Vigil/Plast snapshot)

  Emits:
    • [:brain, :ofc, :value]
      measurements: %{value: 0.0..1.0}
      metadata: %{
        probe_id: String.t(),
        source:   term(),
        mood_snapshot: %{exploration:, inhibition:, vigilance:, plasticity:} | nil,
        mood_weights: %{expl:, inhib:, vigil:, plast:},
        mood_cap: float(),
        risk_weight: float(),
        novelty_weight: float(),
        v: 2
      }

  Public API:
    set_params/2, get_params/1  (adjust weights live)
  """

  use Brain, region: :ofc
  require Logger

  @cur_handler_prefix "brain-ofc-curiosity-"
  @mood_handler_prefix "brain-ofc-mood-"

  @impl GenServer
  def init(opts) do
    opts_kw = normalize_opts(opts)

    state = %{
      region: :ofc,
      opts: opts_kw,
      # cached %{exploration:, inhibition:, vigilance:, plasticity:}
      mood: nil,
      mood_last_ms: nil
    }

    cur_id = unique(@cur_handler_prefix)
    mood_id = unique(@mood_handler_prefix)

    :ok =
      :telemetry.attach(cur_id, [:curiosity, :proposal], &__MODULE__.on_curiosity/4, %{
        pid: self()
      })

    :ok =
      :telemetry.attach(mood_id, [:brain, :mood, :update], &__MODULE__.on_mood_update/4, %{
        pid: self()
      })

    {:ok,
     state
     |> Map.put(:cur_handler, cur_id)
     |> Map.put(:mood_handler, mood_id)}
  end

  @impl GenServer
  def terminate(_reason, %{cur_handler: cur, mood_handler: mood}) do
    if is_binary(cur), do: :telemetry.detach(cur)
    if is_binary(mood), do: :telemetry.detach(mood)
    :ok
  end

  def terminate(_, _), do: :ok

  # ---- Public API -----------------------------------------------------------

  @doc """
  Update runtime parameters.

  Accepted keys:
    :risk_weight       (default 0.35)
    :novelty_weight    (default 0.15)
    :mood_cap          (default 0.12)   # bounds mood multiplier around 1.0
    :mood_weights      (map of expl/inhib/vigil/plast; small magnitudes)
  """
  def set_params(server \\ __MODULE__, opts) when is_list(opts) or is_map(opts),
    do: GenServer.call(server, {:set_params, opts})

  @doc "Returns the effective params map."
  def get_params(server \\ __MODULE__),
    do: GenServer.call(server, :get_params)

  # ---- Telemetry bridges ----------------------------------------------------

  def on_curiosity(_ev, meas, meta, %{pid: pid}) when is_pid(pid),
    do: send(pid, {:curiosity, meas, meta})

  def on_curiosity(_, _, _, _), do: :ok

  def on_mood_update(_ev, meas, meta, %{pid: pid}) when is_pid(pid),
    do: send(pid, {:mood, meas, meta})

  def on_mood_update(_, _, _, _), do: :ok

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
      inhibition: get_num(meas, :inhibition, 0.5) |> clamp01(),
      vigilance: get_num(meas, :vigilance, 0.5) |> clamp01(),
      plasticity: get_num(meas, :plasticity, 0.5) |> clamp01()
    }

    {:noreply, %{state | mood: mood, mood_last_ms: System.system_time(:millisecond)}}
  end

  @impl GenServer
  def handle_info({:curiosity, meas, meta}, state) do
    # Base features
    base_score = get_num(meas, :score, 0.0) |> clamp01()
    risk = get_num(meas, :risk, 0.0) |> clamp01()
    novelty = get_num(meas, :novelty, 0.0) |> clamp01()

    probe_id =
      meta_get(meta, :probe_id, nil) ||
        meta_get(meta, :probe, %{})
        |> case do
          %{} = p -> p[:id] || p["id"]
          _ -> nil
        end

    probe_id = to_string(probe_id || "curiosity|probe|unknown")
    source = meta_get(meta, :source, nil)

    # Params
    %{
      risk_weight: rw,
      novelty_weight: nw,
      mood_cap: cap,
      mood_weights: mw
    } = effective_params(state.opts)

    # Base valuation (risk reduces; novelty adds a bit)
    base_val =
      base_score
      |> Kernel.*(1.0 - rw * risk)
      |> Kernel.+(nw * novelty)
      |> clamp01()

    # Mood factor
    {factor, w_used, mood_snapshot} = mood_factor(state.mood, mw, cap)

    value = clamp01(base_val * factor)

    :telemetry.execute(
      [:brain, :ofc, :value],
      %{value: value},
      %{
        probe_id: probe_id,
        source: source,
        mood_snapshot: mood_snapshot,
        mood_weights: w_used,
        mood_cap: cap,
        risk_weight: rw,
        novelty_weight: nw,
        v: 2
      }
    )

    {:noreply, state}
  end

  @impl GenServer
  def handle_info(_, state), do: {:noreply, state}

  # ---- Math & helpers -------------------------------------------------------

  defp mood_factor(nil, weights, _cap), do: {1.0, weights, nil}

  defp mood_factor(mood, weights, cap) do
    dx = %{
      expl: Map.get(mood, :exploration, 0.5) - 0.5,
      inhib: Map.get(mood, :inhibition, 0.5) - 0.5,
      vigil: Map.get(mood, :vigilance, 0.5) - 0.5,
      plast: Map.get(mood, :plasticity, 0.5) - 0.5
    }

    raw =
      dx.expl * (weights.expl || 0.0) +
        dx.inhib * (weights.inhib || 0.0) * -1.0 +
        dx.vigil * (weights.vigil || 0.0) * -1.0 +
        dx.plast * (weights.plast || 0.0)

    factor = 1.0 + max(-cap, min(cap, raw))
    {factor, weights, mood}
  end

  defp effective_params(opts) do
    %{
      risk_weight:
        to_small(get_opt(opts, :risk_weight, Application.get_env(:brain, :ofc_risk_weight, 0.35))),
      novelty_weight:
        to_small(
          get_opt(opts, :novelty_weight, Application.get_env(:brain, :ofc_novelty_weight, 0.15))
        ),
      mood_cap:
        to_cap(get_opt(opts, :mood_cap, Application.get_env(:brain, :ofc_mood_cap, 0.12))),
      mood_weights:
        to_mw(
          get_opt(
            opts,
            :mood_weights,
            Application.get_env(:brain, :ofc_mood_weights, %{
              expl: 0.06,
              inhib: 0.07,
              vigil: 0.04,
              plast: 0.05
            })
          )
        )
    }
  end

  defp to_mw(val) do
    %{
      expl: to_small(val[:expl] || val["expl"] || 0.06),
      inhib: to_small(val[:inhib] || val["inhib"] || 0.07),
      vigil: to_small(val[:vigil] || val["vigil"] || 0.04),
      plast: to_small(val[:plast] || val["plast"] || 0.05)
    }
  end

  defp to_small(x) when is_number(x), do: x * 1.0
  defp to_small(_), do: 0.0

  defp to_cap(x) when is_number(x), do: max(0.0, min(1.0, x * 1.0))
  defp to_cap(_), do: 0.12

  defp unique(prefix),
    do:
      prefix <>
        Integer.to_string(:erlang.unique_integer([:positive])) <>
        "-" <> Integer.to_string(System.system_time(:microsecond))

  defp get_opt(opts, key, default) when is_list(opts), do: Keyword.get(opts, key, default)
  defp get_opt(%{} = opts, key, default), do: Map.get(opts, key, default)
  defp get_opt(_, _, default), do: default

  defp get_num(map, key, default) do
    case {Map.get(map, key), Map.get(map, to_string(key))} do
      {v, _} when is_integer(v) -> v * 1.0
      {v, _} when is_float(v) -> v
      {_, v} when is_integer(v) -> v * 1.0
      {_, v} when is_float(v) -> v
      _ -> default * 1.0
    end
  end

  defp meta_get(meta, key, default) do
    case {Map.get(meta, key), Map.get(meta, to_string(key))} do
      {nil, nil} -> default
      {nil, v} -> v
      {v, _} -> v
    end
  end

  defp clamp01(x) when is_number(x), do: max(0.0, min(1.0, x * 1.0))
  defp clamp01(_), do: 0.0

  defp normalize_opts(opts) when is_list(opts) do
    if Keyword.keyword?(opts), do: opts, else: []
  end

  defp normalize_opts(%{} = opts), do: Map.to_list(opts)
  defp normalize_opts(_), do: []
end
