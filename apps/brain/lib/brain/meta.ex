defmodule Brain.Meta do
  @moduledoc """
  Metacognition monitor (lightweight).

  Tracks:
    • lifg confidence (lower = need help)
    • acc conflict (higher = competing interpretations)
    • surprise (1 - min margin across winners)
    • last reasons (token_index, chosen_id, top_alts, margin)

  Public API:
    Brain.Meta.status()  -> %{conf, conflict, surprise, last_reasons}
    Brain.Meta.reasons() -> last_reasons
  """

  use Brain, region: :meta
  require Logger

  # emits %{value: conf}
  @ev_conf [:brain, :lifg, :confidence]
  # emits %{conflict: c} (our ACC)
  @ev_conflict [:brain, :acc, :conflict]
  # meta: %{reasons: [...]}
  @ev_reasons [:brain, :lifg, :reasons]

  @impl true
  def init(opts) do
    st = %{
      region: :meta,
      conf: 1.0,
      conflict: 0.0,
      surprise: 0.0,
      last_reasons: [],
      overrides: Map.get(opts, :overrides, %{}),
      h_conf: nil,
      h_conflict: nil,
      h_reasons: nil
    }

    # Attach telemetry listeners with the server pid in config
    h_conf = attach(@ev_conf, &__MODULE__.on_confidence/4, self())
    h_conflict = attach(@ev_conflict, &__MODULE__.on_conflict/4, self())
    h_reasons = attach(@ev_reasons, &__MODULE__.on_reasons/4, self())

    {:ok, %{st | h_conf: h_conf, h_conflict: h_conflict, h_reasons: h_reasons}}
  end

  defp attach(event, fun, pid) do
    name = {__MODULE__, event, make_ref()}

    case :telemetry.attach(name, event, fun, %{pid: pid}) do
      :ok -> name
      {:error, _} -> name
    end
  end

  @impl true
  def terminate(_reason, %{h_conf: hc, h_conflict: hk, h_reasons: hr}) do
    for h <- [hc, hk, hr], do: :telemetry.detach(h)
    :ok
  end

  def terminate(_reason, _), do: :ok

  @impl true
  def handle_call(:status, _from, state) do
    reply = Map.take(state, [:conf, :conflict, :surprise, :last_reasons, :region])
    {:reply, reply, state}
  end

  @impl true
  def handle_call(:reasons, _from, state), do: {:reply, state.last_reasons, state}

  @impl true
  def handle_cast({:__update, fun}, st) when is_function(fun, 1), do: {:noreply, fun.(st)}

  # ——— Telemetry handlers ———

  @doc false
  def on_confidence(_event, meas, _meta, %{pid: pid}) when is_pid(pid) do
    v = pick_num(meas, [:value, :conf, :confidence])

    if is_number(v) do
      GenServer.cast(pid, {:__update, fn st -> %{st | conf: clamp01(as_float(v))} end})
    end

    :ok
  end

  def on_confidence(_, _, _, _), do: :ok

  @doc false
  def on_conflict(_event, meas, _meta, %{pid: pid}) when is_pid(pid) do
    # Accept both our ACC shape (%{conflict: ...}) and generic %{value: ...}
    v = pick_num(meas, [:conflict, :value])

    if is_number(v) do
      GenServer.cast(pid, {:__update, fn st -> %{st | conflict: clamp01(as_float(v))} end})
    end

    :ok
  end

  def on_conflict(_, _, _, _), do: :ok

  @doc false
  def on_reasons(_event, _meas, %{reasons: reasons}, %{pid: pid})
      when is_pid(pid) and is_list(reasons) do
    min_margin =
      reasons
      |> Enum.map(&Kernel.get_in(&1, [:margin]))
      |> Enum.reject(&is_nil/1)
      |> case do
        [] -> 1.0
        ms -> Enum.min(ms) * 1.0
      end

    surprise = clamp01(1.0 - min_margin)

    GenServer.cast(
      pid,
      {:__update,
       fn st ->
         %{st | last_reasons: reasons, surprise: surprise}
       end}
    )

    :ok
  end

  def on_reasons(_, _, _, _), do: :ok

  # ——— Public helpers ———
  @spec status() :: map()
  def status, do: GenServer.call(__MODULE__, :status)

  @spec reasons() :: list()
  def reasons, do: GenServer.call(__MODULE__, :reasons)

  # ——— Small utils ———
  defp pick_num(map, keys) when is_map(map) and is_list(keys) do
    Enum.find_value(keys, fn k ->
      case Map.get(map, k) do
        x when is_integer(x) ->
          x * 1.0

        x when is_float(x) ->
          x

        x when is_binary(x) ->
          case Float.parse(x) do
            {f, _} -> f
            _ -> nil
          end

        _ ->
          nil
      end
    end)
  end

  defp pick_num(_, _), do: nil

  defp as_float(x) when is_integer(x), do: x * 1.0
  defp as_float(x) when is_float(x), do: x
  defp as_float(_), do: 0.0

  defp clamp01(x) when is_number(x), do: max(0.0, min(1.0, x))
  defp clamp01(_), do: 0.0
end
