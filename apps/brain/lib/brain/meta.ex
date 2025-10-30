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

  @ev_conf      [:brain, :lifg, :confidence]
  @ev_conflict  [:brain, :acc, :conflict]
  @ev_reasons   [:brain, :lifg, :reasons]

  @impl true
  def init(opts) do
    st = %{
      region: :meta,
      conf: 1.0,
      conflict: 0.0,
      surprise: 0.0,
      last_reasons: [],
      overrides: Map.get(opts, :overrides, %{})
    }

    # Attach telemetry listeners (unique handler names)
    :ok = attach(@ev_conf,     &__MODULE__.on_confidence/4)
    :ok = attach(@ev_conflict, &__MODULE__.on_conflict/4)
    :ok = attach(@ev_reasons,  &__MODULE__.on_reasons/4)

    {:ok, st}
  end

  defp attach(event, fun) do
    name = {__MODULE__, event, make_ref()}
    case :telemetry.attach(name, event, fun, %{}) do
      :ok -> :ok
      {:error, _} -> :ok
    end
  end

  @impl true
  def handle_call(:status, _from, state),
    do: {:reply, Map.take(state, [:conf, :conflict, :surprise, :last_reasons, :region]), state}

  @impl true
  def handle_call(:reasons, _from, state),
    do: {:reply, state.last_reasons, state}

  # ——— Telemetry handlers ———

  @doc false
  def on_confidence(_event, %{value: v}, _meta, state_or_pid) do
    update_state(state_or_pid, fn st -> %{st | conf: clamp01(as_float(v))} end)
  end

  @doc false
  def on_conflict(_event, %{value: v}, _meta, state_or_pid) do
    update_state(state_or_pid, fn st -> %{st | conflict: clamp01(as_float(v))} end)
  end

  @doc false
  def on_reasons(_event, _meas, %{reasons: reasons} = _meta, state_or_pid) when is_list(reasons) do
    min_margin =
      reasons
      |> Enum.map(&Kernel.get_in(&1, [:margin]))
      |> Enum.reject(&is_nil/1)
      |> case do
        [] -> 1.0
        ms -> Enum.min(ms) * 1.0
      end

    surprise = clamp01(1.0 - min_margin)

    update_state(state_or_pid, fn st ->
      %{st | last_reasons: reasons, surprise: surprise}
    end)
  end

  def on_reasons(_e, _m, _meta, _state_or_pid), do: :ok

  defp update_state(%{} = st, fun) when is_function(fun, 1), do: fun.(st)
  defp update_state(pid, fun) when is_pid(pid), do: GenServer.cast(__MODULE__, {:__update, fun})
  defp update_state(_other, _fun), do: :ok

  @impl true
  def handle_cast({:__update, fun}, st) when is_function(fun, 1) do
    {:noreply, fun.(st)}
  end

  # ——— Public helpers ———

  @spec status() :: map()
  def status, do: GenServer.call(__MODULE__, :status)

  @spec reasons() :: list()
  def reasons, do: GenServer.call(__MODULE__, :reasons)

  # ——— Small utils ———
  defp as_float(x) when is_integer(x), do: x * 1.0
  defp as_float(x) when is_float(x), do: x
  defp as_float(_), do: 0.0
  defp clamp01(x) when is_number(x), do: max(0.0, min(1.0, x))
  defp clamp01(_), do: 0.0
end

