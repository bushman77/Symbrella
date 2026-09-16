defmodule Brain.DriveLoop do
  @moduledoc """
  DriveLoop: endogenous impulse gate.

  Listens for salience signals from SelfPortrait. When a salience event
  arrives AND the system has been idle longer than the threshold,
  triggers Curiosity to generate an endogenous proposal.

  SelfPortrait detects what's interesting.
  DriveLoop decides whether to act on it.
  """

  use GenServer

  @default_idle_threshold_ms 60_000
  @default_idle_status_interval_ms 0
  @handler_id {__MODULE__, :salience}

  def start_link(opts \\ []), do: GenServer.start_link(__MODULE__, opts, name: __MODULE__)

  def note_input do
    if Process.whereis(__MODULE__) do
      send(__MODULE__, :user_input)
    end
  end

  def configure(opts) when is_list(opts), do: GenServer.call(__MODULE__, {:configure, opts})

  def status, do: GenServer.call(__MODULE__, :status)

  def init(opts) do
    attach_telemetry()

    state =
      %{
        last_user_input: System.monotonic_time(:millisecond),
        idle_threshold_ms: idle_threshold_ms(opts),
        idle_status_interval_ms: idle_status_interval_ms(opts),
        idle_status_timer_ref: nil
      }
      |> schedule_idle_status()

    {:ok, state}
  end

  def terminate(_reason, _state) do
    _ = :telemetry.detach(@handler_id)
    :ok
  end

  # Telemetry handler — called when SelfPortrait emits salience
  def handle_salience(_event, measurements, _metadata, %{pid: pid}) do
    send(pid, {:salience, measurements})
  end

  def handle_info(:user_input, state) do
    {:noreply, %{state | last_user_input: System.monotonic_time(:millisecond)}}
  end

  def handle_info(:idle_status, state) do
    now = System.monotonic_time(:millisecond)
    idle_ms = now - state.last_user_input

    if idle_ms >= state.idle_threshold_ms do
      :telemetry.execute(
        [:brain, :drive_loop, :idle_status],
        %{idle_ms: idle_ms},
        %{
          current: :idle,
          threshold_ms: state.idle_threshold_ms,
          interval_ms: state.idle_status_interval_ms
        }
      )
    end

    state = %{state | idle_status_timer_ref: nil}

    {:noreply, schedule_idle_status(state)}
  end

  def handle_info({:salience, measurements}, state) do
    now = System.monotonic_time(:millisecond)
    idle_ms = now - state.last_user_input

    if idle_ms >= state.idle_threshold_ms do
      maybe_generate_impulse(measurements, idle_ms)
    end

    {:noreply, state}
  end

  defp maybe_generate_impulse(measurements, idle_ms) do
    trace_id = trace_id()

    :telemetry.execute(
      [:brain, :drive_loop, :impulse],
      %{
        idle_ms: idle_ms,
        salience_score: salience_score(measurements)
      },
      %{
        trace_id: trace_id,
        reason: :endogenous,
        salience: measurements
      }
    )

    Brain.Curiosity.nudge(%{
      reason: :endogenous,
      idle_ms: idle_ms,
      salience: measurements,
      trace_id: trace_id,
      id: "#{trace_id}|curiosity"
    })
  end

  defp attach_telemetry do
    case attach_salience_handler() do
      :ok -> :ok
      {:error, :already_exists} -> reattach_salience_handler()
      {:error, reason} -> {:error, reason}
    end
  end

  defp reattach_salience_handler do
    _ = :telemetry.detach(@handler_id)
    attach_salience_handler()
  end

  defp attach_salience_handler do
    :telemetry.attach(
      @handler_id,
      [:brain, :self_portrait, :salience],
      &__MODULE__.handle_salience/4,
      %{pid: self()}
    )
  end

  def handle_call(:status, _from, state) do
    {:reply, {:ok, state}, state}
  end

  def handle_call({:configure, opts}, _from, state) do
    state =
      state
      |> cancel_idle_status()
      |> Map.merge(%{
        idle_threshold_ms: idle_threshold_ms(opts),
        idle_status_interval_ms: idle_status_interval_ms(opts)
      })
      |> schedule_idle_status()

    {:reply, :ok, state}
  end

  defp idle_threshold_ms(opts) when is_list(opts) do
    cfg = Application.get_env(:brain, __MODULE__, [])

    opts
    |> Keyword.get(
      :idle_threshold_ms,
      Keyword.get(cfg, :idle_threshold_ms, @default_idle_threshold_ms)
    )
    |> normalize_idle_threshold_ms()
  end

  defp idle_threshold_ms(_opts), do: idle_threshold_ms([])

  defp normalize_idle_threshold_ms(ms) when is_integer(ms) and ms >= 0, do: ms
  defp normalize_idle_threshold_ms(_), do: @default_idle_threshold_ms

  defp idle_status_interval_ms(opts) when is_list(opts) do
    cfg = Application.get_env(:brain, __MODULE__, [])

    opts
    |> Keyword.get(
      :idle_status_interval_ms,
      Keyword.get(cfg, :idle_status_interval_ms, @default_idle_status_interval_ms)
    )
    |> normalize_idle_status_interval_ms()
  end

  defp idle_status_interval_ms(_opts), do: idle_status_interval_ms([])

  defp normalize_idle_status_interval_ms(ms) when is_integer(ms) and ms >= 0, do: ms
  defp normalize_idle_status_interval_ms(_), do: @default_idle_status_interval_ms

  defp schedule_idle_status(%{idle_status_interval_ms: ms} = state) when ms > 0 do
    ref = Process.send_after(self(), :idle_status, ms)
    %{state | idle_status_timer_ref: ref}
  end

  defp schedule_idle_status(state), do: state

  defp cancel_idle_status(%{idle_status_timer_ref: ref} = state) when is_reference(ref) do
    _ = Process.cancel_timer(ref)
    %{state | idle_status_timer_ref: nil}
  end

  defp cancel_idle_status(state), do: state

  defp salience_score(measurements) when is_map(measurements) do
    [
      number(Map.get(measurements, :lifg_anomalies, 0)),
      number(Map.get(measurements, :boundary_drops, 0)) / 3.0,
      number(Map.get(measurements, :fallback_wins, 0)),
      number(Map.get(measurements, :gate_failures, 0)),
      number(Map.get(measurements, :curiosity_bias, 0.5))
    ]
    |> Enum.max()
    |> clamp01()
  end

  defp salience_score(_), do: 0.0

  defp trace_id do
    unique = System.unique_integer([:positive, :monotonic])
    "agency|endogenous|#{unique}"
  end

  defp number(value) when is_integer(value), do: value * 1.0
  defp number(value) when is_float(value), do: value
  defp number(_), do: 0.0

  defp clamp01(value) when is_number(value), do: value |> max(0.0) |> min(1.0)
  defp clamp01(_), do: 0.0
end
