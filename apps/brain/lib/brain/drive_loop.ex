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

  @idle_threshold_ms 60_000

  def start_link(_), do: GenServer.start_link(__MODULE__, [], name: __MODULE__)

  def note_input do
    if Process.whereis(__MODULE__) do
      send(__MODULE__, :user_input)
    end
  end

  def init(_) do
    attach_telemetry()
    {:ok, %{last_user_input: System.monotonic_time(:millisecond)}}
  end

  # Telemetry handler — called when SelfPortrait emits salience
  def handle_salience(_event, measurements, _metadata, %{pid: pid}) do
    send(pid, {:salience, measurements})
  end

  def handle_info(:user_input, state) do
    {:noreply, %{state | last_user_input: System.monotonic_time(:millisecond)}}
  end

  def handle_info({:salience, measurements}, state) do
    now = System.monotonic_time(:millisecond)
    idle_ms = now - state.last_user_input

    if idle_ms > @idle_threshold_ms do
      maybe_generate_impulse(measurements, idle_ms)
    end

    {:noreply, state}
  end

  defp maybe_generate_impulse(measurements, idle_ms) do
    # SelfPortrait already decided this is salient.
    # We just pass it through to Curiosity.
    Brain.Curiosity.nudge(%{
      reason: :endogenous,
      idle_ms: idle_ms,
      salience: measurements
    })
  end

  defp attach_telemetry do
    :telemetry.attach(
      {__MODULE__, :salience},
      [:brain, :self_portrait, :salience],
      &__MODULE__.handle_salience/4,
      %{pid: self()}
    )
  end
end
