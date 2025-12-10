defmodule Brain.Blackboard do
  @moduledoc """
  Central Brain event sink/source.

  Responsibilities:
  - Subscribe to the "brain:blackboard" PubSub topic (via Brain.Bus) and keep the last payload.
  - Bridge selected telemetry events -> PubSub messages so the UI (and any listeners)
    can react in real time with a single subscription.

  Notes:
  - Uses a remote function capture for telemetry handlers (avoids "local function" perf warning).
  - Subscribes after init with retry to avoid boot-order races.
  """

  use GenServer
  require Logger

  alias Brain.Bus

  @topic "brain:blackboard"
  @retry_ms 50

  # Global, stable telemetry handler ID (avoid duplicates across restarts)
  @telemetry_id "brain-blackboard-telemetry-bridge"

  # Keep this list small + high-signal. Add more as needed.
  @telemetry_events [
    [:brain, :wm, :update],
    [:brain, :pipeline, :lifg_stage1, :stop],
    [:brain, :pfc, :policy],
    [:brain, :pfc, :status],

    # Optional but ML-useful:
    [:brain, :lifg, :confidence],
    [:brain, :intent, :selected],
    [:core, :intent, :selected],

    # SelfPortrait-friendly signals (compact, high-signal)
    [:brain, :lifg, :stage1, :winner],
    [:brain, :lifg, :stage1, :boundary_drop],
    [:brain, :lifg, :stage1, :chargram_violation],
    [:brain, :pmtg, :consult],
    [:brain, :pmtg, :no_mwe_senses]
  ]

  # ——— Public API ———

  @doc "Child-spec friendly start_link."
  def start_link(init_arg \\ []) do
    GenServer.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  @doc "Returns the current internal state snapshot."
  def state, do: GenServer.call(__MODULE__, :state)

  @doc "Broadcast a blackboard update to subscribers."
  def publish(message), do: safe_broadcast({:blackboard, message})

  @doc "PubSub topic for this blackboard."
  def topic, do: @topic

  # ——— GenServer ———

  @impl true
  def init(init_arg) do
    # Defer subscription so we don't crash if PubSub hasn't started yet.
    send(self(), :subscribe_pubsub)
    # Attach telemetry bridge quickly; publishing is guarded if PubSub isn't ready yet.
    send(self(), :attach_telemetry)

    {:ok,
     %{
       init_arg: init_arg,
       subscribed?: false,
       telemetry_attached?: false,
       last: nil
     }}
  end

  @impl true
  def handle_info(:subscribe_pubsub, %{subscribed?: false} = state) do
    case safe_subscribe() do
      :ok ->
        Logger.info(fn -> "[Blackboard] subscribed to #{@topic} on #{inspect(Bus.name())}" end)
        {:noreply, %{state | subscribed?: true}}

      {:error, reason} ->
        Logger.warning(fn ->
          "[Blackboard] PubSub not ready (#{inspect(reason)}); retrying in #{@retry_ms}ms"
        end)

        Process.send_after(self(), :subscribe_pubsub, @retry_ms)
        {:noreply, state}
    end
  end

  @impl true
  def handle_info(:attach_telemetry, %{telemetry_attached?: false} = state) do
    case safe_attach_telemetry() do
      :ok ->
        {:noreply, %{state | telemetry_attached?: true}}

      {:error, :already_exists} ->
        # Another instance/test might have attached already; treat as OK.
        {:noreply, %{state | telemetry_attached?: true}}

      {:error, reason} ->
        Logger.warning(fn ->
          "[Blackboard] telemetry attach failed (#{inspect(reason)}); retrying in #{@retry_ms}ms"
        end)

        Process.send_after(self(), :attach_telemetry, @retry_ms)
        {:noreply, state}
    end
  end

  # Generic PubSub handler; stores last payload for introspection.
  @impl true
  def handle_info({:blackboard, payload}, state) do
    {:noreply, %{state | last: payload}}
  end

  # Catch-all so unexpected messages don't crash the server.
  @impl true
  def handle_info(_msg, state), do: {:noreply, state}

  @impl true
  def handle_call(:state, _from, state), do: {:reply, state, state}

  @impl true
  def terminate(_reason, _state) do
    # Best-effort detach (safe if missing)
    _ = safe_detach_telemetry()
    :ok
  end

  # ——— Telemetry → Blackboard bridge ———
  #
  # Avoid "local function" warnings by using a remote function capture:
  #   &__MODULE__.handle_telemetry/4
  #
  def handle_telemetry(event, measurements, meta, _config) do
    msg = %{
      kind: :telemetry,
      event: event,
      measurements: measurements,
      meta: meta,
      at_ms: System.system_time(:millisecond)
    }

    safe_broadcast({:blackboard, msg})
    :ok
  end

  # ——— Helpers ———

  defp safe_subscribe do
    try do
      :ok = Bus.subscribe(@topic)
    rescue
      e -> {:error, e}
    catch
      :exit, reason -> {:error, reason}
    end
  end

  defp safe_attach_telemetry do
    try do
      :telemetry.attach_many(
        @telemetry_id,
        @telemetry_events,
        &__MODULE__.handle_telemetry/4,
        nil
      )
    rescue
      e -> {:error, e}
    catch
      :exit, reason -> {:error, reason}
    end
  end

  defp safe_detach_telemetry do
    try do
      :telemetry.detach(@telemetry_id)
      :ok
    rescue
      _ -> :ok
    catch
      _, _ -> :ok
    end
  end

  defp safe_broadcast(message) do
    try do
      Bus.broadcast(@topic, message)
      :ok
    rescue
      _ -> :ok
    catch
      _, _ -> :ok
    end
  end
end

