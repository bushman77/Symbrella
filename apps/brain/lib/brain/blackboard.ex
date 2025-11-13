defmodule Brain.Blackboard do
  @moduledoc """
  Central Brain event sink/source.

  Changes in this version:
  - Uses Brain.Bus for all PubSub interactions (no hard-coded bus name).
  - Subscribes after init via a retrying :subscribe_pubsub message to avoid boot-order races.
  - Provides a minimal public API for introspection and publishing.
  """

  use GenServer
  require Logger

  alias Brain.Bus

  @topic "brain:blackboard"
  @retry_ms 50

  # ——— Public API ———

  @doc "Child-spec friendly start_link."
  def start_link(init_arg \\ []) do
    GenServer.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  @doc "Returns the current internal state snapshot."
  def state, do: GenServer.call(__MODULE__, :state)

  @doc "Broadcast a blackboard update to subscribers."
  def publish(message), do: Bus.broadcast(@topic, {:blackboard, message})

  @doc "PubSub topic for this blackboard."
  def topic, do: @topic

  # ——— GenServer ———

  @impl true
  def init(init_arg) do
    # Defer subscription so we don't crash if PubSub hasn't started yet.
    send(self(), :subscribe_pubsub)
    {:ok, %{init_arg: init_arg, subscribed?: false, last: nil}}
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

  # Generic PubSub handler; adjust patterns as your producers require.
  @impl true
  def handle_info({:blackboard, payload}, state) do
    {:noreply, %{state | last: payload}}
  end

  # Catch-all so unexpected messages don't crash the server.
  @impl true
  def handle_info(_msg, state), do: {:noreply, state}

  @impl true
  def handle_call(:state, _from, state), do: {:reply, state, state}

  # ——— Helpers ———

  defp safe_subscribe do
    try do
      :ok = Bus.subscribe(@topic)
    rescue
      e ->
        {:error, e}
    catch
      :exit, reason ->
        {:error, reason}
    end
  end
end
