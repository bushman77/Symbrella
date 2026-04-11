# apps/brain/lib/brain/self_portrait.ex
defmodule Brain.SelfPortrait do
  @moduledoc """
  SelfPortrait — region wrapper (GenServer) for a compact self-model snapshot.

  Design:
    * Subscribes to the Blackboard PubSub stream (topic: `Brain.Blackboard.topic/0`)
    * Updates an in-memory portrait via `Brain.SelfPortrait.Model.observe/2`
    * Provides a stable `snapshot/0` API for other regions/modules
  """

  use Brain, region: :self_portrait
  require Logger

  alias Brain.Bus
  alias Brain.SelfPortrait.Model

  @name __MODULE__
  @retry_ms 50

  @spec snapshot(pid() | module()) :: map()
  def snapshot(server \\ @name), do: GenServer.call(server, :snapshot)

  @spec reset(pid() | module()) :: :ok
  def reset(server \\ @name), do: GenServer.call(server, :reset)

  @spec observe(map()) :: :ok
  def observe(%{} = ev), do: GenServer.cast(@name, {:observe, ev})

  @impl true
  def init(opts) do
    base = Brain.Region.base_state(:self_portrait, opts)
    portrait = Model.new(opts)
    topic = blackboard_topic()

    send(self(), :subscribe_pubsub)

    {:ok,
     Map.merge(base, %{
       portrait: portrait,
       topic: topic,
       subscribed?: false,
       opts: opts
     })}
  end

  @impl true
  def handle_info(:subscribe_pubsub, %{subscribed?: false, topic: topic} = state) do
    case safe_subscribe(topic) do
      :ok ->
        Logger.info(fn -> "[SelfPortrait] subscribed to #{topic} on #{inspect(Bus.name())}" end)
        {:noreply, %{state | subscribed?: true}}

      {:error, reason} ->
        Logger.warning(fn ->
          "[SelfPortrait] PubSub not ready (#{inspect(reason)}); retrying in #{@retry_ms}ms"
        end)

        Process.send_after(self(), :subscribe_pubsub, @retry_ms)
        {:noreply, state}
    end
  end

  @impl true
  def handle_info(:subscribe_pubsub, state), do: {:noreply, state}

  @impl true
  def handle_info({:blackboard, %{} = payload}, state) do
    portrait0 = state.portrait
    portrait = Model.observe(portrait0, payload)
    maybe_emit_monitor(portrait0, portrait)

    safe_telemetry(
      [:brain, :self_portrait, :update],
      %{events: 1},
      %{at_ms: System.system_time(:millisecond)}
    )

    {:noreply, %{state | portrait: portrait}}
  end

  @impl true
  def handle_info(_msg, state), do: {:noreply, state}

  @impl true
  def handle_cast({:observe, %{} = ev}, state) do
    portrait0 = state.portrait
    portrait = Model.observe(portrait0, ev)
    maybe_emit_monitor(portrait0, portrait)

    {:noreply, %{state | portrait: portrait}}
  end

  @impl true
  def handle_cast(_msg, state), do: {:noreply, state}

  @impl true
  def handle_call(:snapshot, _from, state), do: {:reply, state.portrait, state}

  @impl true
  def handle_call(:reset, _from, state) do
    opts = Map.get(state, :opts, []) || []
    {:reply, :ok, %{state | portrait: Model.new(opts)}}
  end

  defp blackboard_topic do
    if Code.ensure_loaded?(Brain.Blackboard) and function_exported?(Brain.Blackboard, :topic, 0) do
      Brain.Blackboard.topic()
    else
      "brain:blackboard"
    end
  end

  defp safe_subscribe(topic) do
    Bus.subscribe(topic)
  end

  defp maybe_emit_monitor(before, after_) do
    before_gaps = get_in(before, [:patterns, :lifg_payload_gaps]) || 0
    after_gaps = get_in(after_, [:patterns, :lifg_payload_gaps]) || 0

    if after_gaps > before_gaps do
      safe_telemetry(
        [:brain, :self_portrait, :monitor],
        %{count: after_gaps - before_gaps},
        %{issue: :lifg_payload_gap, severity: :warning}
      )
    end

    before_pos = get_in(before, [:patterns, :lifg_pos_anomalies]) || 0
    after_pos = get_in(after_, [:patterns, :lifg_pos_anomalies]) || 0

    if after_pos > before_pos do
      safe_telemetry(
        [:brain, :self_portrait, :monitor],
        %{count: after_pos - before_pos},
        %{issue: :lifg_pos_anomaly, severity: :warning}
      )
    end

    :ok
  end

  defp safe_telemetry(event, measurements, meta) do
    if Code.ensure_loaded?(:telemetry) and function_exported?(:telemetry, :execute, 3) do
      :telemetry.execute(event, measurements, meta)
    else
      :ok
    end
  end
end
