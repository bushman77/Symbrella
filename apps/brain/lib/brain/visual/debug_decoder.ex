defmodule Brain.Visual.DebugDecoder do
  @moduledoc """
  Debug visual decoder for proving the decoded-observation pathway.

  This module does not inspect pixels. It listens for undecoded camera
  `:visual_observation` telemetry, enriches the observation with a deliberately
  low-confidence debug description, republishes it to the Blackboard, and emits
  the same telemetry/proposal shapes that a future real decoder should emit.

  Keep this module separate from `Brain.Camera`: camera capture remains a sensor
  adapter, while this module owns the temporary pixel-to-symbol contract.
  """

  use GenServer

  @visual_observation_event [:brain, :visual_observation]

  defstruct enabled?: true,
            telemetry_id: nil,
            count: 0,
            last_decoded: nil,
            last_decoded_at_ms: nil

  @doc "Child-spec friendly start_link."
  def start_link(opts \\ []) do
    opts = normalize_opts(opts)
    name = Keyword.get(opts, :name, __MODULE__)

    if name do
      GenServer.start_link(__MODULE__, opts, name: name)
    else
      GenServer.start_link(__MODULE__, opts)
    end
  end

  @doc "Return the latest debug-decoded visual observation."
  def latest(server \\ __MODULE__), do: GenServer.call(server, :latest)

  @doc "Return compact decoder status."
  def status(server \\ __MODULE__), do: GenServer.call(server, :status)

  @doc false
  def handle_visual_observation(_event, measurements, metadata, %{pid: pid}) when is_pid(pid) do
    send(pid, {:visual_observation, measurements, metadata})
    :ok
  end

  def handle_visual_observation(_, _, _, _), do: :ok

  @impl true
  def init(opts) do
    enabled? = truthy?(Keyword.get(opts, :enabled?, true))

    state = %__MODULE__{
      enabled?: enabled?,
      telemetry_id: "brain-visual-debug-decoder-#{System.unique_integer([:positive])}"
    }

    if enabled? do
      :ok =
        :telemetry.attach(
          state.telemetry_id,
          @visual_observation_event,
          &__MODULE__.handle_visual_observation/4,
          %{pid: self()}
        )
    end

    {:ok, state}
  end

  @impl true
  def terminate(_reason, %{telemetry_id: id}) when is_binary(id) do
    :telemetry.detach(id)
    :ok
  end

  def terminate(_, _), do: :ok

  @impl true
  def handle_call(:latest, _from, state), do: {:reply, state.last_decoded, state}

  def handle_call(:status, _from, state) do
    reply = %{
      enabled?: state.enabled?,
      count: state.count,
      last_decoded_at_ms: state.last_decoded_at_ms,
      last_decoded: state.last_decoded
    }

    {:reply, reply, state}
  end

  @impl true
  def handle_info({:visual_observation, _measurements, observation}, state)
      when is_map(observation) do
    if decode_candidate?(observation) do
      {decoded, state} = decode_observation(observation, state)

      Brain.Blackboard.publish(decoded)
      emit_visual_observation(decoded, state.count)
      emit_curiosity_proposal(decoded)

      {:noreply, state}
    else
      {:noreply, state}
    end
  end

  def handle_info(_msg, state), do: {:noreply, state}

  defp decode_candidate?(observation) do
    map_get(observation, :kind, nil) == :visual_observation and
      map_get(observation, :source, nil) == :camera and
      map_get(observation, :decoded?, false) != true
  end

  defp decode_observation(observation, state) do
    decoded_at_ms = System.system_time(:millisecond)

    decoded =
      observation
      |> Map.put(:decoded?, true)
      |> Map.put(:decoder, :debug_stub)
      |> Map.put(:description, "Camera frame available.")
      |> Map.put(:labels, ["camera_frame"])
      |> Map.put(:confidence, 0.1)
      |> Map.put(:decoded_at_ms, decoded_at_ms)
      |> Map.put(:score, max(map_get(observation, :score, 0.35), 0.4))
      |> Map.put(:novelty, map_get(observation, :novelty, 0.25))
      |> Map.put(:risk, map_get(observation, :risk, 0.0))
      |> Map.put(:v, 1)

    state = %{
      state
      | count: state.count + 1,
        last_decoded: decoded,
        last_decoded_at_ms: decoded_at_ms
    }

    {decoded, state}
  end

  defp emit_visual_observation(observation, count) do
    :telemetry.execute(
      @visual_observation_event,
      %{
        count: count,
        score: observation.score,
        novelty: observation.novelty,
        risk: observation.risk,
        decoded: 1
      },
      observation
    )
  end

  defp emit_curiosity_proposal(observation) do
    probe = %{
      id: "camera|debug_decoder|#{observation.observed_at_ms}",
      source: :camera,
      reason: :decoded_visual_observation,
      seed: %{
        kind: observation.kind,
        frame_path: observation.frame_path,
        decoder: observation.decoder,
        labels: observation.labels,
        decoded?: observation.decoded?
      },
      score: observation.score
    }

    :telemetry.execute(
      [:curiosity, :proposal],
      %{score: observation.score, novelty: observation.novelty, risk: observation.risk},
      %{probe: probe, source: :camera, visual_observation: observation}
    )
  end

  defp map_get(map, key, default) when is_map(map) do
    Map.get(map, key, Map.get(map, to_string(key), default))
  end

  defp normalize_opts(opts) when is_list(opts), do: opts
  defp normalize_opts(opts) when is_map(opts), do: Map.to_list(opts)
  defp normalize_opts(_), do: []

  defp truthy?(value) when value in [true, "true", "1", 1, :on, "on", :yes, "yes"],
    do: true

  defp truthy?(_), do: false
end
