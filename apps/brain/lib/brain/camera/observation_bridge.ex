defmodule Brain.Camera.ObservationBridge do
  @moduledoc """
  Bridges raw camera frames into symbolic visual observations.

  This is intentionally not an image decoder and not a brain region. It listens
  for `Brain.Camera` frame telemetry, wraps each frame in a conservative
  `:visual_observation` payload, publishes it to the Blackboard, and nudges the
  existing curiosity/OFC/Thalamus path with a low-risk proposal.

  The observation is explicit about its limits: `decoded?: false`,
  `description: nil`, and `labels: []`. A later decoder can enrich the same
  payload shape without moving camera capture or valuation concerns into
  `Brain.Camera`.
  """

  use GenServer

  @camera_frame_event [:brain, :camera, :frame]
  @visual_observation_event [:brain, :visual_observation]

  @default_score 0.35
  @default_novelty 0.25

  defstruct enabled?: true,
            telemetry_id: nil,
            count: 0,
            last_observation: nil,
            last_size_bytes: nil,
            last_observed_at_ms: nil

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

  @doc "Return the latest bridged visual observation."
  def latest(server \\ __MODULE__), do: GenServer.call(server, :latest)

  @doc "Return compact bridge status."
  def status(server \\ __MODULE__), do: GenServer.call(server, :status)

  @doc false
  def handle_camera_frame(_event, measurements, metadata, %{pid: pid}) when is_pid(pid) do
    send(pid, {:camera_frame, measurements, metadata})
    :ok
  end

  def handle_camera_frame(_, _, _, _), do: :ok

  @impl true
  def init(opts) do
    enabled? = truthy?(Keyword.get(opts, :enabled?, true))

    state = %__MODULE__{
      enabled?: enabled?,
      telemetry_id: "brain-camera-observation-bridge-#{System.unique_integer([:positive])}"
    }

    if enabled? do
      :ok =
        :telemetry.attach(
          state.telemetry_id,
          @camera_frame_event,
          &__MODULE__.handle_camera_frame/4,
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
  def handle_call(:latest, _from, state) do
    {:reply, state.last_observation, state}
  end

  def handle_call(:status, _from, state) do
    reply = %{
      enabled?: state.enabled?,
      count: state.count,
      last_observed_at_ms: state.last_observed_at_ms,
      last_observation: state.last_observation
    }

    {:reply, reply, state}
  end

  @impl true
  def handle_info({:camera_frame, measurements, frame}, state) when is_map(frame) do
    {observation, state} = observe_frame(frame, measurements, state)

    Brain.Blackboard.publish(observation)
    emit_visual_observation(observation, state.count)
    emit_curiosity_proposal(observation)

    {:noreply, state}
  end

  def handle_info(_msg, state), do: {:noreply, state}

  defp observe_frame(frame, measurements, state) do
    observed_at_ms = map_get(frame, :captured_at_ms, System.system_time(:millisecond))
    size_bytes = map_get(frame, :size_bytes, map_get(measurements, :size_bytes, nil))
    novelty = size_delta_novelty(size_bytes, state.last_size_bytes)
    score = observation_score(novelty)

    observation = %{
      kind: :visual_observation,
      source: :camera,
      decoder: :none,
      decoded?: false,
      description: nil,
      labels: [],
      confidence: nil,
      frame_path: map_get(frame, :path, nil),
      frame: frame,
      observed_at_ms: observed_at_ms,
      size_bytes: size_bytes,
      duration_ms: map_get(frame, :duration_ms, map_get(measurements, :duration_ms, nil)),
      novelty: novelty,
      risk: 0.0,
      score: score,
      change_hint: change_hint(size_bytes, state.last_size_bytes),
      v: 1
    }

    state = %{
      state
      | count: state.count + 1,
        last_observation: observation,
        last_size_bytes: size_bytes,
        last_observed_at_ms: observed_at_ms
    }

    {observation, state}
  end

  defp emit_visual_observation(observation, count) do
    :telemetry.execute(
      @visual_observation_event,
      %{
        count: count,
        score: observation.score,
        novelty: observation.novelty,
        risk: observation.risk
      },
      observation
    )
  end

  defp emit_curiosity_proposal(observation) do
    probe_id = "camera|visual_observation|#{observation.observed_at_ms}"

    probe = %{
      id: probe_id,
      source: :camera,
      reason: :visual_observation,
      seed: %{
        kind: observation.kind,
        frame_path: observation.frame_path,
        observed_at_ms: observation.observed_at_ms,
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

  defp size_delta_novelty(size, nil) when is_integer(size) and size > 0, do: @default_novelty

  defp size_delta_novelty(size, last_size)
       when is_integer(size) and size > 0 and is_integer(last_size) and last_size > 0 do
    delta = abs(size - last_size)
    basis = max(size, last_size)

    (delta / basis * 3.0)
    |> max(0.05)
    |> min(1.0)
  end

  defp size_delta_novelty(_, _), do: @default_novelty

  defp observation_score(novelty) when is_number(novelty) do
    (@default_score + novelty * 0.25)
    |> max(0.0)
    |> min(1.0)
  end

  defp observation_score(_), do: @default_score

  defp change_hint(size, last_size)
       when is_integer(size) and is_integer(last_size) and last_size > 0 do
    %{
      basis: :jpeg_size_delta,
      current_size_bytes: size,
      previous_size_bytes: last_size,
      delta_bytes: size - last_size
    }
  end

  defp change_hint(size, _last_size) do
    %{
      basis: :first_frame,
      current_size_bytes: size,
      previous_size_bytes: nil,
      delta_bytes: nil
    }
  end

  defp map_get(map, key, default) when is_map(map) do
    Map.get(map, key, Map.get(map, to_string(key), default))
  end

  defp normalize_opts(opts) when is_list(opts), do: opts
  defp normalize_opts(opts) when is_map(opts), do: Map.to_list(opts)
  defp normalize_opts(_), do: []

  defp truthy?(value) when value in [true, "true", "1", 1, true, :on, "on", :yes, "yes"],
    do: true

  defp truthy?(_), do: false
end
