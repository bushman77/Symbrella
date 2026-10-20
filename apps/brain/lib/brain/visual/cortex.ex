defmodule Brain.Visual.Cortex do
  @moduledoc """
  First-stage visual cortex: pixel-derived, nonsemantic sensory features.

  Listens to raw camera observations and asks a sampler for an 8x8 grayscale
  thumbnail of the actual JPEG. The values support basic brightness and
  frame-to-frame change measurements. They do not establish objects, people,
  scene descriptions, or spatial relationships.

  Emits [:brain, :visual, :features] and publishes a :visual_features event
  to the Blackboard. It deliberately emits no curiosity proposal and no
  :visual_observation with decoded?: true. A future vision adapter may
  produce semantic observations with independent evidence and provenance.
  """

  use GenServer

  @observation_event [:brain, :visual_observation]
  @features_event [:brain, :visual, :features]
  @error_event [:brain, :visual, :error]
  @grid_width 8
  @pixel_count 64

  defstruct enabled?: true,
            telemetry_id: nil,
            sampler: Brain.Visual.LumaSampler,
            count: 0,
            failure_count: 0,
            last_features: nil,
            last_error: nil,
            last_grid: nil

  @doc "Starts a supervised visual cortex; :name can be nil for isolated tests."
  def start_link(opts \\ []) do
    opts = normalize_opts(opts)
    name = Keyword.get(opts, :name, __MODULE__)

    if name do
      GenServer.start_link(__MODULE__, opts, name: name)
    else
      GenServer.start_link(__MODULE__, opts)
    end
  end

  @doc "Latest real pixel-derived features, or nil before the first capture."
  def latest(server \\ __MODULE__), do: GenServer.call(server, :latest)

  @doc "Processing counters and most recent feature/error."
  def status(server \\ __MODULE__), do: GenServer.call(server, :status)

  @doc false
  def handle_observation(_event, measurements, metadata, %{pid: pid}) when is_pid(pid) do
    send(pid, {:visual_observation, measurements, metadata})
    :ok
  end

  def handle_observation(_, _, _, _), do: :ok

  @impl true
  def init(opts) do
    state = %__MODULE__{
      enabled?: Keyword.get(opts, :enabled?, true) == true,
      sampler: Keyword.get(opts, :sampler, Brain.Visual.LumaSampler),
      telemetry_id: "brain-visual-cortex-#{System.unique_integer([:positive])}"
    }

    if state.enabled? do
      :ok = :telemetry.attach(state.telemetry_id, @observation_event,
        &__MODULE__.handle_observation/4, %{pid: self()})
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
  def handle_call(:latest, _from, state), do: {:reply, state.last_features, state}

  def handle_call(:status, _from, state) do
    {:reply,
     %{
       enabled?: state.enabled?,
       count: state.count,
       failure_count: state.failure_count,
       last_features: state.last_features,
       last_error: state.last_error
     }, state}
  end

  @impl true
  def handle_info({:visual_observation, _measurements, observation}, state)
      when is_map(observation) do
    if raw_camera_observation?(observation) do
      path = map_get(observation, :frame_path)

      case safely_sample(state.sampler, path) do
        {:ok, pixels} when is_binary(pixels) and byte_size(pixels) == @pixel_count ->
          {features, state} = extract_features(observation, pixels, state)
          Brain.Blackboard.publish(features)

          :telemetry.execute(@features_event,
            %{
              count: state.count,
              mean_luminance: features.mean_luminance,
              change_fraction: features.change_fraction || 0.0
            }, features)

          {:noreply, state}

        {:ok, _} ->
          {:noreply, record_failure(state, :invalid_sample_size, path)}

        {:error, reason} ->
          {:noreply, record_failure(state, reason, path)}

        _ ->
          {:noreply, record_failure(state, :invalid_sampler_response, path)}
      end
    else
      {:noreply, state}
    end
  end

  def handle_info(_msg, state), do: {:noreply, state}

  defp raw_camera_observation?(observation) do
    map_get(observation, :kind) == :visual_observation and
      map_get(observation, :source) == :camera and
      map_get(observation, :decoder) in [nil, :none] and
      map_get(observation, :decoded?, false) == false and
      is_binary(map_get(observation, :frame_path))
  end

  defp extract_features(observation, pixels, state) do
    grid = :binary.bin_to_list(pixels)
    mean = Float.round(Enum.sum(grid) / (@pixel_count * 255), 4)
    change = frame_change(grid, state.last_grid)

    features = %{
      kind: :visual_features,
      source: :camera,
      method: :grayscale_8x8,
      frame_path: map_get(observation, :frame_path),
      observed_at_ms: map_get(observation, :observed_at_ms),
      analyzed_at_ms: System.system_time(:millisecond),
      mean_luminance: mean,
      quadrants: %{
        top_left: quadrant(grid, 0..3, 0..3),
        top_right: quadrant(grid, 0..3, 4..7),
        bottom_left: quadrant(grid, 4..7, 0..3),
        bottom_right: quadrant(grid, 4..7, 4..7)
      },
      change_fraction: change,
      change_basis: if(is_nil(change), do: :first_frame, else: :pixel_difference),
      semantic_status: :uninterpreted,
      description: nil,
      labels: [],
      decoded?: false,
      v: 1
    }

    {features,
     %{state |
       count: state.count + 1,
       last_features: features,
       last_error: nil,
       last_grid: grid}}
  end

  defp quadrant(grid, rows, cols) do
    values = for row <- rows, col <- cols, do: Enum.at(grid, row * @grid_width + col)
    Float.round(Enum.sum(values) / (length(values) * 255), 4)
  end

  defp frame_change(_grid, nil), do: nil

  defp frame_change(grid, previous) do
    difference =
      Enum.zip(grid, previous)
      |> Enum.reduce(0, fn {current, old}, total -> total + abs(current - old) end)

    Float.round(difference / (@pixel_count * 255), 4)
  end

  defp record_failure(state, reason, path) do
    error = %{
      reason: reason,
      frame_path: path,
      occurred_at_ms: System.system_time(:millisecond)
    }

    :telemetry.execute(@error_event,
      %{count: state.failure_count + 1}, error)

    %{state |
      failure_count: state.failure_count + 1,
      last_error: error}
  end

  defp safely_sample(sampler, path) do
    cond do
      is_function(sampler, 1) -> sampler.(path)
      is_atom(sampler) -> sampler.sample(path)
      true -> {:error, :invalid_sampler}
    end
  rescue
    _ -> {:error, :sampler_failed}
  catch
    :exit, _ -> {:error, :sampler_exit}
  end

  defp map_get(map, key, default \\ nil) when is_map(map),
    do: Map.get(map, key, Map.get(map, to_string(key), default))

  defp normalize_opts(opts) when is_list(opts), do: opts
  defp normalize_opts(opts) when is_map(opts), do: Map.to_list(opts)
  defp normalize_opts(_), do: []
end
