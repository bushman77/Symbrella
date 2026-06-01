defmodule Brain.Camera do
  @moduledoc """
  Minimal camera sensor process for Symbrella.

  This module captures still frames from a configured camera source and keeps
  metadata about the latest captured frame. It does not interpret images and it
  does not mutate working memory. Higher-level vision/perception modules should
  consume the emitted telemetry or call `latest/0`.
  """

  use GenServer
  require Logger

  @defaults [
    enabled?: false,
    ffmpeg: "ffmpeg",
    source: {:v4l2, "/dev/video1"},
    input_format: "yuyv422",
    video_size: "1280x720",
    interval_ms: 1_000,
    output_path: "/tmp/symbrella_camera/latest.jpg",
    timeout_ms: 8_000,
    loglevel: "warning"
  ]

  defstruct enabled: false,
            ffmpeg: "ffmpeg",
            source: {:v4l2, "/dev/video1"},
            input_format: "yuyv422",
            video_size: "1280x720",
            interval_ms: 1_000,
            output_path: "/tmp/symbrella_camera/latest.jpg",
            timeout_ms: 8_000,
            loglevel: "warning",
            latest: nil,
            last_error: nil,
            capture_count: 0,
            failure_count: 0,
            timer_ref: nil

  # ── Public API ────────────────────────────────────────────────────────────

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def latest do
    call_if_running(:latest)
  end

  def status do
    call_if_running(:status)
  end

  def capture_now do
    call_if_running(:capture_now, 15_000)
  end

  # ── GenServer callbacks ───────────────────────────────────────────────────

  @impl true
  def init(opts) do
    state =
      opts
      |> normalize_config()
      |> ensure_output_dir()

    state =
      if state.enabled do
        schedule_capture(state, 0)
      else
        state
      end

    {:ok, state}
  end

  @impl true
  def handle_call(:latest, _from, state) do
    {:reply, state.latest, state}
  end

  def handle_call(:status, _from, state) do
    {:reply, status_map(state), state}
  end

  def handle_call(:capture_now, _from, state) do
    {reply, state} = capture_frame(state)
    {:reply, reply, state}
  end

  @impl true
  def handle_info(:capture, state) do
    {_reply, state} = capture_frame(state)
    {:noreply, schedule_capture(state, state.interval_ms)}
  end

  # ── Capture pipeline ───────────────────────────────────────────────────────

  defp capture_frame(state) do
    started_at = System.monotonic_time(:millisecond)

    case build_ffmpeg_args(state) do
      {:ok, args} ->
        {output, status} = run_capture_command(state.ffmpeg, args, state.timeout_ms)
        duration_ms = System.monotonic_time(:millisecond) - started_at

        handle_capture_result(state, status, output, duration_ms)

      {:error, reason} ->
        duration_ms = System.monotonic_time(:millisecond) - started_at
        handle_capture_error(state, reason, "", duration_ms)
    end
  end

  defp handle_capture_result(state, 0, output, duration_ms) do
    case File.stat(state.output_path) do
      {:ok, %File.Stat{size: size}} when size > 0 ->
        frame = frame_metadata(state, size, duration_ms)

        emit(
          [:brain, :camera, :frame],
          %{
            count: state.capture_count + 1,
            duration_ms: duration_ms,
            size_bytes: size
          },
          frame
        )

        state = %{
          state
          | latest: frame,
            last_error: nil,
            capture_count: state.capture_count + 1
        }

        {{:ok, frame}, state}

      {:ok, %File.Stat{size: size}} ->
        handle_capture_error(state, {:empty_output_file, size}, output, duration_ms)

      {:error, reason} ->
        handle_capture_error(state, {:missing_output_file, reason}, output, duration_ms)
    end
  end

  defp handle_capture_result(state, 124, output, duration_ms) do
    handle_capture_error(state, :timeout, output, duration_ms)
  end

  defp handle_capture_result(state, status, output, duration_ms) do
    handle_capture_error(state, {:ffmpeg_exit, status}, output, duration_ms)
  end

  defp handle_capture_error(state, reason, output, duration_ms) do
    error = %{
      source: source_metadata(state.source),
      path: state.output_path,
      reason: reason,
      output: trim_output(output),
      occurred_at_ms: System.system_time(:millisecond),
      duration_ms: duration_ms
    }

    emit(
      [:brain, :camera, :error],
      %{
        count: state.failure_count + 1,
        duration_ms: duration_ms
      },
      error
    )

    Logger.debug(fn ->
      "Brain.Camera capture failed: #{inspect(reason)} #{trim_output(output)}"
    end)

    state = %{
      state
      | last_error: error,
        failure_count: state.failure_count + 1
    }

    {{:error, error}, state}
  end

  # ── ffmpeg command construction ───────────────────────────────────────────

  defp build_ffmpeg_args(%{source: {:mjpeg_url, url}} = state) when is_binary(url) do
    {:ok,
     [
       "-y",
       "-hide_banner",
       "-loglevel",
       state.loglevel,
       "-i",
       url,
       "-t",
       "3",
       "-vf",
       "fps=1",
       "-update",
       "1",
       "-q:v",
       "2",
       state.output_path
     ]}
  end

  defp build_ffmpeg_args(%{source: source}) do
    {:error, {:unsupported_camera_source, source}}
  end

  defp run_capture_command(ffmpeg, args, timeout_ms) do
    seconds = timeout_seconds(timeout_ms)

    System.cmd(
      "timeout",
      ["#{seconds}s", ffmpeg | args],
      stderr_to_stdout: true
    )
  rescue
    error ->
      {Exception.message(error), 127}
  end

  # ── Metadata ───────────────────────────────────────────────────────────────

  defp frame_metadata(state, size_bytes, duration_ms) do
    {width, height} = parse_video_size(state.video_size)

    %{
      kind: :camera_frame,
      source: source_metadata(state.source),
      path: state.output_path,
      width: width,
      height: height,
      input_format: state.input_format,
      video_size: state.video_size,
      size_bytes: size_bytes,
      captured_at_ms: System.system_time(:millisecond),
      duration_ms: duration_ms
    }
  end

  defp source_metadata({:v4l2, device}) do
    %{
      type: :v4l2,
      device: device
    }
  end

  defp source_metadata({:mjpeg_url, url}) do
    %{
      type: :mjpeg_url,
      url: url
    }
  end

  defp source_metadata(source) do
    %{
      type: :unknown,
      value: inspect(source)
    }
  end

  defp status_map(state) do
    %{
      enabled?: state.enabled,
      source: source_metadata(state.source),
      input_format: state.input_format,
      video_size: state.video_size,
      interval_ms: state.interval_ms,
      output_path: state.output_path,
      timeout_ms: state.timeout_ms,
      capture_count: state.capture_count,
      failure_count: state.failure_count,
      latest: state.latest,
      last_error: state.last_error
    }
  end

  # ── Config / lifecycle helpers ─────────────────────────────────────────────

  defp normalize_config(opts) do
    opts = to_keyword(opts)
    config = Keyword.merge(@defaults, opts)

    %__MODULE__{
      enabled: truthy?(Keyword.get(config, :enabled?, false)),
      ffmpeg: Keyword.get(config, :ffmpeg, "ffmpeg"),
      source: Keyword.get(config, :source, {:v4l2, "/dev/video1"}),
      input_format: Keyword.get(config, :input_format, "yuyv422"),
      video_size: Keyword.get(config, :video_size, "1280x720"),
      interval_ms: positive_integer(Keyword.get(config, :interval_ms), 1_000),
      output_path: Keyword.get(config, :output_path, "/tmp/symbrella_camera/latest.jpg"),
      timeout_ms: positive_integer(Keyword.get(config, :timeout_ms), 8_000),
      loglevel: Keyword.get(config, :loglevel, "warning")
    }
  end

  defp ensure_output_dir(%{output_path: output_path} = state) when is_binary(output_path) do
    output_path
    |> Path.dirname()
    |> File.mkdir_p()

    state
  end

  defp ensure_output_dir(state), do: state

  defp schedule_capture(state, delay_ms) do
    ref = Process.send_after(self(), :capture, non_negative_integer(delay_ms, 1_000))
    %{state | timer_ref: ref}
  end

  defp call_if_running(message, timeout \\ 5_000) do
    case Process.whereis(__MODULE__) do
      nil -> {:error, :not_started}
      _pid -> GenServer.call(__MODULE__, message, timeout)
    end
  end

  defp emit(event, measurements, metadata) do
    if Code.ensure_loaded?(:telemetry) do
      :telemetry.execute(event, measurements, metadata)
    else
      :ok
    end
  end

  defp parse_video_size(size) when is_binary(size) do
    case String.split(size, "x", parts: 2) do
      [width_text, height_text] ->
        with {width, ""} <- Integer.parse(width_text),
             {height, ""} <- Integer.parse(height_text) do
          {width, height}
        else
          _ -> {nil, nil}
        end

      _ ->
        {nil, nil}
    end
  end

  defp parse_video_size(_), do: {nil, nil}

  defp timeout_seconds(ms) when is_integer(ms) and ms > 0 do
    (ms + 999)
    |> div(1_000)
    |> max(1)
  end

  defp timeout_seconds(_), do: 8

  defp positive_integer(value, _fallback) when is_integer(value) and value > 0, do: value
  defp positive_integer(_value, fallback), do: fallback

  defp non_negative_integer(value, _fallback) when is_integer(value) and value >= 0, do: value
  defp non_negative_integer(_value, fallback), do: fallback

  defp truthy?(value) when value in [true, "true", "1", 1, true, :on, "on", :yes, "yes"], do: true
  defp truthy?(_), do: false

  defp to_keyword(opts) when is_list(opts), do: opts
  defp to_keyword(opts) when is_map(opts), do: Map.to_list(opts)
  defp to_keyword(_), do: []

  defp trim_output(output) when is_binary(output) do
    output
    |> String.trim()
    |> String.slice(0, 2_000)
  end

  defp trim_output(_), do: ""
end
