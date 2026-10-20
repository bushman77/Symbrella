defmodule Brain.Visual.LumaSampler do
  @moduledoc """
  Samples the pixels of a captured JPEG with FFmpeg.

  Returns exactly 64 grayscale bytes (8x8) and performs no semantic decoding.
  Reads the already-approved frame path; creates no files or directories.
  The external process is bounded by GNU timeout, as in Brain.Camera.
  """

  @pixel_count 64

  @spec sample(String.t()) :: {:ok, binary()} | {:error, term()}
  def sample(path) when is_binary(path) do
    cond do
      not File.regular?(path) ->
        {:error, :frame_unavailable}

      not is_binary(System.find_executable("ffmpeg")) ->
        {:error, :ffmpeg_unavailable}

      not is_binary(System.find_executable("timeout")) ->
        {:error, :timeout_unavailable}

      true ->
        ffmpeg = System.find_executable("ffmpeg")
        timeout = System.find_executable("timeout")

        args = [
          "4s",
          ffmpeg,
          "-hide_banner", "-loglevel", "error", "-nostdin",
          "-i", path,
          "-frames:v", "1",
          "-vf", "scale=8:8:flags=area,format=gray",
          "-f", "rawvideo", "pipe:1"
        ]

        case System.cmd(timeout, args, stderr_to_stdout: true) do
          {pixels, 0} when byte_size(pixels) == @pixel_count -> {:ok, pixels}
          {_output, 0} -> {:error, :invalid_grayscale_frame}
          {_output, 124} -> {:error, :sampler_timeout}
          {_output, status} -> {:error, {:ffmpeg_exit, status}}
        end
    end
  rescue
    _ -> {:error, :sampler_failed}
  end

  def sample(_), do: {:error, :invalid_frame_path}
end
