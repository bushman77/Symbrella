defmodule Brain.SelfCalibration.Logger do
  @moduledoc """
  JSONL logger for self-calibration samples.

  This is the first persistence layer for the ML/tensor calibration track. It is
  intentionally simple: one sample per line, versioned by the sample itself.
  """

  alias Brain.SelfCalibration.Sample

  @event [:brain, :self_calibration, :sample_logged]

  @spec log(Sample.t()) :: :ok | {:error, term()}
  def log(%Sample{} = sample) do
    cfg = Application.get_env(:brain, __MODULE__, [])
    log(sample, cfg)
  end

  def log(_sample), do: {:error, :invalid_sample}

  @spec log(Sample.t(), keyword()) :: :ok | {:error, term()}
  def log(%Sample{} = sample, cfg) when is_list(cfg) do
    if Keyword.get(cfg, :enabled?, false) do
      path = Keyword.get(cfg, :path, "priv/self_calibration/samples.jsonl")
      do_log(sample, path, cfg)
    else
      :ok
    end
  end

  def log(%Sample{}, _cfg), do: {:error, :invalid_config}
  def log(_sample, _cfg), do: {:error, :invalid_sample}

  # A per-file lock prevents concurrent append and rotation from interleaving.
  # The archived files are ignored by Git, like the active samples.jsonl file.
  defp do_log(%Sample{} = sample, path, cfg) when is_binary(path) do
    max_bytes = positive_limit(Keyword.get(cfg, :max_bytes, 25_000_000), 25_000_000)
    max_files = positive_limit(Keyword.get(cfg, :max_files, 3), 3) |> min(10)
    max_record_bytes = positive_limit(Keyword.get(cfg, :max_record_bytes, 4_000_000), 4_000_000)

    with {:ok, encoded} <- encode_sample(sample),
         :ok <- check_record_size(encoded, max_record_bytes),
         :ok <-
           :global.trans({__MODULE__, Path.expand(path)}, fn ->
             with :ok <- ensure_parent_dir(path),
                  :ok <- rotate_if_needed(path, byte_size(encoded) + 1, max_bytes, max_files) do
               File.write(path, encoded <> "\n", [:append])
             end
           end) do
      emit_logged(sample)
      :ok
    end
  end

  defp do_log(_sample, _path, _cfg), do: {:error, :invalid_path}

  defp positive_limit(value, _default) when is_integer(value) and value > 0, do: value
  defp positive_limit(_value, default), do: default

  defp check_record_size(encoded, limit) do
    if byte_size(encoded) + 1 <= limit, do: :ok, else: {:error, :sample_too_large}
  end

  defp rotate_if_needed(path, incoming_bytes, max_bytes, max_files) do
    case File.stat(path) do
      {:ok, %{size: size}} when size > 0 and size + incoming_bytes > max_bytes ->
        rotate_archives(path, max_files)

      {:ok, _} ->
        :ok

      {:error, :enoent} ->
        :ok

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp rotate_archives(path, max_files) do
    with :ok <- remove_if_present("#{path}.#{max_files}") do
      Enum.reduce_while(max_files..1//-1, :ok, fn index, :ok ->
        source = if index == 1, do: path, else: "#{path}.#{index - 1}"
        destination = "#{path}.#{index}"

        case File.rename(source, destination) do
          :ok -> {:cont, :ok}
          {:error, :enoent} -> {:cont, :ok}
          {:error, reason} -> {:halt, {:error, reason}}
        end
      end)
    end
  end

  defp remove_if_present(path) do
    case File.rm(path) do
      :ok -> :ok
      {:error, :enoent} -> :ok
      {:error, reason} -> {:error, reason}
    end
  end

  defp ensure_parent_dir(path) do
    path
    |> Path.dirname()
    |> File.mkdir_p()
  end

  defp encode_sample(%Sample{} = sample) do
    sample
    |> Map.from_struct()
    |> json_safe()
    |> Jason.encode()
  end

  defp json_safe(value) when is_binary(value) or is_number(value) or is_boolean(value) or is_nil(value),
    do: value

  defp json_safe(value) when is_atom(value), do: Atom.to_string(value)

  defp json_safe(value) when is_list(value), do: Enum.map(value, &json_safe/1)

  defp json_safe(value) when is_map(value) do
    value
    |> maybe_from_struct()
    |> Enum.into(%{}, fn {key, nested} -> {json_key(key), json_safe(nested)} end)
  end

  defp json_safe(value), do: inspect(value)

  defp maybe_from_struct(%{__struct__: _} = value), do: Map.from_struct(value)
  defp maybe_from_struct(value), do: value

  defp json_key(key) when is_binary(key), do: key
  defp json_key(key) when is_atom(key), do: Atom.to_string(key)
  defp json_key(key), do: inspect(key)

  defp emit_logged(%Sample{} = sample) do
    :telemetry.execute(
      @event,
      %{count: 1},
      %{
        source: sample.source,
        v: sample.v,
        feature_schema_v: get_in(sample.meta, [:feature_schema_v])
      }
    )
  end
end
