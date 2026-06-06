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
      do_log(sample, path)
    else
      :ok
    end
  end

  def log(%Sample{}, _cfg), do: {:error, :invalid_config}
  def log(_sample, _cfg), do: {:error, :invalid_sample}

  defp do_log(%Sample{} = sample, path) when is_binary(path) do
    with :ok <- ensure_parent_dir(path),
         {:ok, encoded} <- encode_sample(sample),
         :ok <- File.write(path, encoded <> "\n", [:append]) do
      emit_logged(sample)
      :ok
    end
  end

  defp do_log(_sample, _path), do: {:error, :invalid_path}

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
