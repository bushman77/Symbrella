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

    if Keyword.get(cfg, :enabled?, false) do
      path = Keyword.get(cfg, :path, "priv/self_calibration/samples.jsonl")
      do_log(sample, path)
    else
      :ok
    end
  end

  def log(_sample), do: {:error, :invalid_sample}

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
    |> Jason.encode()
  end

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
