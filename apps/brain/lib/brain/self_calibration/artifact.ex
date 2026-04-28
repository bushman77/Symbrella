defmodule Brain.SelfCalibration.Artifact do
  @moduledoc """
  Persistence boundary for self-calibration training artifacts.

  Artifacts are trusted local files. They contain Axon model params, so the first
  persistence format uses Erlang external term format instead of JSON.
  """

  alias Brain.SelfCalibration.Training

  @type load_result :: {:ok, Training.t()} | {:error, term()}

  @spec save(Training.t(), Path.t()) :: :ok | {:error, term()}
  def save(%Training{} = artifact, path) when is_binary(path) do
    with :ok <- ensure_parent_dir(path),
         binary <- :erlang.term_to_binary(artifact),
         :ok <- File.write(path, binary) do
      :ok
    end
  end

  def save(_artifact, _path), do: {:error, :invalid_artifact}

  @spec load(Path.t()) :: load_result()
  def load(path) when is_binary(path) do
    with {:ok, binary} <- File.read(path),
         {:ok, artifact} <- decode(binary),
         :ok <- validate(artifact) do
      {:ok, artifact}
    end
  end

  def load(_path), do: {:error, :invalid_path}

  defp ensure_parent_dir(path) do
    path
    |> Path.dirname()
    |> File.mkdir_p()
  end

  defp decode(binary) when is_binary(binary) do
    try do
      {:ok, :erlang.binary_to_term(binary, [:safe])}
    rescue
      ArgumentError -> {:error, :invalid_artifact_binary}
    end
  end

  defp validate(%Training{} = artifact) do
    cond do
      artifact.model == nil -> {:error, :missing_model}
      artifact.params == nil -> {:error, :missing_params}
      artifact.feature_names == [] -> {:error, :missing_feature_names}
      artifact.label_names == [] -> {:error, :missing_label_names}
      true -> :ok
    end
  end

  defp validate(_artifact), do: {:error, :invalid_artifact}
end
