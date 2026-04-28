defmodule Brain.SelfCalibration.Dataset do
  @moduledoc """
  Loads self-calibration JSONL samples and converts them into stable numeric rows.

  This is the last symbolic step before Nx tensors. Keep the feature and label
  order stable; model artifacts depend on it.
  """

  alias Brain.SelfCalibration.Sample

  @feature_names [
    :appraisal_valence,
    :appraisal_arousal,
    :appraisal_dominance,
    :attribution_confidence,
    :lifg_choices_count,
    :cognitive_load,
    :recent_error_count,
    :mood_vigilance,
    :mood_plasticity,
    :mood_inhibition
  ]

  @label_names [
    :confidence,
    :uncertainty,
    :stability
  ]

  @type rows :: %{
          feature_names: [atom()],
          label_names: [atom()],
          x: [[float()]],
          y: [[float()]]
        }

  @doc """
  Loads newline-delimited JSON samples from disk.

  Invalid or blank lines are skipped so one bad sample does not make the whole
  dataset unusable.
  """
  @spec load_jsonl(Path.t()) :: {:ok, [Sample.t()]} | {:error, term()}
  def load_jsonl(path) when is_binary(path) do
    with {:ok, body} <- File.read(path) do
      samples =
        body
        |> String.split("\n", trim: true)
        |> Enum.flat_map(&decode_line/1)

      {:ok, samples}
    end
  end

  def load_jsonl(_path), do: {:error, :invalid_path}

  @doc """
  Converts samples into stable feature and label rows.
  """
  @spec to_rows([Sample.t() | map()]) :: rows()
  def to_rows(samples) when is_list(samples) do
    %{
      feature_names: @feature_names,
      label_names: @label_names,
      x: Enum.map(samples, &feature_row/1),
      y: Enum.map(samples, &label_row/1)
    }
  end

  def feature_names, do: @feature_names
  def label_names, do: @label_names

  defp decode_line(line) when is_binary(line) do
    case Jason.decode(line) do
      {:ok, %{} = map} -> [sample_from_map(map)]
      _ -> []
    end
  end

  defp sample_from_map(%{} = map) do
    %Sample{
      features: atomize_known_keys(Map.get(map, "features") || Map.get(map, :features) || %{}),
      labels: atomize_known_keys(Map.get(map, "labels") || Map.get(map, :labels) || %{}),
      raw: normalize_raw(Map.get(map, "raw") || Map.get(map, :raw) || %{}),
      source: atomize_source(Map.get(map, "source") || Map.get(map, :source)),
      meta: atomize_known_keys(Map.get(map, "meta") || Map.get(map, :meta) || %{}),
      v: int_value(Map.get(map, "v") || Map.get(map, :v) || 1)
    }
  end

  defp atomize_known_keys(%{} = map) do
    map
    |> Enum.map(fn {key, value} -> {known_key(key), value} end)
    |> Map.new()
  end

  defp atomize_known_keys(_), do: %{}

  defp known_key(key) when is_atom(key), do: key

  defp known_key(key) when is_binary(key) do
    case key do
      "appraisal_valence" -> :appraisal_valence
      "appraisal_arousal" -> :appraisal_arousal
      "appraisal_dominance" -> :appraisal_dominance
      "attribution_confidence" -> :attribution_confidence
      "lifg_choices_count" -> :lifg_choices_count
      "cognitive_load" -> :cognitive_load
      "recent_error_count" -> :recent_error_count
      "mood_vigilance" -> :mood_vigilance
      "mood_plasticity" -> :mood_plasticity
      "mood_inhibition" -> :mood_inhibition
      "confidence" -> :confidence
      "uncertainty" -> :uncertainty
      "stability" -> :stability
      "feature_schema_v" -> :feature_schema_v
      "self_model_v" -> :self_model_v
      "label_source" -> :label_source
      other -> other
    end
  end

  defp feature_row(%Sample{features: features}), do: row(features, @feature_names)
  defp feature_row(%{} = sample), do: row(Map.get(sample, :features, %{}), @feature_names)
  defp feature_row(_), do: row(%{}, @feature_names)

  defp label_row(%Sample{labels: labels}), do: row(labels, @label_names)
  defp label_row(%{} = sample), do: row(Map.get(sample, :labels, %{}), @label_names)
  defp label_row(_), do: row(%{}, @label_names)

  defp row(%{} = values, names) do
    Enum.map(names, fn name ->
      values
      |> Map.get(name, Map.get(values, to_string(name), 0.0))
      |> number()
    end)
  end

  defp row(_values, names), do: Enum.map(names, fn _ -> 0.0 end)

  defp number(value) when is_integer(value), do: value * 1.0
  defp number(value) when is_float(value), do: value

  defp number(value) when is_binary(value) do
    case Float.parse(value) do
      {parsed, ""} -> parsed
      _ -> 0.0
    end
  end

  defp number(_), do: 0.0
  defp normalize_raw(%{} = raw), do: raw
  defp normalize_raw(_), do: %{}

  defp int_value(value) when is_integer(value), do: value

  defp int_value(value) when is_binary(value) do
    case Integer.parse(value) do
      {parsed, ""} -> parsed
      _ -> 1
    end
  end

  defp int_value(_), do: 1

  defp atomize_source(value) when value in [:runtime, :test, :reviewed, :synthetic], do: value
  defp atomize_source("runtime"), do: :runtime
  defp atomize_source("test"), do: :test
  defp atomize_source("reviewed"), do: :reviewed
  defp atomize_source("synthetic"), do: :synthetic
  defp atomize_source(_), do: :runtime
end
