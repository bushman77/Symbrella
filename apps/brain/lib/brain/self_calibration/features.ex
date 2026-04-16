defmodule Brain.SelfCalibration.Features do
  @moduledoc """
  Extracts stable calibration features and labels from a self-model plus runtime evidence.
  """

  alias Brain.SelfCalibration.Sample

  @spec build_sample(Brain.SelfModel.t(), keyword()) :: Sample.t()
  def build_sample(%Brain.SelfModel{} = self_model, opts \\ []) do
    appraisal = Keyword.get(opts, :appraisal) || self_model.last_appraisal || %{}
    attribution = get_in(appraisal, [:evidence, :attribution]) || %{}
    lifg = Keyword.get(opts, :lifg) || self_model.last_lifg || %{}
    mood = self_model.mood || %{}

    %Sample{
      features: %{
        appraisal_valence: number(Map.get(appraisal, :valence)),
        appraisal_arousal: number(Map.get(appraisal, :arousal)),
        appraisal_dominance: number(Map.get(appraisal, :dominance)),
        attribution_confidence: number(Map.get(attribution, :confidence)),
        lifg_choices_count: count(Map.get(lifg, :choices_count)),
        cognitive_load: number(self_model.cognitive_load),
        recent_error_count: length(List.wrap(self_model.recent_errors)),
        mood_vigilance: mood_value(mood, :vigilance),
        mood_plasticity: mood_value(mood, :plasticity),
        mood_inhibition: mood_value(mood, :inhibition)
      },
      labels: %{
        confidence: number(self_model.confidence),
        uncertainty: number(self_model.uncertainty),
        stability: number(self_model.stability)
      },
      source: Keyword.get(opts, :source, :runtime),
      meta: %{
        self_model_v: self_model.v,
        feature_schema_v: 1
      },
      v: 1
    }
  end

  defp mood_value(%{} = mood, key) do
    derived = Map.get(mood, :derived) || Map.get(mood, "derived") || %{}
    nested = Map.get(mood, :mood) || Map.get(mood, "mood") || %{}

    number(
      Map.get(derived, key) ||
        Map.get(derived, to_string(key)) ||
        Map.get(nested, key) ||
        Map.get(nested, to_string(key)) ||
        Map.get(mood, key) ||
        Map.get(mood, to_string(key))
    )
  end

  defp mood_value(_, _), do: 0.0

  defp count(value) when is_integer(value), do: value
  defp count(value) when is_list(value), do: length(value)
  defp count(_), do: 0

  defp number(value) when is_integer(value), do: value * 1.0
  defp number(value) when is_float(value), do: value
  defp number(_), do: 0.0
end
