defmodule Brain.Introspection do
  @moduledoc """
  Builds the canonical runtime self-model from current Brain evidence.
  """
  alias Brain.SelfCalibration.Features
  alias Brain.SelfCalibration.Logger, as: CalibrationLogger

  def snapshot do
    Brain.SelfModel.from_runtime(
      self_portrait: Brain.SelfPortrait.snapshot(),
      meta: Brain.Meta.status(),
      mood: Brain.MoodCore.snapshot(),
      wm: Brain.snapshot_wm()
    )
  end

  def update_from_resolved(resolved, appraisal) when is_map(resolved) and is_map(appraisal) do
    model =
      snapshot()
      |> Map.put(:last_appraisal, appraisal)
      |> Map.put(:last_lifg, %{
        choices_count: resolved |> Map.get(:lifg_choices, []) |> List.wrap() |> length(),
        choices: resolved |> Map.get(:lifg_choices, []) |> List.wrap() |> Enum.take(5),
        acc_conflict: Map.get(resolved, :acc_conflict),
        frame_run_id: Map.get(resolved, :frame_run_id)
      })
      |> Map.put(:self_other_attribution, attribution_from(appraisal))

    emit_update(model)
    maybe_log_calibration_sample(model, resolved, appraisal)
    {:ok, model}
  end

  def update_from_resolved(_resolved, _appraisal), do: {:error, :invalid_args}

  defp attribution_from(%{evidence: %{target: target}}), do: %{target: target}
  defp attribution_from(%{target: target}), do: %{target: target}
  defp attribution_from(_), do: %{}

  defp emit_update(%Brain.SelfModel{} = model) do
    :telemetry.execute(
      [:brain, :self_model, :update],
      %{
        count: 1,
        confidence: model.confidence,
        uncertainty: model.uncertainty,
        stability: model.stability,
        cognitive_load: model.cognitive_load
      },
      %{
        v: model.v,
        target: Map.get(model.self_other_attribution, :target),
        active_goals: length(model.active_goals || []),
        lifg_choices_count: get_in(model.last_lifg || %{}, [:choices_count])
      }
    )
  end

  defp maybe_log_calibration_sample(%Brain.SelfModel{} = model, resolved, appraisal) do
    sample =
      Features.build_sample(model,
        appraisal: appraisal,
        lifg: model.last_lifg,
        raw: %{
          resolved: resolved,
          appraisal: appraisal,
          mood: model.mood,
          wm: Brain.snapshot_wm(),
          errors: model.recent_errors
        },
        source: :runtime
      )

    case CalibrationLogger.log(sample) do
      :ok -> :ok
      {:error, _reason} -> :ok
    end
  end
end
