defmodule Core.Brain.Introspection do
  @moduledoc """
  Core orchestration hook for Brain-owned affect and self-model updates.
  """

  @spec update_self_model(map(), keyword()) :: map()
  def update_self_model(%{} = si, opts) when is_list(opts) do
    case Keyword.get(opts, :self_model, :auto) do
      false -> si
      :off -> si
      _ -> do_update_self_model(si)
    end
  end

  def update_self_model(si, _opts), do: si

  defp do_update_self_model(%{} = si) do
    appraisal = Brain.AffectiveAppraisal.appraise(si)
    _ = Brain.MoodCore.apply_appraisal(appraisal)

    case Brain.Introspection.update_from_resolved(si, appraisal) do
      {:ok, self_model} ->
        mood = get_in(self_model.mood, [:mood])

        si
        |> Map.put(:appraisal, appraisal)
        |> Map.put(:mood, mood || %{})
        |> Map.put(:self_model, self_model)
        |> Core.Pipeline.Trace.append(
          :self_model,
          decision: :updated,
          reason: :resolved_input_appraisal,
          scores: %{confidence: self_model.confidence, uncertainty: self_model.uncertainty},
          meta: %{confidence: self_model.confidence, uncertainty: self_model.uncertainty}
        )

      _ ->
        Map.put(si, :appraisal, appraisal)
    end
  rescue
    _ -> si
  catch
    _, _ -> si
  end
end
