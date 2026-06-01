defmodule Core.Brain.Introspection do
  @moduledoc """
  Core orchestration hook for Brain-owned affect and self-model updates.
  """

  @spec update_self_model(map(), keyword()) :: map()
  def update_self_model(%{} = si, opts) when is_list(opts) do
    case Keyword.get(opts, :self_model, :auto) do
      false -> si
      :off -> si
      _ -> do_update_self_model(si, opts)
    end
  end

  def update_self_model(si, _opts), do: si

  defp do_update_self_model(%{} = si, opts) do
    appraisal = Brain.AffectiveAppraisal.appraise(si)
    _ = Brain.MoodCore.apply_appraisal(appraisal)

    case Brain.Introspection.update_from_resolved(si, appraisal) do
      {:ok, self_model} ->
        mood = get_in(self_model.mood, [:mood])
        {self_model, continuity_meta} = maybe_persist_continuity(self_model, si, opts)

        si
        |> Map.put(:appraisal, appraisal)
        |> Map.put(:mood, mood || %{})
        |> Map.put(:self_model, self_model)
        |> put_self_continuity(continuity_meta)
        |> Core.Pipeline.Trace.append(
          :self_model,
          decision: continuity_decision(continuity_meta),
          reason: :resolved_input_appraisal,
          scores: %{confidence: self_model.confidence, uncertainty: self_model.uncertainty},
          meta:
            %{
              confidence: self_model.confidence,
              uncertainty: self_model.uncertainty,
              focus: self_model.focus
            }
            |> Map.merge(continuity_meta)
        )

      _ ->
        Map.put(si, :appraisal, appraisal)
    end
  end

  defp maybe_persist_continuity(self_model, _si, opts) do
    if Keyword.get(opts, :persist_self_model, true) do
      persist_opts = [
        scope: Keyword.get(opts, :self_snapshot_scope, "runtime"),
        source: Keyword.get(opts, :self_snapshot_source, "core_turn")
      ]

      case Brain.SelfContinuity.persist(self_model, persist_opts) do
        {:ok, row} ->
          continuity =
            self_model.continuity
            |> map_or_empty()
            |> Map.merge(%{
              last_snapshot_id: row.id,
              last_snapshot_scope: row.scope,
              last_snapshot_at_ms: System.system_time(:millisecond)
            })

          {%{self_model | continuity: continuity},
           %{
             continuity: :persisted,
             snapshot_id: row.id,
             snapshot_scope: row.scope,
             snapshot_source: row.source
           }}

        {:error, changeset} ->
          {self_model,
           %{
             continuity: :persist_failed,
             snapshot_errors: changeset_errors(changeset)
           }}
      end
    else
      {self_model, %{continuity: :skipped}}
    end
  end

  defp put_self_continuity(%{} = si, %{continuity: status} = meta) do
    Map.put(si, :self_continuity, Map.put(meta, :status, status))
  end

  defp put_self_continuity(si, _meta), do: si

  defp continuity_decision(%{continuity: :persisted}), do: :updated_and_persisted
  defp continuity_decision(%{continuity: :persist_failed}), do: :updated_persist_failed
  defp continuity_decision(%{continuity: :skipped}), do: :updated
  defp continuity_decision(_), do: :updated

  defp changeset_errors(%Ecto.Changeset{} = changeset) do
    Ecto.Changeset.traverse_errors(changeset, fn {message, opts} ->
      Enum.reduce(opts, message, fn {key, value}, acc ->
        String.replace(acc, "%{#{key}}", to_string(value))
      end)
    end)
  end

  defp changeset_errors(reason), do: %{error: inspect(reason)}

  defp map_or_empty(map) when is_map(map), do: map
  defp map_or_empty(_), do: %{}
end
