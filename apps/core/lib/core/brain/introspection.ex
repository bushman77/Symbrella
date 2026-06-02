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
        self_monitor = monitor_self_model(self_model, opts)

        si
        |> Map.put(:appraisal, appraisal)
        |> Map.put(:mood, mood || %{})
        |> Map.put(:self_model, self_model)
        |> Map.put(:self_monitor, self_monitor)
        |> put_self_continuity(continuity_meta)
        |> Core.Pipeline.Trace.append(
          :meta_monitor,
          decision: self_monitor.status,
          reason: :self_state_checked,
          scores: %{warning_count: length(self_monitor.warnings)},
          meta: %{
            warning_kinds: Enum.map(self_monitor.warnings, & &1.kind),
            severities: Enum.map(self_monitor.warnings, & &1.severity),
            recovery_suggestions: self_monitor.recovery_suggestions
          }
        )
        |> maybe_recall_self_monitor_memories(self_model, self_monitor, opts)
        |> maybe_write_self_monitor_memory(self_model, self_monitor, opts)
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

  defp monitor_self_model(%Brain.SelfModel{} = self_model, opts) do
    monitor_opts =
      opts
      |> Keyword.get(:self_monitor_thresholds, Keyword.get(opts, :meta_monitor_thresholds, %{}))
      |> monitor_thresholds()
      |> then(&[thresholds: &1])

    {status, warnings} = Brain.MetaMonitor.check(self_model, monitor_opts)

    %{
      status: status,
      warnings: warnings,
      recovery_suggestions: Brain.MetaMonitor.recovery_suggestions(warnings)
    }
  end

  defp monitor_thresholds(thresholds) when is_map(thresholds) or is_list(thresholds), do: thresholds
  defp monitor_thresholds(_), do: %{}

  defp maybe_recall_self_monitor_memories(
         %{} = si,
         %Brain.SelfModel{} = self_model,
         %{warnings: warnings, recovery_suggestions: suggestions},
         opts
       ) do
    if Keyword.get(opts, :recall_self_monitor_memories, true) and
         Core.Brain.Runtime.pid_alive?(Brain.Hippocampus) do
      limit = Keyword.get(opts, :self_monitor_memory_recall_limit, 3)
      cues = self_monitor_memory_cues(self_model, warnings, suggestions)

      memories =
        Core.Brain.Runtime.apply_if_exported(
          Brain.Hippocampus,
          :recall_self_memories,
          [cues, [limit: limit, ignore_head: :never]],
          []
        )

      recall = summarize_self_memories(memories)
      decision = if recall.memories == [], do: :none, else: :recalled

      si
      |> put_self_memory_recall(recall)
      |> Core.Pipeline.Trace.append(
        :self_memory_recall,
        decision: decision,
        reason: :self_monitor_context,
        scores: %{memory_count: length(recall.memories)},
        meta: %{
          cues: cues,
          recovery_suggestions: recall.recovery_suggestions,
          warning_kinds: recall.warning_kinds
        }
      )
    else
      si
    end
  end

  defp maybe_recall_self_monitor_memories(si, _self_model, _self_monitor, _opts), do: si

  defp self_monitor_memory_cues(%Brain.SelfModel{} = self_model, warnings, suggestions) do
    ["self_monitor_warning", self_model.focus | warning_kinds(warnings) ++ List.wrap(suggestions)]
    |> Enum.map(&to_string/1)
    |> Enum.reject(&(&1 == ""))
    |> Enum.uniq()
  end

  defp summarize_self_memories(memories) do
    summaries =
      memories
      |> List.wrap()
      |> Enum.map(&summarize_self_memory/1)
      |> Enum.reject(&is_nil/1)

    %{
      source: :hippocampus,
      memories: summaries,
      warning_kinds:
        summaries
        |> Enum.flat_map(&List.wrap(Map.get(&1, :warning_kinds, [])))
        |> Enum.uniq(),
      recovery_suggestions:
        summaries
        |> Enum.flat_map(&List.wrap(Map.get(&1, :recovery_suggestions, [])))
        |> Enum.uniq()
    }
  end

  defp summarize_self_memory(%{score: score, at: at, episode: %{meta: meta, slate: slate}}) do
    payload = map_or_empty(get_in_map(slate, [:payload]) || get_in_map(meta, [:payload]))

    %{
      kind: get_in_map(meta, [:kind]),
      score: score,
      at: at,
      warning_kinds: List.wrap(get_in_map(payload, [:warning_kinds])),
      recovery_suggestions: List.wrap(get_in_map(payload, [:recovery_suggestions])),
      focus: get_in_map(payload, [:focus]),
      uncertainty: get_in_map(payload, [:uncertainty]),
      cognitive_load: get_in_map(payload, [:cognitive_load])
    }
    |> Enum.reject(fn {_key, value} -> is_nil(value) or value == [] end)
    |> Map.new()
  end

  defp summarize_self_memory(_), do: nil

  defp put_self_memory_recall(si, %{memories: []}), do: si
  defp put_self_memory_recall(si, recall), do: Map.put(si, :self_memory_recall, recall)

  defp maybe_write_self_monitor_memory(
         %{} = si,
         %Brain.SelfModel{} = self_model,
         %{status: :warning, warnings: warnings, recovery_suggestions: suggestions},
         opts
       ) do
    if Keyword.get(opts, :write_self_monitor_memory, true) and
         Core.Brain.Runtime.pid_alive?(Brain.Hippocampus) do
      payload = self_monitor_memory_payload(self_model, warnings, suggestions, si)

      _ =
        Core.Brain.Runtime.apply_if_exported(
          Brain.Hippocampus,
          :write_self_memory,
          [
            :self_monitor_warning,
            payload,
            %{
              source: :core_introspection,
              session_id: Map.get(si, :session_id),
              trace_stage: :meta_monitor
            }
          ],
          nil
        )

      Core.Pipeline.Trace.append(si, :self_memory,
        decision: :written,
        reason: :self_monitor_warning,
        scores: %{warning_count: length(List.wrap(warnings))},
        meta: %{
          kind: :self_monitor_warning,
          warning_kinds: Map.get(payload, :warning_kinds, []),
          recovery_suggestions: Map.get(payload, :recovery_suggestions, [])
        }
      )
    else
      si
    end
  end

  defp maybe_write_self_monitor_memory(si, _self_model, _self_monitor, _opts), do: si

  defp self_monitor_memory_payload(%Brain.SelfModel{} = self_model, warnings, suggestions, si) do
    %{
      warning_kinds: warning_kinds(warnings),
      recovery_suggestions: List.wrap(suggestions),
      confidence: self_model.confidence,
      uncertainty: self_model.uncertainty,
      stability: self_model.stability,
      cognitive_load: self_model.cognitive_load,
      focus: self_model.focus,
      self_model_v: self_model.v,
      continuity_status: get_in_map(si, [:self_continuity, :status]),
      session_id: Map.get(si, :session_id)
    }
    |> Enum.reject(fn {_key, value} -> is_nil(value) end)
    |> Map.new()
  end

  defp warning_kinds(warnings) do
    warnings
    |> List.wrap()
    |> Enum.map(&get_in_map(&1, [:kind]))
    |> Enum.reject(&is_nil/1)
    |> Enum.uniq()
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

  defp get_in_map(map, [key | rest]) when is_map(map) do
    map
    |> Map.get(key, Map.get(map, to_string(key)))
    |> get_in_map(rest)
  end

  defp get_in_map(value, []), do: value
  defp get_in_map(_value, _path), do: nil
end
