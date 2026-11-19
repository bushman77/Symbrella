defmodule Core.CognitiveTrace do
  @moduledoc """
  Opt-in, privacy-bounded execution instrumentation for one symbolic chat turn.

  The trace context belongs to the calling worker process. Tracing records
  elapsed time and whitelisted outcome counters, never utterance text, lexical
  candidates, memory contents or generated replies. An instrumented stage
  completing means the function returned; it does not certify comprehension.
  """

  @context_key {__MODULE__, :turn_id}
  @event [:symbrella, :cognitive_trace, :stage]

  @regions %{
    lifg_selection: :lifg,
    atl_integration: :atl,
    prefrontal_control: :prefrontal,
    hippocampus_persist: :hippocampus
  }

  @planner_regions %{
    Brain.DLPFC.Policy => :dlpfc,
    Brain.VmPFC => :vmpfc,
    Brain.FPC => :fpc,
    Brain.DmPFC => :dmpfc,
    Brain.Salience => :salience
  }

  @control_keys [
    :policy, :top_k, :max_retries, :branch_budget, :switch_after_ms,
    :utility_prior, :explore_rate, :confidence_scale, :acc_conflict_gain,
    :salience_boost
  ]
  @actions [
    :answer_user, :ask_clarifying_question, :store_memory, :observe_silently,
    :self_check, :refuse_or_redirect, :safe_support
  ]
  @planner_statuses [:produced, :no_opinion, :unavailable, :failed, :rejected]
  @shadow_statuses [:evaluated, :skipped, :disabled, :failed]
  @lineage_statuses [:reconciled, :unchanged, :not_planned, :not_recorded]
  @lineage_reasons [
    :social_intent_reconciliation, :action_preserved,
    :response_disabled, :response_unavailable,
    :initial_action_unavailable, :final_action_unavailable
  ]
  @command_statuses [:executed, :deferred, :failed, :rejected]
  @shadow_reasons [
    :bounded_candidate_modulation, :protected_live_decision,
    :no_valid_controls, :invalid_context, :opted_out, :shadow_exception
  ]

  @doc "Run a worker under a turn-scoped trace ID and always restore context."
  def with_trace("turn-" <> _ = trace_id, fun)
      when is_binary(trace_id) and byte_size(trace_id) <= 80 and is_function(fun, 0) do
    previous = Process.get(@context_key)
    Process.put(@context_key, trace_id)
    started = System.monotonic_time()

    emit(trace_id, :turn, :started, 0, %{})

    try do
      result = fun.()
      emit(trace_id, :turn, :ok, elapsed(started), %{})
      result
    rescue
      error ->
        emit(trace_id, :turn, :error, elapsed(started), %{})
        reraise(error, __STACKTRACE__)
    catch
      kind, reason ->
        emit(trace_id, :turn, :error, elapsed(started), %{})
        :erlang.raise(kind, reason, __STACKTRACE__)
    after
      if is_nil(previous), do: Process.delete(@context_key), else: Process.put(@context_key, previous)
    end
  end

  def with_trace(_trace_id, fun) when is_function(fun, 0), do: fun.()

  @doc "Time the actual invocation without modifying its inputs or return value."
  def stage(stage, fun) when is_atom(stage) and is_function(fun, 0) do
    case Process.get(@context_key) do
      "turn-" <> _ = trace_id ->
        started = System.monotonic_time()

        try do
          result = fun.()
          emit(trace_id, stage, :ok, elapsed(started), evidence(stage, result))
          result
        rescue
          error ->
            emit(trace_id, stage, :error, elapsed(started), %{})
            reraise(error, __STACKTRACE__)
        catch
          kind, reason ->
            emit(trace_id, stage, :error, elapsed(started), %{})
            :erlang.raise(kind, reason, __STACKTRACE__)
        end

      _ ->
        fun.()
    end
  end

  @doc "Pipe-safe equivalent of stage/2."
  def step(value, stage, fun) when is_function(fun, 1),
    do: stage(stage, fn -> fun.(value) end)

  defp evidence(stage, result) when is_map(result) do
    case stage do
      :lifg_selection ->
        %{choices: count(Map.get(result, :lifg_choices))}

      :atl_integration ->
        %{slate_present: not is_nil(Map.get(result, :atl_slate))}

      :comprehension_summary ->
        summary = Map.get(result, :comprehension) || %{}

        if is_map(summary) do
          %{
            degraded: Map.get(summary, :degraded?) == true,
            understood: count(Map.get(summary, :understood)),
            uncertain: count(Map.get(summary, :uncertain)),
            unresolved: count(Map.get(summary, :unresolved))
          }
          |> Map.merge(Core.Comprehension.Diagnosis.assess(result))
        else
          %{}
        end

      :speech_act_binding ->
        case Map.get(result, :speech_act_frame) do
          %{speech_act: act} when act in [:greeting, :gratitude, :farewell, :wellbeing_query] ->
            %{speech_act: act}

          _ ->
            %{speech_act: :not_detected}
        end

      :prefrontal_control ->
        prefrontal_evidence(Map.get(result, :prefrontal))

      :action_selection ->
        action_evidence(Map.get(result, :action_meta))

      :response_planning ->
        decision_lineage_evidence(Map.get(result, :decision_lineage))

      _ ->
        %{}
    end
  end

  defp evidence(_stage, _result), do: %{}

  # Only small numeric/atom control summaries cross the telemetry boundary.
  # Never include candidate text, input context, module exception details or
  # arbitrary metadata from a planner.
  defp prefrontal_evidence(%{} = report) do
    planners =
      report
      |> Map.get(:planners, [])
      |> List.wrap()
      |> Enum.take(5)
      |> Enum.flat_map(fn
        %{planner: mod, status: status} = item ->
          case Map.get(@planner_regions, mod) do
            nil -> []
            region when status in @planner_statuses ->
              [%{
                region: region,
                status: status,
                reason: planner_reason(Map.get(item, :reason)),
                proposed_keys: control_keys(Map.get(item, :proposed_keys)),
                merged_keys: control_keys(Map.get(item, :accepted_keys)),
                superseded_keys: control_keys(Map.get(item, :superseded_keys))
              }]
            _ -> []
          end

        _ -> []
      end)

    %{
      available: true,
      planners: planners,
      controls: safe_controls(Map.get(report, :signals)),
      feedback_quality:
        if(Map.get(report, :feedback_quality) == :heuristic_proxy,
          do: :heuristic_proxy, else: :unknown)
    }
  end

  defp prefrontal_evidence(_), do: %{available: false}

  defp action_evidence(%{} = meta) do
    shadow = Map.get(meta, :prefrontal_shadow)

    case shadow do
      %{} = comparison ->
        status = Map.get(comparison, :status)
        reason = Map.get(comparison, :reason)
        live = Map.get(comparison, :live_selected)
        proposed = Map.get(comparison, :shadow_selected)
        sources =
          comparison
          |> Map.get(:applied_sources, [])
          |> List.wrap()
          |> Enum.take(10)
          |> Enum.flat_map(fn
            %{key: key, planner: planner} ->
              with true <- key in @control_keys,
                   region when not is_nil(region) <- Map.get(@planner_regions, planner) do
                [%{key: key, region: region}]
              else
                _ -> []
              end
            _ -> []
          end)

        adjustments =
          comparison
          |> Map.get(:adjustments, [])
          |> List.wrap()
          |> Enum.take(7)
          |> Enum.flat_map(fn
            %{action: action, baseline_score: baseline, shadow_score: adjusted, delta: delta}
              when action in @actions and is_number(baseline) and
                   is_number(adjusted) and is_number(delta) ->
              if baseline >= 0.0 and baseline <= 1.0 and
                   adjusted >= 0.0 and adjusted <= 1.0 and
                   delta >= -0.121 and delta <= 0.121 do
                [%{action: action, baseline: baseline, proposed: adjusted,
                   delta: delta}]
              else
                []
              end
            _ -> []
          end)

        %{
          shadow_status: if(status in @shadow_statuses, do: status, else: :unknown),
          shadow_reason: if(reason in @shadow_reasons, do: reason, else: :unknown),
          live_action: if(live in @actions, do: live, else: :unknown),
          shadow_action: if(proposed in @actions, do: proposed, else: :unknown),
          changed: Map.get(comparison, :changed?) == true,
          applied_keys: control_keys(Map.get(comparison, :applied_keys)),
          unused_keys: control_keys(Map.get(comparison, :unused_keys)),
          applied_sources: sources,
          adjustments: adjustments
        }

      _ -> %{shadow_status: :not_recorded}
    end
  end

  defp action_evidence(_), do: %{shadow_status: :not_recorded}

  # Only explicit internal enum fields and bounded command receipt statuses
  # cross the telemetry boundary. Never forward the SI, candidate, response
  # text, decision trace IDs, or an unsanitized lineage map.
  defp decision_lineage_evidence(%{} = lineage) do
    %{
      initial_action: allow_action(Map.get(lineage, :initial_action)),
      final_action: allow_action(Map.get(lineage, :final_action)),
      initial_safety_gate: allow_safety(Map.get(lineage, :initial_safety_gate)),
      final_safety_gate: allow_safety(Map.get(lineage, :final_safety_gate)),
      reconciliation:
        allow_enum(Map.get(lineage, :reconciliation), @lineage_statuses),
      reason: allow_enum(Map.get(lineage, :reason), @lineage_reasons),
      shadow_action: allow_action(Map.get(lineage, :shadow_action)),
      shadow_status: allow_enum(Map.get(lineage, :shadow_status), @shadow_statuses),
      shadow_applied_keys: control_keys(Map.get(lineage, :shadow_applied_keys)),
      initial_command_statuses:
        safe_command_statuses(Map.get(lineage, :initial_command_statuses)),
      final_command_statuses:
        safe_command_statuses(Map.get(lineage, :final_command_statuses)),
      delivery_status: :not_observed
    }
  end

  defp decision_lineage_evidence(_), do: %{reconciliation: :not_recorded}

  defp allow_action(action) when action in @actions, do: action
  defp allow_action(_), do: :unknown

  defp allow_safety(safety) when safety in [:approved, :redirected], do: safety
  defp allow_safety(_), do: :unknown

  defp allow_enum(value, accepted) do
    if value in accepted, do: value, else: :unknown
  end

  defp safe_command_statuses(items) when is_list(items) do
    items
    |> Enum.take(12)
    |> Enum.map(fn status ->
      if status in @command_statuses, do: status, else: :unknown
    end)
  end

  defp safe_command_statuses(_), do: []

  defp planner_reason(reason)
       when reason in [:validated, :empty_result, :module_not_loaded,
                       :function_not_exported, :no_valid_control_values,
                       :planner_exception, :planner_failure], do: reason
  defp planner_reason(_), do: :unknown

  defp control_keys(keys) when is_list(keys) do
    keys
    |> Enum.filter(&(&1 in @control_keys))
    |> Enum.uniq()
    |> Enum.take(10)
  end

  defp control_keys(_), do: []

  defp safe_controls(%{} = raw) do
    for key <- @control_keys,
        value = Map.get(raw, key),
        valid_control?(key, value),
        into: %{}, do: {key, value}
  end

  defp safe_controls(_), do: %{}

  defp valid_control?(:policy, value),
    do: value in [:strict_mwe, :loose_single, :balanced]
  defp valid_control?(key, value)
       when key in [:top_k, :max_retries, :branch_budget, :switch_after_ms],
       do: is_integer(value) and value >= 0 and value <= 5_000
  defp valid_control?(_key, value),
    do: is_number(value) and value >= 0.0 and value <= 1.5

  defp count(list) when is_list(list), do: length(list)
  defp count(map) when is_map(map), do: map_size(map)
  defp count(_), do: 0

  defp elapsed(started),
    do: System.convert_time_unit(System.monotonic_time() - started, :native, :microsecond)

  defp emit(trace_id, stage, status, duration_us, evidence) do
    measurements = %{duration_us: duration_us}
    metadata = %{
      trace_id: trace_id,
      stage: stage,
      status: status,
      region: Map.get(@regions, stage),
      evidence: evidence,
      at_ms: System.system_time(:millisecond)
    }

    # Observability must never interrupt symbolic processing.
    try do
      :telemetry.execute(@event, measurements, metadata)
    rescue
      _ -> :ok
    catch
      _, _ -> :ok
    end
  end
end
