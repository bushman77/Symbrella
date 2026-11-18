defmodule SymbrellaWeb.CognitiveTraceStore do
  @moduledoc """
  Read-only, in-memory observer for actual instrumented symbolic chat turns.

  This process receives only allowlisted trace metadata from Core.CognitiveTrace.
  It never stores input text, tokens, generated replies, or memory contents.
  History expires and is bounded; it is not a full distributed trace system.
  """

  use GenServer

  alias Brain.Bus

  @topic "brain:cognitive_trace"
  @telemetry_event [:symbrella, :cognitive_trace, :stage]
  @max_traces 20
  @max_steps 40
  @max_age_ms 600_000

  @stages [
    :turn, :chat_route,
    :phase_1_input, :phase_2_context, :phase_3_lexical,
    :phase_4_grammar, :lifg_selection, :atl_integration,
    :phase_5_semantics, :phase_6_comprehension,
    :comprehension_summary, :speech_act_binding,
    :phase_7_planning, :prefrontal_control, :action_selection,
    :response_planning, :phase_8_memory, :hippocampus_persist
  ]

  def start_link(opts \\ []), do: GenServer.start_link(__MODULE__, opts, name: __MODULE__)

  @doc "Last observed, bounded turn summaries, newest first."
  def latest do
    if Process.whereis(__MODULE__), do: GenServer.call(__MODULE__, :latest), else: []
  end

  @impl true
  def init(_opts) do
    handler_id = {__MODULE__, self()}
    :ok = :telemetry.attach(handler_id, @telemetry_event, &__MODULE__.handle_telemetry/4, self())
    {:ok, %{traces: %{}, order: [], handler_id: handler_id}}
  end

  @doc false
  def handle_telemetry(_event, measurements, metadata, receiver) do
    send(receiver, {:cognitive_trace_event, measurements, metadata})
  end

  @impl true
  def handle_call(:latest, _from, state) do
    state = expire(state, System.system_time(:millisecond))
    {:reply, Enum.map(state.order, &Map.fetch!(state.traces, &1)), state}
  end

  @impl true
  def handle_info({:cognitive_trace_event, measurements, metadata}, state) do
    case ingest(state, measurements, metadata) do
      {updated, nil} ->
        {:noreply, updated}

      {updated, trace} ->
        Bus.broadcast(@topic, {:cognitive_trace_updated, trace})
        {:noreply, updated}
    end
  end

  @impl true
  def terminate(_reason, state) do
    :telemetry.detach(state.handler_id)
    :ok
  end

  @doc false
  def ingest(state, measurements, metadata) when is_map(state) and is_map(measurements) and is_map(metadata) do
    with id when is_binary(id) <- Map.get(metadata, :trace_id),
         true <- byte_size(id) in 6..77,
         true <- Regex.match?(~r/\Aturn-[0-9a-f]+\z/, id),
         stage when stage in @stages <- Map.get(metadata, :stage),
         status when status in [:started, :ok, :error] <- Map.get(metadata, :status),
         duration when is_integer(duration) and duration >= 0 <- Map.get(measurements, :duration_us),
         at_ms when is_integer(at_ms) <- Map.get(metadata, :at_ms) do
      clean = %{
        stage: stage,
        status: status,
        region: region_for(stage),
        duration_us: duration,
        at_ms: at_ms,
        evidence: evidence_for(stage, Map.get(metadata, :evidence))
      }

      state = expire(state, at_ms)
      old = Map.get(state.traces, id, %{
        trace_id: id,
        started_at_ms: at_ms,
        updated_at_ms: at_ms,
        status: :running,
        events: []
      })

      trace = %{
        old
        | updated_at_ms: at_ms,
          status: trace_status(old.status, stage, status),
          events: Enum.take(old.events ++ [clean], -@max_steps)
      }

      order = [id | Enum.reject(state.order, &(&1 == id))] |> Enum.take(@max_traces)
      traces = state.traces |> Map.put(id, trace) |> Map.take(order)
      {%{state | order: order, traces: traces}, trace}
    else
      _ -> {state, nil}
    end
  end

  def ingest(state, _measurements, _metadata), do: {state, nil}

  defp region_for(:lifg_selection), do: :lifg
  defp region_for(:atl_integration), do: :atl
  defp region_for(:prefrontal_control), do: :prefrontal
  defp region_for(:hippocampus_persist), do: :hippocampus
  defp region_for(_), do: nil

  defp evidence_for(:lifg_selection, %{} = raw), do: numeric(raw, [:choices])
  defp evidence_for(:comprehension_summary, %{} = raw) do
    Map.merge(numeric(raw, [:understood, :uncertain, :unresolved]),
      %{degraded: Map.get(raw, :degraded) == true})
  end
  defp evidence_for(:atl_integration, %{} = raw),
    do: %{slate_present: Map.get(raw, :slate_present) == true}
  defp evidence_for(:speech_act_binding, %{speech_act: act})
       when act in [:greeting, :gratitude, :farewell, :wellbeing_query, :not_detected],
       do: %{speech_act: act}
  @executive_regions [:dlpfc, :vmpfc, :fpc, :dmpfc, :salience]
  @planner_statuses [:produced, :no_opinion, :unavailable, :failed, :rejected]
  @planner_reasons [
    :validated, :empty_result, :module_not_loaded, :function_not_exported,
    :no_valid_control_values, :planner_exception, :planner_failure, :unknown
  ]
  @control_keys [
    :policy, :top_k, :max_retries, :branch_budget, :switch_after_ms,
    :utility_prior, :explore_rate, :confidence_scale, :acc_conflict_gain,
    :salience_boost
  ]
  @actions [
    :answer_user, :ask_clarifying_question, :store_memory, :observe_silently,
    :self_check, :refuse_or_redirect, :safe_support, :unknown
  ]
  @shadow_statuses [:evaluated, :skipped, :disabled, :failed, :unknown, :not_recorded]
  @lineage_statuses [:reconciled, :unchanged, :not_planned, :not_recorded, :unknown]
  @lineage_reasons [
    :social_intent_reconciliation, :action_preserved,
    :response_disabled, :response_unavailable,
    :initial_action_unavailable, :final_action_unavailable, :unknown
  ]
  @command_statuses [:executed, :deferred, :failed, :rejected, :unknown]
  @shadow_reasons [
    :bounded_candidate_modulation, :protected_live_decision, :no_valid_controls,
    :invalid_context, :opted_out, :shadow_exception, :unknown
  ]

  defp evidence_for(:prefrontal_control, %{} = raw) do
    planners =
      raw
      |> Map.get(:planners, [])
      |> list()
      |> Enum.take(5)
      |> Enum.flat_map(fn
        %{region: region, status: status} = planner
        when region in @executive_regions and status in @planner_statuses ->
          [%{
            region: region,
            status: status,
            reason: allow_atom(Map.get(planner, :reason), @planner_reasons, :unknown),
            proposed_keys: safe_keys(Map.get(planner, :proposed_keys)),
            merged_keys: safe_keys(Map.get(planner, :merged_keys)),
            superseded_keys: safe_keys(Map.get(planner, :superseded_keys))
          }]
        _ -> []
      end)

    %{
      available: Map.get(raw, :available) == true,
      feedback_quality:
        allow_atom(Map.get(raw, :feedback_quality), [:heuristic_proxy, :unknown], :unknown),
      planners: planners,
      controls: safe_controls(Map.get(raw, :controls))
    }
  end

  defp evidence_for(:action_selection, %{} = raw) do
    sources =
      raw
      |> Map.get(:applied_sources, [])
      |> list()
      |> Enum.take(10)
      |> Enum.flat_map(fn
        %{key: key, region: region}
          when key in @control_keys and region in @executive_regions ->
          [%{key: key, region: region}]
        _ -> []
      end)

    adjustments =
      raw
      |> Map.get(:adjustments, [])
      |> list()
      |> Enum.take(7)
      |> Enum.flat_map(fn
        %{action: action, baseline: baseline, proposed: proposed, delta: delta}
        when action in @actions and is_number(baseline) and is_number(proposed)
             and is_number(delta) ->
          if baseline >= 0.0 and baseline <= 1.0 and
               proposed >= 0.0 and proposed <= 1.0 and
               delta >= -0.121 and delta <= 0.121 do
            [%{action: action, baseline: baseline, proposed: proposed,
               delta: delta}]
          else
            []
          end
        _ -> []
      end)

    %{
      shadow_status:
        allow_atom(Map.get(raw, :shadow_status), @shadow_statuses, :unknown),
      shadow_reason:
        allow_atom(Map.get(raw, :shadow_reason), @shadow_reasons, :unknown),
      live_action: allow_atom(Map.get(raw, :live_action), @actions, :unknown),
      shadow_action: allow_atom(Map.get(raw, :shadow_action), @actions, :unknown),
      changed: Map.get(raw, :changed) == true,
      applied_keys: safe_keys(Map.get(raw, :applied_keys)),
      unused_keys: safe_keys(Map.get(raw, :unused_keys)),
      applied_sources: sources,
      adjustments: adjustments
    }
  end

  defp evidence_for(:response_planning, %{} = raw) do
    %{
      initial_action: allow_atom(Map.get(raw, :initial_action), @actions, :unknown),
      final_action: allow_atom(Map.get(raw, :final_action), @actions, :unknown),
      initial_safety_gate:
        allow_atom(Map.get(raw, :initial_safety_gate), [:approved, :redirected], :unknown),
      final_safety_gate:
        allow_atom(Map.get(raw, :final_safety_gate), [:approved, :redirected], :unknown),
      reconciliation:
        allow_atom(Map.get(raw, :reconciliation), @lineage_statuses, :unknown),
      reason: allow_atom(Map.get(raw, :reason), @lineage_reasons, :unknown),
      shadow_action: allow_atom(Map.get(raw, :shadow_action), @actions, :unknown),
      shadow_status:
        allow_atom(Map.get(raw, :shadow_status), @shadow_statuses, :unknown),
      shadow_applied_keys: safe_keys(Map.get(raw, :shadow_applied_keys)),
      initial_command_statuses:
        bounded_statuses(Map.get(raw, :initial_command_statuses)),
      final_command_statuses:
        bounded_statuses(Map.get(raw, :final_command_statuses)),
      delivery_status: :not_observed
    }
  end

  defp bounded_statuses(items) when is_list(items) do
    items
    |> Enum.take(12)
    |> Enum.map(&allow_atom(&1, @command_statuses, :unknown))
  end

  defp bounded_statuses(_), do: []

  defp evidence_for(_, _), do: %{}

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

  defp safe_keys(value) do
    value
    |> list()
    |> Enum.filter(&(&1 in @control_keys))
    |> Enum.uniq()
    |> Enum.take(10)
  end

  defp list(value) when is_list(value), do: value
  defp list(_), do: []

  defp allow_atom(value, allowed, fallback),
    do: if(value in allowed, do: value, else: fallback)

  defp numeric(map, keys) do
    for key <- keys, value = Map.get(map, key), is_integer(value),
        value >= 0 and value <= 10_000, into: %{}, do: {key, value}
  end

  defp trace_status(_old, :turn, :started), do: :running
  defp trace_status(_old, :turn, :ok), do: :completed
  defp trace_status(_old, :turn, :error), do: :error
  defp trace_status(old, _stage, _status), do: old

  defp expire(state, now_ms) do
    kept =
      state.order
      |> Enum.filter(fn id ->
        trace = Map.fetch!(state.traces, id)
        now_ms - trace.updated_at_ms < @max_age_ms
      end)
      |> Enum.take(@max_traces)

    %{state | order: kept, traces: Map.take(state.traces, kept)}
  end
end
