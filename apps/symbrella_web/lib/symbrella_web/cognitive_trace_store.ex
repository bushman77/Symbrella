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
    :phase_7_planning, :phase_8_memory, :hippocampus_persist
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
  defp evidence_for(_, _), do: %{}

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
