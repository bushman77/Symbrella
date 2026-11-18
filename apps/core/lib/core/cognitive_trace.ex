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
    hippocampus_persist: :hippocampus
  }

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

      _ ->
        %{}
    end
  end

  defp evidence(_stage, _result), do: %{}

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
