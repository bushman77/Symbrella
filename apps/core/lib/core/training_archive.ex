defmodule Core.TrainingArchive do
  @moduledoc """
  Opt-in, append-only JSONL observations for future neural training.

  This archive records *observations*, not verified training labels. It
  captures completed chat turns together with hippocampal recall evidence
  and fact-memory operations. It never changes retrieval or response decisions.

  Files are local/private by default and are stored in umbrella-root priv/.
  The raw Core.StateDump remains a separate diagnostic facility.
  """

  @default_dir "priv"
  @schema_version 1

  @cognitive_fields [
    :sentence,
    :intent,
    :confidence,
    :conversation_act,
    :context_intent,
    :selected_action,
    :action_meta,
    :action_candidates,
    :tokens,
    :sense_candidates,
    :lifg_choices,
    :semantic_choices,
    :sentence_signatures,
    :comprehension,
    :response_meta,
    :trace,
    :mood,
    :appraisal,
    :self_model,
    :self_monitor,
    :self_memory_recall,
    :symbolic_frame
  ]

  @doc """
  Store one completed human/assistant exchange.

  Required attributes: :turn_id, :session_id, :user_text, :assistant_text.
  Optional: :assistant_message_id, :si, :response_meta.
  Disabled by default. Pass `config: [enabled?: true, dir: ...]` in tests.
  """
  @spec record_turn(map(), keyword()) :: :ok | :disabled | {:error, term()}
  def record_turn(attrs, opts \\ [])

  def record_turn(%{} = attrs, opts) when is_list(opts) do
    config = Keyword.get(opts, :config, Application.get_env(:core, __MODULE__, []))

    if Keyword.get(config, :enabled?, false) == true do
      with :ok <- validate(attrs),
           {:ok, turn, memory_events} <- build_records(attrs),
           :ok <- persist(config, turn, memory_events) do
        maybe_notify_observers(config, turn, memory_events)
        :ok
      end
    else
      :disabled
    end
  rescue
    _error -> {:error, :archive_exception}
  end

  def record_turn(_, _), do: {:error, :invalid_turn}

  # Observation publication is strictly best-effort, after persistence, and
  # never changes the archive's return status or the live response pathway.
  # The subscriber receives the EXACT JSON-safe turn and memory events that
  # the offline trainer reads; no competing cognitive reconstruction occurs.
  defp maybe_notify_observers(config, turn, memory_events) do
    if Keyword.get(config, :enabled?, false) == true do
      :telemetry.execute(
        [:core, :training_archive, :completed_turn],
        %{count: 1},
        %{turn: json_safe(turn), events: json_safe(memory_events)}
      )
    end

    :ok
  rescue
    _ -> :ok
  catch
    _, _ -> :ok
  end

  @doc """
  Append an explicitly human-reviewed correction linked to an archived turn.

  Required: :turn_id, :session_id, :reviewed_by and a nonempty :label map.
  Feedback remains a separate file so raw observations are never overwritten.
  No model training or weight update occurs in this function.
  """
  @spec record_feedback(map(), keyword()) :: :ok | :disabled | {:error, term()}
  def record_feedback(attrs, opts \\ [])

  def record_feedback(%{} = attrs, opts) when is_list(opts) do
    config = Keyword.get(opts, :config, Application.get_env(:core, __MODULE__, []))

    if Keyword.get(config, :enabled?, false) == true do
      fields = [:turn_id, :session_id, :reviewed_by]

      valid_fields? =
        Enum.all?(fields, fn field ->
          value = Map.get(attrs, field)
          is_binary(value) and String.trim(value) != ""
        end)

      case {valid_fields?, Map.get(attrs, :label)} do
        {true, %{} = label} when map_size(label) > 0 ->
          event = %{
            schema_version: @schema_version,
            record_type: "human_feedback",
            recorded_at: DateTime.utc_now() |> DateTime.to_iso8601(),
            session_id: attrs.session_id,
            turn_id: attrs.turn_id,
            reviewed_by: attrs.reviewed_by,
            source: "human_review",
            correction: Map.get(attrs, :correction) == true,
            label: json_safe(label),
            note: json_safe(Map.get(attrs, :note))
          }

          persist_feedback(config, event)

        _ ->
          {:error, :invalid_human_feedback}
      end
    else
      :disabled
    end
  rescue
    _error -> {:error, :archive_exception}
  end

  def record_feedback(_, _), do: {:error, :invalid_human_feedback}

  defp validate(attrs) do
    if Enum.all?([:turn_id, :session_id, :user_text, :assistant_text], fn field ->
         value = Map.get(attrs, field)
         is_binary(value) and String.trim(value) != ""
       end) do
      :ok
    else
      {:error, :missing_turn_fields}
    end
  end

  defp build_records(attrs) do
    si = map_or_empty(Map.get(attrs, :si))
    meta = map_or_empty(Map.get(attrs, :response_meta))
    turn_id = Map.fetch!(attrs, :turn_id)
    session_id = Map.fetch!(attrs, :session_id)

    turn = %{
      schema_version: @schema_version,
      record_type: "completed_turn",
      recorded_at: DateTime.utc_now() |> DateTime.to_iso8601(),
      turn_id: turn_id,
      session_id: session_id,
      assistant_message_id: Map.get(attrs, :assistant_message_id),
      user_text: Map.fetch!(attrs, :user_text),
      assistant_text: Map.fetch!(attrs, :assistant_text),
      response_meta: json_safe(meta),
      cognition: cognition(si),
      # These describe observation completeness, not inferred cognition.
      pipeline_status: json_safe(Map.get(attrs, :pipeline_status)),
      runtime_observation: json_safe(Map.get(attrs, :runtime_observation)),
      hippocampal_trace: trace_summary(Map.get(attrs, :hippocampal_trace)),
      training: %{review_status: "unreviewed", label: nil}
    }

    traced = trace_events(attrs, session_id, turn_id)
    traced_episodes =
      traced
      |> Enum.filter(&(&1.kind == "episodic_recall"))
      |> Enum.map(& &1.episode_fingerprint)
      |> MapSet.new()

    # The attached-SI projection is a compatibility fallback. If the same
    # retrieval was already traced at the Hippocampus boundary, do not duplicate it.
    attached =
      episodic_events(si, session_id, turn_id)
      |> Enum.reject(&MapSet.member?(traced_episodes, &1.episode_fingerprint))

    events = traced ++ attached ++ fact_memory_events(meta, session_id, turn_id)

    {:ok, turn, events}
  end

  # Missing trace metadata is UNKNOWN, never interpreted as a negative query.
  # The combined trace covers the two observed LiveView processes only.
  defp trace_summary(%{} = input) do
    worker = map_or_empty(fetch(input, :worker))
    final = map_or_empty(fetch(input, :final))
    worker_status = fetch(worker, :status)
    final_status = fetch(final, :status)
    complete? = worker_status in ["complete", :complete] and
                final_status in ["complete", :complete]
    attempts = if complete?, do: trace_count(worker, :recall_attempts) +
                                  trace_count(final, :recall_attempts), else: nil
    misses = if complete?, do: trace_count(worker, :recall_misses) +
                                trace_count(final, :recall_misses), else: nil

    %{
      status: if(complete?, do: "complete", else: "incomplete"),
      scope: "worker_and_final_liveview_process",
      recall_status:
        cond do
          not complete? -> "unknown"
          attempts == 0 -> "not_attempted"
          true -> "attempted"
        end,
      recall_attempts: attempts,
      recall_misses: misses,
      worker: json_safe(worker),
      final: json_safe(final)
    }
  end

  defp trace_summary(_), do: %{
    status: "unknown", scope: "unknown", recall_status: "unknown",
    recall_attempts: nil, recall_misses: nil
  }

  defp trace_count(map, key) do
    case fetch(map, key) do
      n when is_integer(n) and n >= 0 -> n
      _ -> 0
    end
  end

  defp cognition(si) do
    @cognitive_fields
    |> Enum.reduce(%{}, fn field, acc ->
      case fetch(si, field) do
        nil -> acc
        value -> Map.put(acc, field, json_safe(value))
      end
    end)
  end

  # Brain.Hippocampus.TurnTrace supplies events from the chat worker.
  # Core owns JSONL persistence so the Brain -> Core dependency direction
  # stays acyclic. Identifiers are joined here, on the completed turn.
  defp trace_events(attrs, session_id, turn_id) do
    case fetch(attrs, :hippocampal_events) do
      events when is_list(events) ->
        events
        |> Enum.flat_map(&trace_event(&1, session_id, turn_id))

      _ ->
        []
    end
  end

  defp trace_event(raw, session_id, turn_id) when is_map(raw) do
    base = %{
      schema_version: @schema_version,
      record_type: "memory_event",
      session_id: session_id,
      turn_id: turn_id,
      recorded_at: DateTime.utc_now() |> DateTime.to_iso8601(),
      review_status: "unreviewed"
    }

    case fetch(raw, :kind) do
      :recall_attempt ->
        [Map.merge(base, %{
          kind: "recall_attempt",
          recall_id: fetch(raw, :recall_id),
          operation: json_safe(fetch(raw, :operation)),
          source: json_safe(fetch(raw, :source)),
          cues: json_safe(fetch(raw, :cues)),
          cue_count: fetch(raw, :cue_count),
          limit: fetch(raw, :limit),
          min_jaccard: fetch(raw, :min_jaccard),
          result_count: fetch(raw, :result_count),
          hit: fetch(raw, :hit?)
        })]

      :episodic_recall ->
        trace_episode(base, "episodic_recall", raw)

      :episodic_encode ->
        trace_episode(base, "episodic_encode", raw)

      :fact_lookup ->
        [Map.merge(base, %{
          kind: "fact_lookup",
          memory_key: json_safe(fetch(raw, :memory_key)),
          found: fetch(raw, :found?),
          result: json_safe(fetch(raw, :result))
        })]

      :trace_truncated ->
        [Map.merge(base, %{
          kind: "trace_truncated",
          limit: fetch(raw, :limit)
        })]

      _ ->
        []
    end
  end

  defp trace_event(_raw, _session_id, _turn_id), do: []

  defp trace_episode(base, kind, raw) do
    case fetch(raw, :episode) do
      %{} = episode ->
        at = fetch(raw, :episode_at_ms)

        [Map.merge(base, %{
          kind: kind,
          recall_id: fetch(raw, :recall_id),
          operation: json_safe(fetch(raw, :operation)),
          rank: fetch(raw, :rank),
          score: fetch(raw, :score),
          episode_at_ms: at,
          episode_fingerprint: fingerprint({at, episode}),
          episode: json_safe(episode),
          used_in_response: fetch(raw, :used_in_response)
        })]

      _ ->
        []
    end
  end

  defp episodic_events(si, session_id, turn_id) do
    evidence = si |> fetch(:evidence) |> map_or_empty()
    episodes = evidence |> fetch(:episodes) |> List.wrap()

    episodes
    |> Enum.with_index(1)
    |> Enum.map(fn {result, rank} ->
      result = map_or_empty(result)
      episode = result |> fetch(:episode) |> map_or_empty()

      %{
        schema_version: @schema_version,
        record_type: "memory_event",
        kind: "episodic_recall",
        session_id: session_id,
        turn_id: turn_id,
        rank: rank,
        score: fetch(result, :score),
        episode_at_ms: fetch(result, :at),
        # Content fingerprint for joining observations; NOT a durable DB episode ID.
        episode_fingerprint: fingerprint({fetch(result, :at), episode}),
        episode: json_safe(episode),
        used_in_response: nil,
        review_status: "unreviewed"
      }
    end)
  end

  defp fact_memory_events(meta, session_id, turn_id) do
    source = fetch(meta, :memory_source) || fetch(meta, :response_source)
    action = fetch(meta, :action)

    if source in [:hippocampus_fact, "hippocampus_fact", :memory, "memory"] and
         action in [:identity, :recall_fact, :remember_fact, "identity", "recall_fact", "remember_fact"] do
      kind =
        if action in [:remember_fact, "remember_fact"], do: "fact_write", else: "fact_recall"

      [
        %{
          schema_version: @schema_version,
          record_type: "memory_event",
          kind: kind,
          session_id: session_id,
          turn_id: turn_id,
          memory_key: json_safe(fetch(meta, :memory_key) || fetch(meta, :fact_key)),
          source: json_safe(source),
          used_in_response: kind == "fact_recall",
          review_status: "unreviewed"
        }
      ]
    else
      []
    end
  end

  defp fingerprint(value) do
    value
    |> :erlang.term_to_binary()
    |> then(&:crypto.hash(:sha256, &1))
    |> Base.encode16(case: :lower)
  end

  defp persist(config, turn, events) do
    dir = config |> Keyword.get(:dir, @default_dir) |> Path.expand()
    turns_path = Path.join(dir, "turns.jsonl")
    memory_path = Path.join(dir, "memory_events.jsonl")

    with :ok <- File.mkdir_p(dir),
         :ok <- File.chmod(dir, 0o700) do
      case :global.trans({{__MODULE__, dir}, self()}, fn ->
             with :ok <- append_json_line(turns_path, turn),
                  :ok <- append_json_lines(memory_path, events) do
               :ok
             end
           end) do
        :ok -> :ok
        {:error, _} = error -> error
        {:aborted, reason} -> {:error, {:lock_aborted, reason}}
        other -> {:error, {:archive_write_failed, other}}
      end
    end
  end

  defp persist_feedback(config, event) do
    dir = config |> Keyword.get(:dir, @default_dir) |> Path.expand()
    path = Path.join(dir, "feedback.jsonl")

    with :ok <- File.mkdir_p(dir),
         :ok <- File.chmod(dir, 0o700) do
      case :global.trans({{__MODULE__, dir}, self()}, fn ->
             append_json_line(path, event)
           end) do
        :ok -> :ok
        {:error, _} = error -> error
        {:aborted, reason} -> {:error, {:lock_aborted, reason}}
        other -> {:error, {:archive_write_failed, other}}
      end
    end
  end

  defp append_json_lines(_path, []), do: :ok

  defp append_json_lines(path, events) do
    Enum.reduce_while(events, :ok, fn event, :ok ->
      case append_json_line(path, event) do
        :ok -> {:cont, :ok}
        error -> {:halt, error}
      end
    end)
  end

  defp append_json_line(path, value) do
    with {:ok, line} <- Jason.encode(value),
         :ok <- create_if_missing(path),
         :ok <- File.chmod(path, 0o600),
         :ok <- File.write(path, line <> "\n", [:append, :binary]) do
      :ok
    end
  end

  defp create_if_missing(path) do
    case File.open(path, [:write, :exclusive]) do
      {:ok, device} -> File.close(device)
      {:error, :eexist} -> :ok
      {:error, reason} -> {:error, reason}
    end
  end

  defp fetch(%{} = map, key) when is_atom(key) do
    Map.get(map, key, Map.get(map, Atom.to_string(key)))
  end

  defp fetch(_, _), do: nil

  defp map_or_empty(%{} = map), do: map
  defp map_or_empty(_), do: %{}

  defp json_safe(%MapSet{} = set), do: Enum.map(MapSet.to_list(set), &json_safe/1)
  defp json_safe(%DateTime{} = date), do: DateTime.to_iso8601(date)
  defp json_safe(%NaiveDateTime{} = date), do: NaiveDateTime.to_iso8601(date)

  defp json_safe(%{} = value) do
    value = if is_struct(value), do: Map.from_struct(value), else: value

    Map.new(value, fn {key, item} ->
      {json_key(key), json_safe(item)}
    end)
  end

  defp json_safe(list) when is_list(list), do: Enum.map(list, &json_safe/1)
  defp json_safe(tuple) when is_tuple(tuple), do: tuple |> Tuple.to_list() |> json_safe()
  defp json_safe(value) when is_atom(value) and not is_nil(value) and not is_boolean(value),
    do: Atom.to_string(value)
  defp json_safe(value) when is_binary(value), do: value
  defp json_safe(value) when is_number(value) or is_boolean(value) or is_nil(value),
    do: value
  defp json_safe(value), do: inspect(value)

  defp json_key(key) when is_binary(key), do: key
  defp json_key(key) when is_atom(key), do: Atom.to_string(key)
  defp json_key(key), do: inspect(key)
end
