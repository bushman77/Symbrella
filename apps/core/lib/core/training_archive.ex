defmodule Core.TrainingArchive do
  @moduledoc """
  Opt-in, append-only JSONL observations for future neural training.

  This archive records *observations*, not verified training labels. It
  captures completed chat turns together with hippocampal recall evidence
  and fact-memory operations. It never changes retrieval or response decisions.

  Files are local/private by default and are intentionally stored under tmp/.
  The raw Core.StateDump remains a separate diagnostic facility.
  """

  @default_dir "tmp/training"
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
        :ok
      end
    else
      :disabled
    end
  rescue
    _error -> {:error, :archive_exception}
  end

  def record_turn(_, _), do: {:error, :invalid_turn}

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
      training: %{review_status: "unreviewed", label: nil}
    }

    events =
      episodic_events(si, session_id, turn_id) ++
        fact_memory_events(meta, session_id, turn_id)

    {:ok, turn, events}
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
