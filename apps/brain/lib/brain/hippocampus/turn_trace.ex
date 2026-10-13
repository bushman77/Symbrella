defmodule Brain.Hippocampus.TurnTrace do
  @moduledoc """
  Opt-in, process-local observation of hippocampal API operations.

  A chat worker explicitly calls start/0 before cognitive processing and
  finish/0 before sending its reply. The events belong only to that worker;
  no GenServer/global state is used and no neural weights or memory decisions
  are changed. Events are raw observations, not validated training labels.

  This instrumentation observes public API calls made by the worker itself.
  Work delegated to other asynchronous processes requires future propagation.
  """

  @key {__MODULE__, :events}
  @max_events 512

  @doc "Activate an empty trace in the current process."
  @spec start() :: :ok
  def start do
    Process.put(@key, {0, []})
    :ok
  end

  @doc "Return events in observed order and deactivate this process trace."
  @spec finish() :: [map()]
  def finish do
    case Process.delete(@key) do
      {_count, events} -> Enum.reverse(events)
      _ -> []
    end
  end

  @doc "Whether the current process is collecting a turn trace."
  @spec active?() :: boolean()
  def active?, do: match?({_, _}, Process.get(@key))

  @doc "Record a successful episode encoding; does not modify its payload."
  @spec encode(map(), integer()) :: :ok
  def encode(episode, at_ms) do
    if active?() do
      append(%{
        kind: :episodic_encode,
        episode_at_ms: at_ms,
        episode: episode
      })
    end

    :ok
  rescue
    _ -> :ok
  end

  @doc "Record fact lookup success or miss."
  @spec fact(term(), term()) :: :ok
  def fact(key, result) do
    if active?() do
      append(%{
        kind: :fact_lookup,
        memory_key: key,
        found?: not is_nil(result),
        result: result
      })
    end

    :ok
  rescue
    _ -> :ok
  end

  @doc """
  Record the search attempt *including zero-result misses*, then every ranked
  result. Recall IDs distinguish separate queries within the same turn.
  """
  @spec recall(atom(), term(), term(), list()) :: :ok
  def recall(operation, cues, options, results) when is_list(results) do
    if active?() do
      recall_id = Base.encode16(:crypto.strong_rand_bytes(8), case: :lower)
      source = option(options, :source) || option(options, :recall_source) || :memory

      append(%{
        kind: :recall_attempt,
        recall_id: recall_id,
        operation: operation,
        source: source,
        cues: cue_projection(cues),
        limit: option(options, :limit),
        result_count: length(results),
        hit?: results != []
      })

      results
      |> Enum.with_index(1)
      |> Enum.each(fn {result, rank} ->
        if is_map(result) do
          append(%{
            kind: :episodic_recall,
            recall_id: recall_id,
            operation: operation,
            rank: rank,
            score: Map.get(result, :score),
            episode_at_ms: Map.get(result, :at),
            episode: Map.get(result, :episode),
            used_in_response: nil
          })
        end
      end)
    end

    :ok
  rescue
    _ -> :ok
  end

  # Never copy the full SemanticInput into each retrieval event. Its nested
  # lexical/candidate state can be very large and is already in turns.jsonl.
  defp cue_projection(cues) when is_list(cues) do
    cues
    |> Enum.take(48)
    |> Enum.map(&cue_term/1)
  end

  defp cue_projection(%{} = cues) do
    %{
      sentence: Map.get(cues, :sentence) || Map.get(cues, "sentence"),
      winners: cue_projection(List.wrap(Map.get(cues, :winners) || Map.get(cues, "winners"))),
      tokens: cue_projection(List.wrap(Map.get(cues, :tokens) || Map.get(cues, "tokens")))
    }
  end

  defp cue_projection(_), do: []

  defp cue_term(value) when is_binary(value), do: value
  defp cue_term(value) when is_atom(value), do: Atom.to_string(value)
  defp cue_term(%{} = value) do
    Map.get(value, :norm) || Map.get(value, :lemma) || Map.get(value, :phrase) ||
      Map.get(value, :word) || Map.get(value, :id) || Map.get(value, "norm") ||
      Map.get(value, "lemma") || Map.get(value, "phrase") || Map.get(value, "word") ||
      Map.get(value, "id") || inspect(value, limit: 3, printable_limit: 120)
  end

  defp cue_term(value), do: inspect(value, limit: 3, printable_limit: 120)

  defp append(event) do
    case Process.get(@key) do
      {count, events} when count < @max_events ->
        Process.put(@key, {count + 1, [event | events]})

      {@max_events, events} ->
        # Preserve an explicit truncation marker rather than silently
        # presenting a partial retrieval history as a complete one.
        Process.put(@key, {@max_events + 1,
          [%{kind: :trace_truncated, limit: @max_events} | events]})

      _ ->
        :ok
    end

    :ok
  end

  defp option(%{} = opts, key), do: Map.get(opts, key, Map.get(opts, Atom.to_string(key)))
  defp option(opts, key) when is_list(opts), do: Keyword.get(opts, key)
  defp option(_, _), do: nil
end
