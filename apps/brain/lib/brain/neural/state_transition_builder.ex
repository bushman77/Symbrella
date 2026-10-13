defmodule Brain.Neural.StateTransitionBuilder do
  @moduledoc """
  Self-supervised dataset from consecutive, *observed* cognitive states.

  Every pair is two successive completed turns in the same session:
  x(t) -> y(t+1). The next observation is the target; no human rating,
  model output or guessed correctness label is used.

  This v1 feature projection covers selected numeric cognitive state fields
  and aggregate memory events. It is NOT an encoder for all raw NLP content.
  Missing values are tracked where possible and otherwise represented as 0.
  """

  @schema_version 1
  @feature_names [
    :confidence,
    :confidence_present,
    :mood_da,
    :mood_5ht,
    :mood_glu,
    :mood_ne,
    :mood_present,
    :input_token_count,
    :lifg_choices_count,
    :semantic_choices_count,
    :action_candidates_count,
    :episodic_encode_count,
    :episodic_recall_count,
    :recall_attempt_count,
    :recall_hit_fraction,
    :self_monitor_present
  ]
  @max_lines 50_000

  @spec feature_names() :: [atom()]
  def feature_names, do: @feature_names
  def feature_schema_version, do: @schema_version

  @spec build(Path.t(), keyword()) :: {:ok, map()} | {:error, term()}
  def build(dir, opts \\ [])

  def build(dir, opts) when is_binary(dir) and is_list(opts) do
    smoke? = Keyword.get(opts, :smoke, false) == true

    with {:ok, turns} <- read(Path.join(dir, "turns.jsonl")),
         {:ok, events} <- read(Path.join(dir, "memory_events.jsonl"), true),
         :ok <- validate_turns(turns),
         :ok <- check_traces(events),
         {:ok, pairs} <- paired_states(turns, events),
         {:ok, train, evaluation} <- split(pairs, smoke?),
         {:ok, train_batch} <- tensor_batch(train),
         {:ok, eval_batch} <- tensor_batch(evaluation) do
      {:ok,
       %{
         train: train_batch,
         eval: eval_batch,
         metadata: %{
           task: :state_transition,
           feature_schema_version: @schema_version,
           feature_names: @feature_names,
           train_examples: length(train),
           eval_examples: length(evaluation),
           total_pairs: length(pairs),
           input_turns: length(turns),
           input_events: length(events),
           train_sessions: session_names(train),
           eval_sessions: session_names(evaluation),
           split_mode: if(smoke?, do: :in_sample_smoke, else: :session_holdout)
         }
       }}
    end
  end

  def build(_, _), do: {:error, :invalid_arguments}

  defp read(path, optional \\ false) do
    cond do
      optional and not File.exists?(path) ->
        {:ok, []}

      not File.exists?(path) ->
        {:error, {:missing_archive, Path.basename(path)}}

      true ->
        path
        |> File.stream!([], :line)
        |> Stream.with_index(1)
        |> Enum.reduce_while({:ok, []}, fn {line, number}, {:ok, acc} ->
          cond do
            number > @max_lines ->
              {:halt, {:error, {:archive_too_large, Path.basename(path)}}}

            String.trim(line) == "" ->
              {:cont, {:ok, acc}}

            true ->
              case Jason.decode(line) do
                {:ok, %{} = record} -> {:cont, {:ok, [record | acc]}}
                _ -> {:halt, {:error, {:invalid_jsonl_record, Path.basename(path), number}}}
              end
          end
        end)
        |> case do
          {:ok, records} -> {:ok, Enum.reverse(records)}
          error -> error
        end
    end
  rescue
    e in File.Error -> {:error, {:archive_io_error, e.reason}}
  end

  defp validate_turns(turns) do
    ids =
      Enum.map(turns, fn turn -> {turn["session_id"], turn["turn_id"]} end)

    cond do
      Enum.any?(turns, fn turn ->
        turn["record_type"] != "completed_turn" or not is_map(turn["cognition"]) or
          not is_binary(turn["session_id"]) or not is_binary(turn["turn_id"])
      end) ->
        {:error, :invalid_turn_record}

      Enum.uniq(ids) != ids ->
        {:error, :duplicate_turns}

      true ->
        :ok
    end
  end

  defp check_traces(events) do
    if Enum.any?(events, &(&1["kind"] == "trace_truncated")) do
      {:error, :truncated_hippocampal_trace}
    else
      :ok
    end
  end

  defp paired_states(turns, events) do
    stats = memory_stats(events)

    groups =
      turns
      |> Enum.group_by(& &1["session_id"])
      |> Enum.sort_by(&elem(&1, 0))

    pairs =
      Enum.flat_map(groups, fn {session, session_turns} ->
        session_turns
        |> Enum.chunk_every(2, 1, :discard)
        |> Enum.map(fn [current, next] ->
          %{
            session_id: session,
            from_turn_id: current["turn_id"],
            to_turn_id: next["turn_id"],
            x: features(current, stats),
            y: features(next, stats)
          }
        end)
      end)

    if pairs == [], do: {:error, :insufficient_consecutive_turns}, else: {:ok, pairs}
  end

  defp memory_stats(events) do
    events
    |> Enum.group_by(fn ev -> {ev["session_id"], ev["turn_id"]} end)
    |> Map.new(fn {key, rows} ->
      attempt_rows = Enum.filter(rows, &(&1["kind"] == "recall_attempt"))
      {key,
       %{
         encode: Enum.count(rows, &(&1["kind"] == "episodic_encode")),
         recall: Enum.count(rows, &(&1["kind"] == "episodic_recall")),
         attempts: length(attempt_rows),
         hits: Enum.count(attempt_rows, &(&1["hit"] == true))
       }}
    end)
  end

  defp features(turn, stats) do
    cognitive = map(turn["cognition"])
    mood = map(cognitive["mood"])
    values = map(mood["levels"])
    values = if map_size(values) == 0, do: mood, else: values

    confidence = cognitive["confidence"]
    memory =
      Map.get(stats, {turn["session_id"], turn["turn_id"]},
        %{encode: 0, recall: 0, attempts: 0, hits: 0}
      )

    tokens =
      case cognitive["tokens"] do
        list when is_list(list) -> length(list)
        _ -> String.split(turn["user_text"] || "", ~r/\s+/, trim: true) |> length()
      end

    [
      unit(confidence),
      bool(is_number(confidence)),
      mood_unit(values, ["da", "dopamine"]),
      mood_unit(values, ["5ht", "serotonin", "ht"]),
      mood_unit(values, ["glu", "glutamate"]),
      mood_unit(values, ["ne", "norepinephrine"]),
      bool(map_size(values) > 0),
      count(tokens, 64),
      count(size(cognitive["lifg_choices"]), 16),
      count(size(cognitive["semantic_choices"]), 16),
      count(size(cognitive["action_candidates"]), 16),
      count(memory.encode, 16),
      count(memory.recall, 16),
      count(memory.attempts, 16),
      if(memory.attempts == 0, do: 0.0, else: memory.hits / memory.attempts),
      bool(present?(cognitive["self_monitor"]))
    ]
  end

  defp split(pairs, true), do: {:ok, pairs, pairs}

  defp split(pairs, false) do
    groups = pairs |> Enum.group_by(& &1.session_id) |> Enum.sort_by(&elem(&1, 0))

    if length(groups) < 2 do
      {:error, :insufficient_independent_sessions}
    else
      evaluation_session = groups |> List.last() |> elem(0)
      train = Enum.reject(pairs, &(&1.session_id == evaluation_session))
      evaluation = Enum.filter(pairs, &(&1.session_id == evaluation_session))
      {:ok, train, evaluation}
    end
  end

  defp tensor_batch(pairs) when pairs != [] do
    {:ok,
     %{
       x: Nx.tensor(Enum.map(pairs, & &1.x), type: :f32),
       y: Nx.tensor(Enum.map(pairs, & &1.y), type: :f32),
       feature_names: @feature_names,
       sample_refs: Enum.map(pairs, &%{session_id: &1.session_id, from: &1.from_turn_id, to: &1.to_turn_id})
     }}
  end

  defp tensor_batch(_), do: {:error, :empty_transition_batch}

  defp session_names(rows), do: rows |> Enum.map(& &1.session_id) |> Enum.uniq() |> Enum.sort()
  defp mood_unit(mood, names), do: names |> Enum.find_value(0.0, &Map.get(mood, &1)) |> unit()
  defp unit(value) when is_number(value), do: max(0.0, min(value * 1.0, 1.0))
  defp unit(_), do: 0.0
  defp count(value, max_value), do: min(value, max_value) / max_value
  defp bool(true), do: 1.0
  defp bool(false), do: 0.0
  defp size(value) when is_list(value), do: length(value)
  defp size(value) when is_map(value), do: map_size(value)
  defp size(_), do: 0
  defp map(%{} = value), do: value
  defp map(_), do: %{}
  defp present?(nil), do: false
  defp present?(%{} = value) when map_size(value) == 0, do: false
  defp present?([]), do: false
  defp present?(_), do: true
end
