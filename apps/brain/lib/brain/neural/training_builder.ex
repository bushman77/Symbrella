defmodule Brain.Neural.TrainingBuilder do
  @moduledoc """
  Read-only bridge from Symbrella's private JSONL experience archive to Nx.

  Joins completed turns, hippocampal events and explicitly human-reviewed
  feedback. No unreviewed response, retrieved episode or LIFG decision is
  treated as a target. No model training or runtime modification occurs here.

  Supported independent tasks:
    * :memory_relevance — human confirms whether a particular recalled
      episode was relevant (label: %{kind: "memory_relevance",
      episode_fingerprint: "...", relevant: true | false}).
    * :uncertainty — human-provided 0..1 target for cognitive uncertainty
      (label: %{kind: "uncertainty", value: number}).

  Splits are deterministic and grouped by session_id. If fewer than two
  independent sessions have reviewed examples, the builder fails closed.
  """

  @schema_version 1
  @feature_schema_version 1
  @max_lines_per_file 50_000
  @tasks [:memory_relevance, :uncertainty]
  @feature_names [
    :intent_confidence,
    :intent_confidence_present,
    :mood_da,
    :mood_5ht,
    :mood_glu,
    :mood_ne,
    :lifg_choices_count,
    :input_token_count,
    :recall_score,
    :recall_rank,
    :episodic_recall_count,
    :episodic_recall_present
  ]

  @type task :: :memory_relevance | :uncertainty

  @doc "Stable feature order for both first-generation neural training targets."
  def feature_names, do: @feature_names

  @doc "Version of the input feature schema used by produced Nx tensors."
  def feature_schema_version, do: @feature_schema_version

  @doc """
  Construct independent training and evaluation tensors from an archive folder.

  Returns {:ok, %{train: batch, eval: batch, metadata: map}}.
  A batch includes :x, :y, :feature_names, :label_names and :sample_refs.

  Never synthesizes labels. Empty data, malformed records, missing feedback,
  conflicting labels, orphan IDs and unsplittable data return tagged errors.
  The optional :eval_fraction defaults to 0.2.
  """
  @spec build(Path.t(), task(), keyword()) :: {:ok, map()} | {:error, term()}
  def build(dir, task, opts \\ [])

  def build(dir, task, opts)
      when is_binary(dir) and task in @tasks and is_list(opts) do
    with {:ok, eval_fraction} <- eval_fraction(opts),
         {:ok, turns} <- read_jsonl(Path.join(dir, "turns.jsonl")),
         {:ok, events} <- read_jsonl(Path.join(dir, "memory_events.jsonl"), optional: true),
         {:ok, feedback} <- read_jsonl(Path.join(dir, "feedback.jsonl")),
         {:ok, turn_index} <- index_turns(turns),
         {:ok, examples} <- reviewed_examples(task, feedback, turn_index, events),
         {:ok, training, evaluation} <- split(examples, eval_fraction),
         :ok <- prevent_episode_leakage(training, evaluation),
         {:ok, train_batch} <- tensor_batch(training, task),
         {:ok, eval_batch} <- tensor_batch(evaluation, task) do
      {:ok,
       %{
         train: train_batch,
         eval: eval_batch,
         metadata: %{
           schema_version: @schema_version,
           feature_schema_version: @feature_schema_version,
           task: task,
           feature_names: @feature_names,
           label_names: label_names(task),
           reviewed_examples: length(examples),
           train_examples: length(training),
           eval_examples: length(evaluation),
           train_sessions: training |> Enum.map(& &1.session_id) |> Enum.uniq() |> Enum.sort(),
           eval_sessions: evaluation |> Enum.map(& &1.session_id) |> Enum.uniq() |> Enum.sort()
         }
       }}
    end
  end

  def build(_dir, _task, _opts), do: {:error, :invalid_builder_arguments}

  defp eval_fraction(opts) do
    value = Keyword.get(opts, :eval_fraction, 0.2)

    if is_number(value) and value > 0 and value < 1 do
      {:ok, value}
    else
      {:error, :invalid_eval_fraction}
    end
  end

  defp read_jsonl(path, opts \\ []) do
    if Keyword.get(opts, :optional, false) and not File.exists?(path) do
      {:ok, []}
    else
      if File.exists?(path) do
        path
        |> File.stream!([], :line)
        |> Stream.with_index(1)
        |> Enum.reduce_while({:ok, []}, fn {line, number}, {:ok, acc} ->
          cond do
            number > @max_lines_per_file ->
              {:halt, {:error, {:archive_too_large, Path.basename(path)}}}

            String.trim(line) == "" ->
              {:cont, {:ok, acc}}

            true ->
              case Jason.decode(line) do
                {:ok, %{} = record} ->
                  {:cont, {:ok, [record | acc]}}

                _ ->
                  {:halt, {:error, {:invalid_jsonl_record, Path.basename(path), number}}}
              end
          end
        end)
        |> case do
          {:ok, records} -> {:ok, Enum.reverse(records)}
          error -> error
        end
      else
        {:error, {:missing_archive, Path.basename(path)}}
      end
    end
  rescue
    error in File.Error -> {:error, {:archive_io_error, error.reason}}
  end

  defp index_turns(turns) do
    Enum.reduce_while(turns, {:ok, %{}}, fn turn, {:ok, index} ->
      key = record_key(turn)

      cond do
        turn["schema_version"] != @schema_version or
            turn["record_type"] != "completed_turn" or
            not valid_key?(key) or
            not is_binary(turn["user_text"]) or
            not is_map(turn["cognition"]) ->
          {:halt, {:error, :invalid_turn_record}}

        Map.has_key?(index, key) ->
          {:halt, {:error, {:duplicate_turn, key}}}

        true ->
          {:cont, {:ok, Map.put(index, key, turn)}}
      end
    end)
  end

  defp reviewed_examples(task, feedback, turns, events) do
    selected =
      Enum.filter(feedback, fn item ->
        label = item["label"]
        is_map(label) and label["kind"] == Atom.to_string(task)
      end)

    if selected == [] do
      {:error, :no_reviewed_examples}
    else
      episodic_index = index_episodic_events(events)
      events_by_turn =
        events
        |> Enum.filter(fn event ->
          event["record_type"] == "memory_event" and
            event["schema_version"] == @schema_version and
            event["kind"] == "episodic_recall" and
            valid_key?(record_key(event))
        end)
        |> Enum.group_by(&record_key/1)

      result =
        Enum.reduce_while(selected, {:ok, [], MapSet.new()}, fn feedback_row,
                                                                {:ok, acc, seen} ->
          case example_from_feedback(task, feedback_row, turns, episodic_index, events_by_turn) do
            {:ok, example} ->
              if MapSet.member?(seen, example.label_key) do
                {:halt, {:error, {:duplicate_review, example.label_key}}}
              else
                {:cont,
                 {:ok, [example | acc], MapSet.put(seen, example.label_key)}}
              end

            {:error, reason} ->
              {:halt, {:error, reason}}
          end
        end)

      case result do
        {:ok, examples, _seen} -> {:ok, Enum.reverse(examples)}
        error -> error
      end
    end
  end

  defp example_from_feedback(task, feedback, turns, episodes, events_by_turn) do
    key = record_key(feedback)
    label = feedback["label"]

    with :ok <- validate_feedback(feedback),
         {:ok, turn} <- fetch_turn(turns, key),
         {:ok, event, target, label_key} <- training_target(task, label, key, episodes),
         {:ok, features} <- extract_features(turn, event, Map.get(events_by_turn, key, [])) do
      {:ok,
       %{
         session_id: elem(key, 0),
         turn_id: elem(key, 1),
         episode_fingerprint: if(event, do: event["episode_fingerprint"], else: nil),
         label_key: label_key,
         x: features,
         y: [target]
       }}
    end
  end

  defp validate_feedback(feedback) do
    key = record_key(feedback)

    if feedback["schema_version"] == @schema_version and
         feedback["record_type"] == "human_feedback" and
         feedback["source"] == "human_review" and
         nonblank?(feedback["reviewed_by"]) and valid_key?(key) do
      :ok
    else
      {:error, :unverified_feedback}
    end
  end

  defp fetch_turn(index, key) do
    case Map.fetch(index, key) do
      {:ok, turn} -> {:ok, turn}
      :error -> {:error, {:orphan_feedback, key}}
    end
  end

  defp training_target(:uncertainty, %{"value" => value}, key, _episodes) do
    if unit_number?(value) do
      {:ok, nil, value * 1.0, {:uncertainty, key}}
    else
      {:error, :invalid_uncertainty_label}
    end
  end

  defp training_target(:memory_relevance, label, key, episodes) do
    fingerprint = label["episode_fingerprint"]
    relevant = label["relevant"]

    if fingerprint?(fingerprint) and is_boolean(relevant) do
      case Map.fetch(episodes, {key, fingerprint}) do
        {:ok, event} ->
          if unit_number?(event["score"]) and
               is_integer(event["rank"]) and event["rank"] > 0 do
            {:ok, event, if(relevant, do: 1.0, else: 0.0),
             {:memory_relevance, key, fingerprint}}
          else
            {:error, :invalid_recall_evidence}
          end

        :error ->
          {:error, {:unknown_recalled_episode, key, fingerprint}}
      end
    else
      {:error, :invalid_memory_relevance_label}
    end
  end

  defp training_target(_task, _label, _key, _episodes), do: {:error, :invalid_training_label}

  defp index_episodic_events(events) do
    events
    |> Enum.filter(fn event ->
      event["schema_version"] == @schema_version and
        event["record_type"] == "memory_event" and
        event["kind"] == "episodic_recall" and
        valid_key?(record_key(event)) and
        fingerprint?(event["episode_fingerprint"])
    end)
    |> Enum.reduce(%{}, fn event, acc ->
      key = {record_key(event), event["episode_fingerprint"]}

      Map.update(acc, key, event, fn previous ->
        if rank(event) < rank(previous), do: event, else: previous
      end)
    end)
  end

  defp extract_features(turn, candidate, recalled) do
    cognition = turn["cognition"]
    mood = as_map(cognition["mood"])
    selected = candidate || top_ranked(recalled)
    confidence = cognition["confidence"]

    row = [
      unit_or_zero(confidence),
      if(unit_number?(confidence), do: 1.0, else: 0.0),
      unit_or_zero(mood["da"] || mood["dopamine"]),
      unit_or_zero(mood["5ht"] || mood["serotonin"]),
      unit_or_zero(mood["glu"] || mood["glutamate"]),
      unit_or_zero(mood["ne"] || mood["norepinephrine"]),
      normalized_count(cognition["lifg_choices"], 16),
      input_tokens(turn["user_text"]),
      unit_or_zero(if(selected, do: selected["score"], else: nil)),
      if(selected, do: normalized_rank(selected["rank"]), else: 0.0),
      min(length(recalled), 16) / 16.0,
      if(recalled == [], do: 0.0, else: 1.0)
    ]

    if length(row) == length(@feature_names) do
      {:ok, row}
    else
      {:error, :feature_schema_mismatch}
    end
  end

  defp top_ranked([]), do: nil
  defp top_ranked(events), do: Enum.min_by(events, &rank/1)

  defp rank(event) do
    case event["rank"] do
      n when is_integer(n) and n > 0 -> n
      _ -> 1_000_000
    end
  end

  defp normalized_rank(n) when is_integer(n) and n > 0,
    do: 1.0 / n

  defp normalized_rank(_), do: 0.0

  defp normalized_count(value, maximum) when is_list(value),
    do: min(length(value), maximum) / maximum

  defp normalized_count(_value, _maximum), do: 0.0

  defp input_tokens(text) when is_binary(text) do
    count = text |> String.split(~r/\s+/u, trim: true) |> length()
    min(count, 64) / 64.0
  end

  defp input_tokens(_), do: 0.0

  defp unit_or_zero(value) do
    if unit_number?(value), do: value * 1.0, else: 0.0
  end

  defp unit_number?(value), do: is_number(value) and value >= 0 and value <= 1

  defp record_key(%{} = map), do: {map["session_id"], map["turn_id"]}
  defp record_key(_), do: {nil, nil}

  defp valid_key?({session, turn}), do: nonblank?(session) and nonblank?(turn)
  defp nonblank?(value), do: is_binary(value) and String.trim(value) != ""

  defp fingerprint?(value) when is_binary(value) and byte_size(value) == 64 do
    String.match?(value, ~r/\A[0-9a-f]{64}\z/)
  end

  defp fingerprint?(_), do: false
  defp as_map(%{} = value), do: value
  defp as_map(_), do: %{}

  defp split(examples, fraction) do
    sessions =
      examples
      |> Enum.map(& &1.session_id)
      |> Enum.uniq()
      |> Enum.sort_by(fn session ->
        :crypto.hash(:sha256, session) |> Base.encode16(case: :lower)
      end)

    case length(sessions) do
      n when n < 2 ->
        {:error, :insufficient_independent_sessions}

      n ->
        eval_count = max(1, min(n - 1, ceil(n * fraction)))
        {train_sessions, eval_sessions} = Enum.split(sessions, n - eval_count)
        eval_set = MapSet.new(eval_sessions)
        train_set = MapSet.new(train_sessions)

        training = Enum.filter(examples, &MapSet.member?(train_set, &1.session_id))
        evaluation = Enum.filter(examples, &MapSet.member?(eval_set, &1.session_id))
        {:ok, training, evaluation}
    end
  end

  defp prevent_episode_leakage(train, eval) do
    fingerprints = fn rows ->
      rows
      |> Enum.map(& &1.episode_fingerprint)
      |> Enum.reject(&is_nil/1)
      |> MapSet.new()
    end

    if MapSet.disjoint?(fingerprints.(train), fingerprints.(eval)) do
      :ok
    else
      {:error, :cross_split_episode_leakage}
    end
  end

  defp tensor_batch(examples, task) do
    if Code.ensure_loaded?(Nx) and function_exported?(Nx, :tensor, 2) do
      {:ok,
       %{
         x: Nx.tensor(Enum.map(examples, & &1.x), type: :f32),
         y: Nx.tensor(Enum.map(examples, & &1.y), type: :f32),
         feature_names: @feature_names,
         label_names: label_names(task),
         feature_schema_version: @feature_schema_version,
         sample_refs:
           Enum.map(examples, fn example ->
             %{
               session_id: example.session_id,
               turn_id: example.turn_id,
               episode_fingerprint: example.episode_fingerprint
             }
           end)
       }}
    else
      {:error, :nx_unavailable}
    end
  end

  defp label_names(:memory_relevance), do: [:relevant]
  defp label_names(:uncertainty), do: [:uncertainty]
end
