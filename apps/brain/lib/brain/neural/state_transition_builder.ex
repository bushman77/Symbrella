defmodule Brain.Neural.StateTransitionBuilder do
  @moduledoc """
  Self-supervised dataset from consecutive, *observed* cognitive states.

  Every pair is two successive completed turns in the same session:
  x(t) -> y(t+1). The next observation is the target; no human rating,
  model output or guessed correctness label is used.

  Feature schema v3 differentiates the *raw neuromodulator levels* stored at
  cognition.self_model.mood.levels from derived mood indices at cognition.mood.
  Every raw level has its own presence flag. Missing readings are represented
  by 0 only alongside a false presence flag, and are EXCLUDED from the loss
  and evaluation with a mask. No missing readings are invented. Only features
  observed at BOTH ends of a transition are eligible training targets.
  Flags are input-only, never targets.

  This projection covers selected numeric fields and aggregate memory events.
  It is NOT an encoder for all raw NLP content.
  """

  @schema_version 3
  @feature_names [
    :confidence,
    :confidence_present,
    :mood_da,
    :mood_da_present,
    :mood_5ht,
    :mood_5ht_present,
    :mood_glu,
    :mood_glu_present,
    :mood_ne,
    :mood_ne_present,
    :mood_exploration,
    :mood_inhibition,
    :mood_vigilance,
    :mood_plasticity,
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
           train_valid_cells: count_valid_cells(train_batch),
           eval_valid_cells: count_valid_cells(eval_batch),
           train_possible_cells: length(train) * length(@feature_names),
           eval_possible_cells: length(evaluation) * length(@feature_names),
           mask_policy: :both_endpoints_observed_exclude_presence_indicators,
           complete_modulator_turns: Enum.count(turns, &complete_modulators?/1),
           missing_modulator_turns: Enum.count(turns, &(not complete_modulators?(&1))),
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
          first_mask = observation_mask(current, stats)
          next_mask = observation_mask(next, stats)

          %{
            session_id: session,
            from_turn_id: current["turn_id"],
            to_turn_id: next["turn_id"],
            x: features(current, stats),
            y: features(next, stats),
            mask: Enum.zip_with(first_mask, next_mask, fn a, b -> bool(a == 1.0 and b == 1.0) end)
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
    # The SI mood field is derived (exploration/inhibition/vigilance/plasticity).
    # Raw DA/5HT/Glu/NE are nested in the self-model's MoodCore snapshot.
    derived_mood = map(cognitive["mood"])
    raw_levels = raw_modulators(cognitive)

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
      mood_unit(raw_levels, ["da", "dopamine"]),
      bool(has_mood_number?(raw_levels, ["da", "dopamine"])),
      mood_unit(raw_levels, ["5ht", "serotonin", "ht"]),
      bool(has_mood_number?(raw_levels, ["5ht", "serotonin", "ht"])),
      mood_unit(raw_levels, ["glu", "glutamate"]),
      bool(has_mood_number?(raw_levels, ["glu", "glutamate"])),
      mood_unit(raw_levels, ["ne", "norepinephrine"]),
      bool(has_mood_number?(raw_levels, ["ne", "norepinephrine"])),
      mood_unit(derived_mood, ["exploration"]),
      mood_unit(derived_mood, ["inhibition"]),
      mood_unit(derived_mood, ["vigilance"]),
      mood_unit(derived_mood, ["plasticity"]),
      bool(map_size(derived_mood) > 0),
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
       mask: Nx.tensor(Enum.map(pairs, & &1.mask), type: :f32),
       feature_names: @feature_names,
       sample_refs: Enum.map(pairs, &%{session_id: &1.session_id, from: &1.from_turn_id, to: &1.to_turn_id})
     }}
  end

  defp tensor_batch(_), do: {:error, :empty_transition_batch}

  defp session_names(rows), do: rows |> Enum.map(& &1.session_id) |> Enum.uniq() |> Enum.sort()
  defp count_valid_cells(batch), do: batch.mask |> Nx.sum() |> Nx.to_number() |> round()

  # Only actual measurements are prediction targets. Availability flags
  # tell Axon what was observed but are never targets themselves.
  # A memory event absent from the archive is UNKNOWN, not a verified zero.
  defp observation_mask(turn, stats) do
    cognition = map(turn["cognition"])
    raw = raw_modulators(cognition)
    derived = map(cognition["mood"])
    mem = Map.get(stats, {turn["session_id"], turn["turn_id"]})
    event_known? = is_map(mem)
    attempts? = event_known? and mem.attempts > 0

    [
      observed_number?(cognition, "confidence"),
      false,
      has_mood_number?(raw, ["da", "dopamine"]),
      false,
      has_mood_number?(raw, ["5ht", "serotonin", "ht"]),
      false,
      has_mood_number?(raw, ["glu", "glutamate"]),
      false,
      has_mood_number?(raw, ["ne", "norepinephrine"]),
      false,
      has_mood_number?(derived, ["exploration"]),
      has_mood_number?(derived, ["inhibition"]),
      has_mood_number?(derived, ["vigilance"]),
      has_mood_number?(derived, ["plasticity"]),
      false,
      is_list(cognition["tokens"]) or is_binary(turn["user_text"]),
      observed_collection?(cognition["lifg_choices"]),
      observed_collection?(cognition["semantic_choices"]),
      observed_collection?(cognition["action_candidates"]),
      event_known?,
      event_known?,
      event_known?,
      attempts?,
      false
    ]
    |> Enum.map(&bool/1)
  end

  defp observed_number?(map, key), do: is_number(Map.get(map, key))
  defp observed_collection?(v), do: is_list(v) or is_map(v)


  defp raw_modulators(cognitive) do
    cognitive
    |> Map.get("self_model")
    |> map()
    |> Map.get("mood")
    |> map()
    |> Map.get("levels")
    |> map()
  end

  defp complete_modulators?(turn) do
    raw = turn |> Map.get("cognition") |> map() |> raw_modulators()
    Enum.all?(
      [["da", "dopamine"], ["5ht", "serotonin", "ht"], ["glu", "glutamate"], ["ne", "norepinephrine"]],
      &has_mood_number?(raw, &1)
    )
  end

  defp mood_value(mood, names) do
    Enum.find_value(names, fn name ->
      case Map.get(mood, name) do
        v when is_number(v) -> v
        _ -> nil
      end
    end)
  end

  defp mood_unit(mood, names), do: mood |> mood_value(names) |> unit()
  defp has_mood_number?(mood, names), do: is_number(mood_value(mood, names))
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
