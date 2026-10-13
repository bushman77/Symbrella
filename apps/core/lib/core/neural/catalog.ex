defmodule Core.Neural.Catalog do
  @moduledoc """
  Deterministic, offline categorization of the full recorded cognitive archive.

  This is an *index*, not a set of synthetic ground-truth labels. Every
  completed turn, hippocampal event and human feedback record receives a
  provenance-bearing category and pointer back to its original JSONL row.

  Original JSONL records are never modified. The catalog does not contain
  conversation text, episode payloads or presumed correctness labels.
  """

  @version 1
  @max_lines 50_000
  @inputs [
    {"turns.jsonl", "completed_turn", false},
    {"memory_events.jsonl", "memory_event", true},
    {"feedback.jsonl", "human_feedback", true}
  ]

  @signal_fields %{
    "language" => ["intent", "confidence", "conversation_act", "context_intent"],
    "syntax" => ["tokens", "sentence_signatures", "phrase_candidates"],
    "semantics" => ["sense_candidates", "semantic_choices", "lifg_choices", "comprehension"],
    "affect" => ["mood", "appraisal"],
    "executive" => ["selected_action", "action_meta", "action_candidates", "symbolic_frame"],
    "self_model" => ["self_model", "self_monitor", "self_memory_recall"],
    "trace" => ["trace"]
  }

  @doc """
  Build an index in memory for the entire archive directory.
  Categories are derived only from explicit source fields.
  """
  @spec build(Path.t()) :: {:ok, map()} | {:error, term()}
  def build(dir) when is_binary(dir) do
    with {:ok, parts} <- load_all(dir) do
      rows = Enum.flat_map(parts, fn {_file, records} -> records end)
      categories = counts(rows, "category")
      subtypes = counts(rows, "subtype")
      signals =
        rows
        |> Enum.flat_map(& &1.signal_groups)
        |> Enum.frequencies()
        |> Enum.sort_by(&elem(&1, 0))
        |> Map.new()

      {:ok,
       %{
         schema_version: @version,
         rows: rows,
         summary: %{
           total_records: length(rows),
           categories: categories,
           subtypes: subtypes,
           signal_groups: signals,
           source_files: Map.new(parts, fn {file, records} -> {file, length(records)} end)
         }
       }}
    end
  end

  def build(_), do: {:error, :invalid_directory}

  @doc """
  Atomically regenerate a private JSONL index. The raw archive remains
  authoritative; the catalog can be recreated at any time.
  """
  @spec write(Path.t(), Path.t()) :: {:ok, map()} | {:error, term()}
  def write(dir, output \\ nil)

  def write(dir, output) when is_binary(dir) and (is_nil(output) or is_binary(output)) do
    output = output || Path.join(dir, "catalog.jsonl")
    output = Path.expand(output)
    inputs = Enum.map(@inputs, fn {file, _, _} -> Path.expand(Path.join(dir, file)) end)

    cond do
      output in inputs ->
        {:error, :output_overwrites_raw_archive}

      true ->
        with {:ok, catalog} <- build(dir),
             :ok <- File.mkdir_p(Path.dirname(output)),
             :ok <- write_atomic(output, catalog.rows) do
          {:ok, %{output: output, summary: catalog.summary}}
        end
    end
  end

  def write(_dir, _output), do: {:error, :invalid_arguments}

  defp load_all(dir) do
    Enum.reduce_while(@inputs, {:ok, []}, fn {filename, expected, optional}, {:ok, acc} ->
      case parse_file(Path.join(dir, filename), filename, expected, optional) do
        {:ok, records} -> {:cont, {:ok, [{filename, records} | acc]}}
        {:error, _} = error -> {:halt, error}
      end
    end)
    |> case do
      {:ok, parts} -> {:ok, Enum.reverse(parts)}
      error -> error
    end
  end

  defp parse_file(path, filename, expected_type, optional) do
    cond do
      not File.exists?(path) and optional ->
        {:ok, []}

      not File.exists?(path) ->
        {:error, {:missing_archive, filename}}

      true ->
        path
        |> File.stream!([], :line)
        |> Stream.with_index(1)
        |> Enum.reduce_while({:ok, []}, fn {line, line_no}, {:ok, acc} ->
          cond do
            line_no > @max_lines ->
              {:halt, {:error, {:archive_too_large, filename}}}

            String.trim(line) == "" ->
              {:cont, {:ok, acc}}

            true ->
              case Jason.decode(line) do
                {:ok, %{} = source} ->
                  case classify(source, filename, line_no, expected_type) do
                    {:ok, row} -> {:cont, {:ok, [row | acc]}}
                    {:error, reason} -> {:halt, {:error, {reason, filename, line_no}}}
                  end

                _ ->
                  {:halt, {:error, {:malformed_jsonl, filename, line_no}}}
              end
          end
        end)
        |> case do
          {:ok, rows} -> {:ok, Enum.reverse(rows)}
          error -> error
        end
    end
  rescue
    error in File.Error -> {:error, {:archive_io_error, filename, error.reason}}
  end

  defp classify(%{"record_type" => type} = record, file, line, type) do
    with :ok <- validate_identity(record) do
      {category, subtype, groups, observations, provenance} =
        case type do
          "completed_turn" -> classify_turn(record)
          "memory_event" -> classify_event(record)
          "human_feedback" -> classify_feedback(record)
        end

      {:ok,
       %{
         schema_version: @version,
         source_file: file,
         source_line: line,
         record_type: type,
         session_id: record["session_id"],
         turn_id: record["turn_id"],
         category: category,
         subtype: subtype,
         signal_groups: groups,
         observed_fields: observations,
         label_provenance: provenance
       }}
    end
  end

  defp classify(_, _, _, _), do: {:error, :unexpected_record_type}

  defp validate_identity(record) do
    if valid_id?(record["turn_id"]) and valid_id?(record["session_id"]) do
      :ok
    else
      {:error, :missing_turn_identity}
    end
  end

  defp valid_id?(value), do: is_binary(value) and byte_size(String.trim(value)) > 0

  defp classify_turn(record) do
    cognition = map(record["cognition"])
    response = map(record["response_meta"])
    groups =
      @signal_fields
      |> Enum.filter(fn {_name, fields} ->
        Enum.any?(fields, fn field -> present?(Map.get(cognition, field)) end)
      end)
      |> Enum.map(&elem(&1, 0))
      |> Enum.sort()

    observed =
      %{
        "intent" => cognition["intent"],
        "conversation_act" => cognition["conversation_act"],
        "context_intent" => cognition["context_intent"],
        "selected_action" => cognition["selected_action"],
        "response_source" => response["response_source"],
        "confidence" => if(is_number(cognition["confidence"]), do: cognition["confidence"], else: nil)
      }
      |> Enum.filter(fn {_key, value} -> scalar?(value) and not is_nil(value) end)
      |> Map.new()

    {"cognitive_turn", safe_tag(cognition["intent"], "unclassified"), groups,
     observed, "runtime_observation"}
  end

  defp classify_event(record) do
    kind = safe_tag(record["kind"], "unclassified")

    {category, group} =
      case kind do
        "episodic_encode" -> {"memory_encoding", "hippocampus"}
        "episodic_recall" -> {"memory_retrieval", "hippocampus"}
        "recall_attempt" -> {"memory_retrieval", "hippocampus"}
        "fact_lookup" -> {"fact_memory", "hippocampus"}
        "fact_recall" -> {"fact_memory", "hippocampus"}
        "fact_write" -> {"fact_memory", "hippocampus"}
        "trace_truncated" -> {"trace_integrity", "trace"}
        _ -> {"other_memory_event", "hippocampus"}
      end

    observed =
      %{
        "operation" => record["operation"],
        "hit" => record["hit"],
        "result_count" => record["result_count"],
        "cue_count" => record["cue_count"],
        "rank" => record["rank"],
        "score" => record["score"],
        "episode_fingerprint" => record["episode_fingerprint"]
      }
      |> Enum.filter(fn {_key, value} -> scalar?(value) and not is_nil(value) end)
      |> Map.new()

    {category, kind, [group], observed, "runtime_observation"}
  end

  defp classify_feedback(record) do
    label = map(record["label"])

    observed =
      %{
        "label_kind" => label["kind"],
        "correction" => record["correction"] == true
      }
      |> Enum.filter(fn {_key, value} -> scalar?(value) and not is_nil(value) end)
      |> Map.new()

    provenance = if record["source"] == "human_review", do: "human_review", else: "unverified"
    {"human_feedback", safe_tag(label["kind"], "unclassified"), ["feedback"], observed, provenance}
  end

  defp safe_tag(value, _fallback) when is_binary(value) and byte_size(value) <= 128, do: value
  defp safe_tag(_, fallback), do: fallback

  defp map(%{} = value), do: value
  defp map(_), do: %{}

  defp present?(nil), do: false
  defp present?([]), do: false
  defp present?(%{} = value) when map_size(value) == 0, do: false
  defp present?(_), do: true

  defp scalar?(value), do: is_binary(value) or is_boolean(value) or is_number(value)

  defp counts(rows, "category"), do: frequency_of(rows, :category)
  defp counts(rows, "subtype"), do: frequency_of(rows, :subtype)

  defp frequency_of(rows, key) do
    rows
    |> Enum.frequencies_by(&Map.fetch!(&1, key))
    |> Enum.sort_by(&elem(&1, 0))
    |> Map.new()
  end

  defp write_atomic(path, records) do
    temp = path <> ".tmp-" <> Integer.to_string(System.unique_integer([:positive]))

    try do
      result =
        with :ok <- File.write(temp, "", [:binary]),
             :ok <- File.chmod(temp, 0o600),
             :ok <- append_rows(temp, records),
             :ok <- File.rename(temp, path) do
          :ok
        end

      result
    after
      File.rm(temp)
    end
  end

  defp append_rows(path, records) do
    Enum.reduce_while(records, :ok, fn record, :ok ->
      case Jason.encode(record) do
        {:ok, json} ->
          case File.write(path, json <> "\n", [:append, :binary]) do
            :ok -> {:cont, :ok}
            {:error, reason} -> {:halt, {:error, {:catalog_write_failed, reason}}}
          end

        {:error, reason} ->
          {:halt, {:error, {:catalog_encode_failed, reason}}}
      end
    end)
  end
end
