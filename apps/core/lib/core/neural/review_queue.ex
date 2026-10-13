defmodule Core.Neural.ReviewQueue do
  @moduledoc """
  Offline, read-only selection of retrieved hippocampal episodes awaiting
  explicit human review. Submitting a decision delegates to TrainingArchive.

  No response- or retrieval-derived label is inferred. Only actual
  :episodic_recall records from ordinary :attach searches are offered.
  """

  alias Core.TrainingArchive

  @max_lines 50_000

  @spec pending(Path.t()) :: {:ok, [map()]} | {:error, term()}
  def pending(dir) when is_binary(dir) do
    with {:ok, turns} <- read_jsonl(Path.join(dir, "turns.jsonl")),
         {:ok, events} <- read_jsonl(Path.join(dir, "memory_events.jsonl")),
         {:ok, feedback} <- read_jsonl(Path.join(dir, "feedback.jsonl"), true),
         :ok <- reject_truncation(events) do
      turn_index =
        Map.new(turns, fn turn ->
          {{turn["session_id"], turn["turn_id"]}, turn}
        end)

      reviewed =
        feedback
        |> Enum.filter(&get_in(&1, ["label", "kind"]) == "memory_relevance")
        |> MapSet.new(fn record ->
          {record["session_id"], record["turn_id"], get_in(record, ["label", "episode_fingerprint"])}
        end)

      examples =
        events
        |> Enum.filter(fn event ->
          event["kind"] == "episodic_recall" and event["operation"] == "attach" and
            is_binary(event["episode_fingerprint"]) and
            is_number(event["score"]) and
            is_map(event["episode"])
        end)
        |> Enum.reduce({[], MapSet.new()}, fn event, {acc, seen} ->
          key = {event["session_id"], event["turn_id"], event["episode_fingerprint"]}
          turn = Map.get(turn_index, {event["session_id"], event["turn_id"]})

          if turn && is_binary(turn["user_text"]) and
               not MapSet.member?(reviewed, key) and not MapSet.member?(seen, key) do
            row = %{
              session_id: event["session_id"],
              turn_id: event["turn_id"],
              episode_fingerprint: event["episode_fingerprint"],
              score: event["score"],
              user_text: turn["user_text"],
              assistant_text: turn["assistant_text"],
              memory_summary: summary(event["episode"])
            }

            {[row | acc], MapSet.put(seen, key)}
          else
            {acc, seen}
          end
        end)
        |> elem(0)
        |> Enum.reverse()

      {:ok, examples}
    end
  end

  def pending(_), do: {:error, :invalid_directory}

  @doc "Validate the still-pending candidate and append a human review."
  @spec review(Path.t(), map(), boolean()) :: :ok | {:error, term()}
  def review(dir, example, relevant)
      when is_binary(dir) and is_map(example) and is_boolean(relevant) do
    with {:ok, items} <- pending(dir),
         true <- Enum.any?(items, fn candidate ->
           candidate.session_id == Map.get(example, :session_id) and
             candidate.turn_id == Map.get(example, :turn_id) and
             candidate.episode_fingerprint == Map.get(example, :episode_fingerprint)
         end) do
      TrainingArchive.record_feedback(
        %{
          session_id: example.session_id,
          turn_id: example.turn_id,
          reviewed_by: "local_human_reviewer",
          label: %{
            kind: "memory_relevance",
            episode_fingerprint: example.episode_fingerprint,
            relevant: relevant
          }
        },
        config: [enabled?: true, dir: dir]
      )
    else
      false -> {:error, :not_pending}
      {:error, _} = error -> error
    end
  end

  def review(_dir, _example, _relevant), do: {:error, :invalid_review}

  defp read_jsonl(path, optional \\ false) do
    cond do
      optional and not File.exists?(path) ->
        {:ok, []}

      not File.exists?(path) ->
        {:error, {:missing_archive, Path.basename(path)}}

      true ->
        path
        |> File.stream!([], :line)
        |> Stream.with_index(1)
        |> Enum.reduce_while({:ok, []}, fn {line, index}, {:ok, acc} ->
          cond do
            index > @max_lines ->
              {:halt, {:error, {:archive_too_large, Path.basename(path)}}}

            String.trim(line) == "" ->
              {:cont, {:ok, acc}}

            true ->
              case Jason.decode(line) do
                {:ok, %{} = record} ->
                  {:cont, {:ok, [record | acc]}}

                _ ->
                  {:halt, {:error, {:invalid_jsonl_record, Path.basename(path), index}}}
              end
          end
        end)
        |> case do
          {:ok, rows} -> {:ok, Enum.reverse(rows)}
          error -> error
        end
    end
  rescue
    error in File.Error -> {:error, {:archive_io_error, error.reason}}
  end

  defp reject_truncation(events) do
    if Enum.any?(events, &(&1["kind"] == "trace_truncated")) do
      {:error, :truncated_hippocampal_trace}
    else
      :ok
    end
  end

  defp summary(%{"slate" => slate}) when is_map(slate) do
    sentence = slate["sentence"]

    terms =
      (List.wrap(slate["winners"]) ++ List.wrap(slate["tokens"]))
      |> Enum.map(&word/1)
      |> Enum.reject(&(&1 == ""))
      |> Enum.uniq()
      |> Enum.take(24)
      |> Enum.join(", ")

    case {sentence, terms} do
      {text, tokens} when is_binary(text) and text != "" ->
        String.slice(text, 0, 240) <> if(tokens != "", do: " | Terms: " <> tokens, else: "")

      {_, ""} ->
        inspect(slate, limit: 8, printable_limit: 260)

      {_, tokens} ->
        "Terms: " <> tokens
    end
  end

  defp summary(other), do: inspect(other, limit: 6, printable_limit: 260)

  defp word(%{} = token) do
    Enum.find_value(["lemma", "norm", "phrase", "text", "word", "id"], "", fn key ->
      case Map.get(token, key) do
        value when is_binary(value) and value != "" -> value
        _ -> nil
      end
    end)
  end

  defp word(value) when is_binary(value), do: value
  defp word(_), do: ""
end
