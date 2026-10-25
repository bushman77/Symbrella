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
  def pending(dir), do: list_candidates(dir, :pending)

  @doc "Show previously reviewed candidates for deliberate human relabeling."
  @spec reviewed(Path.t()) :: {:ok, [map()]} | {:error, term()}
  def reviewed(dir), do: list_candidates(dir, :reviewed)

  defp list_candidates(dir, mode) when is_binary(dir) and mode in [:pending, :reviewed] do
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
        |> Enum.reduce(%{}, fn record, acc ->
          key = {record["session_id"], record["turn_id"], get_in(record, ["label", "episode_fingerprint"])}
          Map.put(acc, key, get_in(record, ["label", "relevant"]))
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

          existing = Map.fetch(reviewed, key)
          matches_mode? =
            case {mode, existing} do
              {:pending, :error} -> true
              {:reviewed, {:ok, value}} when is_boolean(value) -> true
              _ -> false
            end

          if turn && is_binary(turn["user_text"]) and
               matches_mode? and not MapSet.member?(seen, key) do
            row = %{
              session_id: event["session_id"],
              turn_id: event["turn_id"],
              episode_fingerprint: event["episode_fingerprint"],
              score: event["score"],
              user_text: turn["user_text"],
              assistant_text: turn["assistant_text"],
              memory_summary: summary(event["episode"]),
              previous_relevant: if(match?({:ok, _}, existing), do: elem(existing, 1), else: nil)
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

  defp list_candidates(_, _), do: {:error, :invalid_directory}

  @doc "Append a new human review, or an auditable correction if correct: true."
  @spec review(Path.t(), map(), boolean(), keyword()) ::
          :ok | :unchanged | {:error, term()}
  def review(dir, example, relevant, opts \\ [])

  def review(dir, example, relevant, opts)
      when is_binary(dir) and is_map(example) and is_boolean(relevant) and is_list(opts) do
    correction? = Keyword.get(opts, :correct, false) == true
    source = if correction?, do: reviewed(dir), else: pending(dir)

    with {:ok, items} <- source,
         %{} = match <- Enum.find(items, fn candidate ->
           candidate.session_id == Map.get(example, :session_id) and
             candidate.turn_id == Map.get(example, :turn_id) and
             candidate.episode_fingerprint == Map.get(example, :episode_fingerprint)
         end) do
      if correction? and match.previous_relevant == relevant do
        :unchanged
      else
        TrainingArchive.record_feedback(
          %{
            session_id: match.session_id,
            turn_id: match.turn_id,
            reviewed_by: "local_human_reviewer",
            correction: correction?,
            note: if(correction?, do: "Explicit correction of prior human relevance review", else: nil),
            label: %{
              kind: "memory_relevance",
              episode_fingerprint: match.episode_fingerprint,
              relevant: relevant
            }
          },
          config: [enabled?: true, dir: dir]
        )
      end
    else
      nil -> {:error, :not_pending}
      {:error, _} = error -> error
    end
  end

  def review(_dir, _example, _relevant, _opts), do: {:error, :invalid_review}

  defp read_jsonl(path, optional \\ false) do
    cond do
      optional and not File.exists?(path) ->
        {:ok, []}

      not File.exists?(path) ->
        {:error, {:missing_archive, Path.basename(path)}}

      true ->
        path
        |> File.stream!(:line, [])
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
