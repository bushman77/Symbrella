defmodule Brain.Cerebellum.Dataset do
  @moduledoc """
  Read-only loader for reviewed Cerebellum word-sense data.

  JSONL v1, Cerebellum feature schema v2.

  Converts JSON keys to known atom keys, decodes context
  hashes, validates candidate scores, and prevents leakage
  between training and evaluation groups.

  Does not train or modify model weights.
  """

  @dataset_version 1
  @feature_schema 2

  @features [
    {"context_fit", :context_fit},
    {"lex_fit", :lex_fit},
    {"rel_prior", :rel_prior},
    {"activation", :activation},
    {"intent_bias", :intent_bias}
  ]

  @required_text ~w(
    id group_id sentence token reviewed_by label_id
  )

  @spec load_jsonl(Path.t()) ::
          {:ok, map()} | {:error, term()}

  def load_jsonl(path) when is_binary(path) do
    with :ok <- check_runtime_schema(),
         {:ok, content} <- File.read(path),
         {:ok, examples} <- decode_lines(content) do
      build_dataset(examples)
    end
  end

  def load_jsonl(_), do: {:error, :invalid_path}

  defp check_runtime_schema do
    config = Application.get_env(:brain, :cerebellum, %{})

    if Map.get(config, :feature_schema, 1) == @feature_schema do
      :ok
    else
      {:error, :runtime_feature_schema_mismatch}
    end
  end

  defp decode_lines(content) do
    result =
      content
      |> String.split("\n")
      |> Enum.with_index(1)
      |> Enum.reduce_while({:ok, []}, fn {line, number}, {:ok, acc} ->
        if String.trim(line) == "" do
          {:cont, {:ok, acc}}
        else
          with {:ok, raw} <- Jason.decode(line),
               {:ok, example} <- decode_example(raw) do
            {:cont, {:ok, [example | acc]}}
          else
            {:error, reason} ->
              {:halt, {:error, {:line, number, reason}}}
          end
        end
      end)

    case result do
      {:ok, examples} -> {:ok, Enum.reverse(examples)}
      error -> error
    end
  end

  defp decode_example(raw) when is_map(raw) do
    with :ok <- check_versions(raw),
         :ok <- check_required_fields(raw),
         {:ok, split} <- parse_split(raw["split"]),
         {:ok, source} <- parse_source(raw["source"]),
         {:ok, context_key} <- parse_context(raw["context_key_hex"]),
         {:ok, candidates} <- parse_candidates(raw["candidates"]),
         {:ok, scores} <- parse_scores(raw["base_scores"]),
         :ok <- check_alignment(candidates, scores, raw["label_id"]) do
      {:ok,
       %{
         id: raw["id"],
         group_id: raw["group_id"],
         sentence: raw["sentence"],
         token: raw["token"],
         token_index: raw["token_index"],
         split: split,
         source: source,
         reviewed_by: raw["reviewed_by"],
         label_id: raw["label_id"],
         context_key: context_key,
         candidates: candidates,
         base_scores: scores,
         feature_schema: @feature_schema
       }}
    end
  end

  defp decode_example(_), do: {:error, :expected_object}

  defp check_versions(raw) do
    cond do
      raw["v"] != @dataset_version ->
        {:error, :unsupported_dataset_version}

      raw["feature_schema"] != @feature_schema ->
        {:error, :unsupported_feature_schema}

      true ->
        :ok
    end
  end

  defp check_required_fields(raw) do
    text_valid? =
      Enum.all?(@required_text, fn field ->
        nonblank?(raw[field])
      end)

    index_valid? =
      is_integer(raw["token_index"]) and
        raw["token_index"] >= 0

    if text_valid? and index_valid? do
      :ok
    else
      {:error, :missing_or_invalid_fields}
    end
  end

  defp parse_split("train"), do: {:ok, :train}
  defp parse_split("eval"), do: {:ok, :eval}
  defp parse_split(_), do: {:error, :invalid_split}

  defp parse_source("human_review"), do: {:ok, :human_review}
  defp parse_source(_), do: {:error, :unverified_source}

  defp parse_context(hex)
       when is_binary(hex) and byte_size(hex) == 64 do
    case Base.decode16(hex, case: :mixed) do
      {:ok, binary} when byte_size(binary) == 32 ->
        {:ok, binary}

      _ ->
        {:error, :invalid_context_key}
    end
  end

  defp parse_context(_), do: {:error, :invalid_context_key}

  defp parse_candidates(candidates)
       when is_list(candidates) and length(candidates) >= 2 do
    result =
      Enum.reduce_while(candidates, {:ok, []}, fn raw, {:ok, acc} ->
        case parse_candidate(raw) do
          {:ok, candidate} ->
            {:cont, {:ok, [candidate | acc]}}

          error ->
            {:halt, error}
        end
      end)

    case result do
      {:ok, rows} -> {:ok, Enum.reverse(rows)}
      error -> error
    end
  end

  defp parse_candidates(_), do: {:error, :invalid_candidates}

  defp parse_candidate(raw) when is_map(raw) do
    valid_features? =
      Enum.all?(@features, fn {key, _atom} ->
        unit_number?(raw[key])
      end)

    if nonblank?(raw["id"]) and valid_features? do
      features =
        Map.new(@features, fn {key, atom} ->
          {atom, raw[key] * 1.0}
        end)

      {:ok, Map.put(features, :id, raw["id"])}
    else
      {:error, :invalid_candidate}
    end
  end

  defp parse_candidate(_), do: {:error, :invalid_candidate}

  defp parse_scores(scores) when is_map(scores) do
    valid? =
      Enum.all?(scores, fn {id, score} ->
        nonblank?(id) and unit_number?(score)
      end)

    if valid? do
      {:ok, scores}
    else
      {:error, :invalid_scores}
    end
  end

  defp parse_scores(_), do: {:error, :invalid_scores}

  defp check_alignment(candidates, scores, label_id) do
    ids = Enum.map(candidates, & &1.id)

    cond do
      length(Enum.uniq(ids)) != length(ids) ->
        {:error, :duplicate_candidate_id}

      MapSet.new(ids) != MapSet.new(Map.keys(scores)) ->
        {:error, :score_candidates_mismatch}

      label_id not in ids ->
        {:error, :label_not_in_candidates}

      true ->
        :ok
    end
  end

  defp build_dataset([]), do: {:error, :empty_dataset}

  defp build_dataset(examples) do
    initial = %{
      ids: MapSet.new(),
      occurrences: MapSet.new(),
      groups: %{},
      train: [],
      eval: []
    }

    result =
      Enum.reduce_while(examples, {:ok, initial}, fn example, {:ok, acc} ->
        occurrence = {
          normalize_sentence(example.sentence),
          example.token_index
        }

        previous_split = Map.get(acc.groups, example.group_id)

        cond do
          MapSet.member?(acc.ids, example.id) ->
            {:halt, {:error, {:duplicate_id, example.id}}}

          MapSet.member?(acc.occurrences, occurrence) ->
            {:halt, {:error, {:duplicate_occurrence, example.id}}}

          previous_split != nil and previous_split != example.split ->
            {:halt, {:error, {:split_leakage, example.group_id}}}

          true ->
            updated =
              acc
              |> Map.update!(:ids, &MapSet.put(&1, example.id))
              |> Map.update!(:occurrences, &MapSet.put(&1, occurrence))
              |> Map.update!(:groups, fn groups ->
                Map.put(groups, example.group_id, example.split)
              end)
              |> Map.update!(example.split, fn rows ->
                [example | rows]
              end)

            {:cont, {:ok, updated}}
        end
      end)

    case result do
      {:error, _reason} = error ->
        error

      {:ok, acc} ->
        train = Enum.reverse(acc.train)
        evaluation = Enum.reverse(acc.eval)

        {:ok,
         %{
           v: @dataset_version,
           feature_schema: @feature_schema,
           total: length(examples),
           train_count: length(train),
           eval_count: length(evaluation),
           train: train,
           eval: evaluation
         }}
    end
  end

  defp normalize_sentence(sentence) do
    sentence
    |> String.trim()
    |> String.downcase()
    |> String.replace(~r/\s+/, " ")
  end

  defp nonblank?(value) when is_binary(value) do
    String.trim(value) != ""
  end

  defp nonblank?(_), do: false

  defp unit_number?(value) when is_number(value) do
    value >= 0.0 and value <= 1.0
  end

  defp unit_number?(_), do: false
end
