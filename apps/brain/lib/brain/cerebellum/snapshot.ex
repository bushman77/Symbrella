defmodule Brain.Cerebellum.Snapshot do
  @moduledoc """
  Opt-in capture of LIFG Stage1's pre-Cerebellum scores.

  Records are UNREVIEWED and cannot be used as training labels.
  This module neither calibrates scores nor learns model weights.
  """

  @dataset_version 1
  @feature_schema 2
  @features [:context_fit, :lex_fit, :rel_prior, :activation, :intent_bias]

  @spec build(map()) :: {:ok, map()} | {:error, atom()}
  def build(%{
        sentence: sentence,
        token: token,
        token_index: index,
        context_key: context_key,
        candidates: candidates,
        base_scores: scores
      }) do
    with :ok <- check_schema(),
         :ok <- check_metadata(sentence, token, index, context_key),
         :ok <- check_candidates(candidates, scores) do
      winner_id = scores |> Enum.max_by(fn {id, score} -> {score, id} end) |> elem(0)

      {:ok,
       %{
         v: @dataset_version,
         feature_schema: @feature_schema,
         id: "stage1-" <> random_id(),
         group_id: token |> String.trim() |> String.downcase(),
         captured_at: DateTime.utc_now() |> DateTime.to_iso8601(),
         sentence: sentence,
         token: token,
         token_index: index,
         source: "stage1_unreviewed",
         reviewed_by: nil,
         split: "unassigned",
         label_id: nil,
         context_key_hex: Base.encode16(context_key, case: :lower),
         base_winner_id: winner_id,
         candidates: Enum.map(candidates, &Map.take(&1, [:id | @features])),
         base_scores: scores
       }}
    end
  end

  def build(_), do: {:error, :invalid_snapshot}

  @doc "Append one validated JSONL snapshot to an explicitly chosen path."
  @spec record(Path.t(), map()) :: :ok | {:error, term()}
  def record(path, attrs) when is_binary(path) and is_map(attrs) do
    if String.trim(path) == "" do
      {:error, :invalid_path}
    else
      expanded = Path.expand(path)

      with {:ok, snapshot} <- build(attrs),
           {:ok, json} <- Jason.encode(snapshot),
           :ok <- File.mkdir_p(Path.dirname(expanded)),
           :ok <- File.write(expanded, json <> "\n", [:append, :binary]) do
        :ok
      end
    end
  end

  def record(_, _), do: {:error, :invalid_path}

  defp check_schema do
    configured =
      Application.get_env(:brain, :cerebellum, %{})
      |> Map.get(:feature_schema, 1)

    if configured == @feature_schema,
      do: :ok,
      else: {:error, :feature_schema_mismatch}
  end

  defp check_metadata(sentence, token, index, context_key) do
    cond do
      not nonblank?(sentence) or not nonblank?(token) ->
        {:error, :missing_sentence_or_token}

      not (is_integer(index) and index >= 0) ->
        {:error, :invalid_token_index}

      not (is_binary(context_key) and byte_size(context_key) == 32) ->
        {:error, :invalid_context_key}

      true ->
        :ok
    end
  end

  defp check_candidates(candidates, scores)
       when is_list(candidates) and is_map(scores) do
    ids =
      Enum.map(candidates, fn
        %{id: id} -> id
        _ -> nil
      end)

    valid_features? =
      Enum.all?(candidates, fn
        %{id: id} = candidate ->
          nonblank?(id) and
            Enum.all?(@features, fn f -> unit_number?(Map.get(candidate, f)) end)

        _ ->
          false
      end)

    cond do
      length(ids) < 2 or not valid_features? ->
        {:error, :invalid_candidates}

      length(Enum.uniq(ids)) != length(ids) ->
        {:error, :duplicate_candidate_id}

      MapSet.new(ids) != MapSet.new(Map.keys(scores)) ->
        {:error, :score_candidates_mismatch}

      not Enum.all?(Map.values(scores), &unit_number?/1) ->
        {:error, :invalid_base_scores}

      true ->
        :ok
    end
  end

  defp check_candidates(_, _), do: {:error, :invalid_candidates}

  defp random_id do
    16 |> :crypto.strong_rand_bytes() |> Base.encode16(case: :lower)
  end

  defp nonblank?(value) when is_binary(value), do: String.trim(value) != ""
  defp nonblank?(_), do: false

  defp unit_number?(value) when is_number(value), do: value >= 0 and value <= 1
  defp unit_number?(_), do: false
end
