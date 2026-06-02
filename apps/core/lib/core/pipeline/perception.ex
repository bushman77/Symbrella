defmodule Core.Pipeline.Perception do
  @moduledoc """
  Deterministic sensory/perception enrichment for `Core.SemanticInput`.

  This is the first stable contract for later learned perception. It avoids
  Axon/Nx for now and writes inspectable placeholder signals that downstream
  stages can consume without depending on a trained model.
  """

  @version 1

  @spec run(map(), keyword()) :: map()
  def run(%{tokens: tokens} = si, opts) when is_list(tokens) and is_list(opts) do
    perception = build_perception(si, opts)

    si
    |> Map.put(:perception, perception)
    |> Core.Pipeline.Trace.append(
      :perception,
      decision: :attached,
      reason: :deterministic_perception,
      scores: %{
        token_count: perception.meta.token_count,
        candidate_bucket_count: perception.meta.candidate_bucket_count
      },
      meta: %{
        backend: perception.backend,
        version: perception.version,
        token_count: perception.meta.token_count,
        candidate_bucket_count: perception.meta.candidate_bucket_count
      }
    )
  end

  def run(si, _opts), do: si

  defp build_perception(%{} = si, opts) do
    tokens = Map.get(si, :tokens, [])
    candidates = candidate_map(si)
    evidence? = present?(Map.get(si, :evidence))
    episode? = present?(Map.get(si, :episode))

    rows =
      tokens
      |> Enum.with_index()
      |> Enum.map(fn {token, fallback_idx} ->
        idx = token_index(token, fallback_idx)
        phrase = token_phrase(token)
        bucket = Map.get(candidates, idx, [])

        {idx,
         %{
           vector: Core.Vectors.fetch({:perception_token, phrase, idx}),
           salience: salience(token, bucket, evidence?, episode?, opts),
           ambiguity: ambiguity(bucket),
           candidate_bias: candidate_bias(bucket),
           phrase_coherence: phrase_coherence(token, bucket)
         }}
      end)

    %{
      version: @version,
      backend: :deterministic,
      token_vectors: map_from_rows(rows, :vector),
      salience: map_from_rows(rows, :salience),
      ambiguity: map_from_rows(rows, :ambiguity),
      candidate_bias: map_from_rows(rows, :candidate_bias),
      phrase_coherence: map_from_rows(rows, :phrase_coherence),
      meta: %{
        token_count: length(tokens),
        candidate_bucket_count: map_size(candidates),
        evidence?: evidence?,
        episode?: episode?
      }
    }
  end

  defp map_from_rows(rows, key) do
    Map.new(rows, fn {idx, values} -> {idx, Map.fetch!(values, key)} end)
  end

  defp salience(token, bucket, evidence?, episode?, opts) do
    base = Keyword.get(opts, :perception_base_salience, 0.12)
    candidate_weight = Keyword.get(opts, :perception_candidate_weight, 0.08)
    mwe_weight = Keyword.get(opts, :perception_mwe_weight, 0.22)
    context_weight = Keyword.get(opts, :perception_context_weight, 0.05)

    mwe_bonus = if multiword?(token), do: mwe_weight, else: 0.0

    context_bonus =
      case {evidence?, episode?} do
        {true, true} -> context_weight * 2
        {true, false} -> context_weight
        {false, true} -> context_weight
        {false, false} -> 0.0
      end

    candidate_bonus = min(length(bucket) * candidate_weight, 0.36)

    clamp01(base + candidate_bonus + mwe_bonus + context_bonus)
  end

  defp ambiguity([]), do: 0.0
  defp ambiguity([_one]), do: 0.12

  defp ambiguity(bucket) when is_list(bucket) do
    scores =
      bucket
      |> Enum.map(&candidate_score/1)
      |> Enum.sort(:desc)

    top = Enum.at(scores, 0, 0.0)
    second = Enum.at(scores, 1, top)
    margin = abs(top - second)

    bucket_pressure = min(length(bucket) / 5.0, 1.0)
    margin_pressure = 1.0 - min(margin / 0.5, 1.0)

    clamp01(bucket_pressure * 0.65 + margin_pressure * 0.35)
  end

  defp candidate_bias([]), do: %{}
  defp candidate_bias([_one]), do: %{}

  defp candidate_bias(bucket) when is_list(bucket) do
    scores = Enum.map(bucket, &candidate_score/1)
    max_score = Enum.max(scores, fn -> 0.0 end)
    min_score = Enum.min(scores, fn -> 0.0 end)
    span = max(max_score - min_score, 1.0e-9)

    bucket
    |> Enum.flat_map(fn candidate ->
      case candidate_id(candidate) do
        nil ->
          []

        id ->
          normalized = (candidate_score(candidate) - min_score) / span
          [{id, Float.round(normalized * 0.2 - 0.1, 4)}]
      end
    end)
    |> Map.new()
  end

  defp phrase_coherence(token, bucket) do
    cond do
      multiword?(token) and bucket != [] -> 0.78
      multiword?(token) -> 0.62
      bucket != [] -> 0.52
      true -> 0.35
    end
  end

  defp normalized_candidate_map(candidates) when is_map(candidates) do
    Map.new(candidates, fn {idx, bucket} -> {normalize_idx(idx), List.wrap(bucket)} end)
  end

  defp normalized_candidate_map(_), do: %{}

  defp candidate_map(%{} = si) do
    si
    |> Map.get(:sense_candidates, %{})
    |> normalized_candidate_map()
    |> case do
      candidates when map_size(candidates) > 0 ->
        candidates

      _ ->
        si
        |> Map.get(:active_cells, [])
        |> active_cells_candidate_map()
    end
  end

  defp active_cells_candidate_map(active_cells) when is_list(active_cells) do
    active_cells
    |> Enum.filter(&is_map/1)
    |> Enum.reduce(%{}, fn cell, acc ->
      case normalize_idx(mget(cell, :token_index)) do
        idx when is_integer(idx) -> Map.update(acc, idx, [cell], &[cell | &1])
        _ -> acc
      end
    end)
    |> Map.new(fn {idx, bucket} -> {idx, Enum.reverse(bucket)} end)
  end

  defp active_cells_candidate_map(_active_cells), do: %{}

  defp token_index(%{} = token, fallback) do
    token
    |> mget(:token_index)
    |> case do
      nil -> mget(token, :index)
      idx -> idx
    end
    |> normalize_idx(fallback)
  end

  defp token_index(_token, fallback), do: fallback

  defp normalize_idx(idx, fallback \\ nil)
  defp normalize_idx(idx, _fallback) when is_integer(idx), do: idx

  defp normalize_idx(idx, fallback) when is_binary(idx) do
    case Integer.parse(idx) do
      {int, ""} -> int
      _ -> fallback
    end
  end

  defp normalize_idx(_idx, fallback), do: fallback

  defp token_phrase(%{} = token) do
    token
    |> mget(:phrase)
    |> case do
      nil -> mget(token, :text) || mget(token, :word) || ""
      phrase -> phrase
    end
    |> to_string()
    |> String.downcase()
    |> String.trim()
  end

  defp token_phrase(_token), do: ""

  defp multiword?(%{} = token) do
    mget(token, :mw) == true
  end

  defp multiword?(_token), do: false

  defp candidate_id(%{} = candidate) do
    case mget(candidate, :id) do
      id when is_binary(id) -> id
      _ -> nil
    end
  end

  defp candidate_id(id) when is_binary(id), do: id
  defp candidate_id(_candidate), do: nil

  defp candidate_score(%{} = candidate) do
    case mget(candidate, :score) || mget(candidate, :rel_prior) || mget(candidate, :prob) do
      score when is_number(score) -> score * 1.0
      _ -> 0.0
    end
  end

  defp candidate_score(_candidate), do: 0.0

  defp present?(nil), do: false
  defp present?(%{} = map), do: map_size(map) > 0
  defp present?([]), do: false
  defp present?(_), do: true

  defp clamp01(value) when value < 0.0, do: 0.0
  defp clamp01(value) when value > 1.0, do: 1.0
  defp clamp01(value), do: value

  defp mget(%{} = map, key) when is_atom(key) do
    Map.get(map, key) || Map.get(map, to_string(key))
  end

  defp mget(_map, _key), do: nil
end
