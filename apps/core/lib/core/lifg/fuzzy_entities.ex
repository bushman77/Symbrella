defmodule Core.LIFG.FuzzyEntities do
  @moduledoc """
  Core-side bridge from fuzzy text repair into LIFG sense candidates.

  Brain owns LIFG and must not depend on Core.Text.Fuzzy. This module runs in
  Core before the Brain.LIFG call and injects protected, token-aligned entity
  candidates when fuzzy repair canonicalizes a known medication mention.
  """

  alias Core.Pipeline.Trace

  @medication_entities %{
    "quetiapine" => %{
      entity_type: :medication,
      definition:
        "Medication entity; an antipsychotic medicine often discussed in dosing or missed-dose contexts.",
      example: "I forgot my quetiapine."
    },
    "seroquel" => %{
      entity_type: :medication,
      definition:
        "Medication entity; a medication brand name often discussed in dosing or missed-dose contexts.",
      example: "I forgot my Seroquel."
    }
  }

  @spec attach(map(), keyword()) :: map()
  def attach(si, opts \\ [])

  def attach(%{} = si, opts) when is_list(opts) do
    corrections = medication_corrections(si)

    if corrections == [] do
      si
    else
      {sense_candidates, inserted} =
        si
        |> tokens()
        |> Enum.with_index()
        |> Enum.reduce({sense_candidates(si), []}, fn {tok, fallback_idx}, {acc, added} ->
          token_norm = token_norm(tok)

          case matching_medication_correction(corrections, token_norm) do
            nil ->
              {acc, added}

            %{original: original, replacement: replacement, confidence: confidence} ->
              idx = token_index(tok, fallback_idx)
              cand = medication_candidate(original, replacement, confidence, idx)
              {put_candidate(acc, idx, cand), [candidate_summary(cand, idx) | added]}
          end
        end)

      case inserted do
        [] ->
          si

        _ ->
          inserted = Enum.reverse(inserted)

          si
          |> Map.put(:sense_candidates, sense_candidates)
          |> Trace.append(:fuzzy_entity_candidates,
            decision: :attached,
            reason: :fuzzy_medication_entity,
            scores: %{count: length(inserted)},
            meta: %{entities: inserted}
          )
      end
    end
  end

  def attach(si, _opts), do: si

  defp medication_corrections(%{} = si) do
    si
    |> Map.get(:fuzzy_text, %{})
    |> map_get(:corrections, [])
    |> List.wrap()
    |> Enum.map(&normalize_correction/1)
    |> Enum.filter(fn
      %{original: original, replacement: replacement} ->
        original != "" and Map.has_key?(@medication_entities, replacement)

      _ ->
        false
    end)
    |> Enum.uniq_by(fn %{original: original, replacement: replacement} ->
      {original, replacement}
    end)
  end

  defp normalize_correction(%{} = correction) do
    original = correction |> map_get(:original, "") |> norm()
    replacement = correction |> map_get(:replacement, "") |> norm()
    confidence = correction |> map_get(:confidence, 0.0) |> to_float()

    %{original: original, replacement: replacement, confidence: confidence}
  end

  defp normalize_correction(_), do: %{original: "", replacement: "", confidence: 0.0}

  defp matching_medication_correction(corrections, token_norm) when is_binary(token_norm) do
    Enum.find(corrections, fn %{original: original} -> original == token_norm end)
  end

  defp matching_medication_correction(_corrections, _token_norm), do: nil

  defp medication_candidate(original, replacement, confidence, token_index) do
    spec = Map.fetch!(@medication_entities, replacement)
    score = max(0.90, min(1.0, confidence))

    %{
      id: "#{original}|entity|medication",
      lemma: replacement,
      norm: original,
      canonical: replacement,
      pos: "entity",
      entity_type: spec.entity_type,
      source: :medical_entity_fallback,
      activation: score,
      score: score,
      protected?: true,
      token_index: token_index,
      corrected_from: original,
      definition: spec.definition,
      example: spec.example,
      features: %{
        lex_fit: 1.0,
        rel_prior: 1.0,
        activation: score,
        intent_bias: 0.05
      }
    }
  end

  defp candidate_summary(%{} = cand, idx) do
    %{
      token_index: idx,
      id: Map.get(cand, :id),
      lemma: Map.get(cand, :lemma),
      corrected_from: Map.get(cand, :corrected_from),
      source: Map.get(cand, :source),
      score: Map.get(cand, :score)
    }
  end

  defp tokens(%{} = si) do
    case Map.get(si, :tokens) || Map.get(si, "tokens") do
      list when is_list(list) -> list
      _ -> []
    end
  end

  defp sense_candidates(%{} = si) do
    case Map.get(si, :sense_candidates) || Map.get(si, "sense_candidates") do
      %{} = map -> map
      _ -> %{}
    end
  end

  defp put_candidate(sc, idx, cand) when is_map(sc) and is_integer(idx) do
    key =
      cond do
        Map.has_key?(sc, idx) -> idx
        Map.has_key?(sc, to_string(idx)) -> to_string(idx)
        true -> idx
      end

    Map.update(sc, key, [cand], fn existing ->
      existing
      |> List.wrap()
      |> Enum.concat([cand])
      |> Enum.filter(&is_map/1)
      |> Enum.uniq_by(fn c -> map_get(c, :id, nil) end)
      |> Enum.reject(fn c -> is_nil(map_get(c, :id, nil)) end)
    end)
  end

  defp put_candidate(sc, _idx, _cand), do: sc

  defp token_index(tok, fallback) when is_map(tok) do
    idx = Map.get(tok, :index) || Map.get(tok, "index")
    if is_integer(idx), do: idx, else: fallback
  end

  defp token_index(_tok, fallback), do: fallback

  defp token_norm(tok) when is_map(tok) do
    tok
    |> token_surface()
    |> norm()
  end

  defp token_norm(_), do: ""

  defp token_surface(tok) when is_map(tok) do
    Map.get(tok, :norm) ||
      Map.get(tok, "norm") ||
      Map.get(tok, :phrase) ||
      Map.get(tok, "phrase") ||
      ""
  end

  defp norm(value) when is_binary(value) do
    value
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/^\p{P}+/u, "")
    |> String.replace(~r/\p{P}+$/u, "")
    |> String.replace(~r/\s+/u, " ")
  end

  defp norm(value), do: value |> to_string() |> norm()

  defp map_get(map, key, default) when is_map(map) and is_atom(key) do
    Map.get(map, key, Map.get(map, Atom.to_string(key), default))
  end

  defp map_get(_map, _key, default), do: default

  defp to_float(value) when is_integer(value), do: value * 1.0
  defp to_float(value) when is_float(value), do: value
  defp to_float(_), do: 0.0
end
