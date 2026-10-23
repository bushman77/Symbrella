defmodule SymbrellaWeb.SocialDiscourse do
  @moduledoc """
  Bounded, read-only discourse ledger. Only supported sentence meanings become
  interpretations. A repeated surface term is a possible link, never coreference.
  """
  @stopwords ~w(the a an and or of to in on is are was were at for with from by
    it its that this those these i you we they he she my your their had has have
    do does did not only what how who when where about say look)
  @max_links 12
  @max_terms 8

  def build(segments) when is_list(segments) do
    claims =
      segments
      |> Enum.with_index()
      |> Enum.flat_map(fn {segment, index} ->
        meaning = get_in(segment, [:result, :sentence_meaning]) || %{}
        if Map.get(segment, :status) == :analyzed and
             Map.get(meaning, :status) in [:structurally_interpreted, :interpreted, :understood, :resolved] do
          [%{
            segment_index: index,
            speaker: segment.author,
            source_label: segment.label,
            speech_act: Map.get(meaning, :speech_act),
            kind: Map.get(meaning, :kind),
            interpretation: Map.get(meaning, :definition),
            verification: :external_unverified
          }]
        else
          []
        end
      end)

    links =
      claims
      |> Enum.flat_map(fn claim ->
        source = Enum.at(segments, claim.segment_index)
        terms = terms(Map.get(source, :excerpt, ""))
        segments
        |> Enum.with_index()
        |> Enum.filter(fn {other, index} ->
          index != claim.segment_index and Map.get(other, :status) == :analyzed and
            Map.get(other, :author) == claim.speaker
        end)
        |> Enum.flat_map(fn {other, index} ->
          overlap = MapSet.intersection(terms, terms(Map.get(other, :excerpt, "")))
            |> MapSet.to_list() |> Enum.sort() |> Enum.take(@max_terms)
          if overlap == [] do
            []
          else
            [%{from: claim.segment_index, to: index, shared_terms: overlap,
              status: :lexical_overlap_only, coreference_verified?: false}]
          end
        end)
      end)
      |> Enum.take(@max_links)

    %{
      status: if(claims == [], do: :no_supported_sentence_meanings, else: :partial_discourse),
      interpretations: claims,
      lexical_links: links,
      unresolved: ["Entity identity across sentences is not established.",
        "Shared words do not establish anaphora, causality, agreement or factual truth."],
      source: :external_unverified,
      mode: :read_only
    }
  end

  defp terms(text) when is_binary(text) do
    Regex.scan(~r/[\p{L}]{3,}/u, String.downcase(text))
    |> Enum.map(&hd/1)
    |> Enum.reject(&(&1 in @stopwords))
    |> MapSet.new()
  end

  defp terms(_), do: MapSet.new()
end
