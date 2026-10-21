defmodule Brain.LIFG.ArgumentCompatibility do
  @moduledoc """
  Pure, shadow-only comparison of independently observed argument structure
  with *existing* dictionary-sense grammatical tags.

  Positive support is a diagnostic compatibility signal, never a calibrated
  probability or proof of meaning. Absence of a tag is not incompatibility.
  There are no mutations, winner flips, candidate removals or network calls.
  """

  @spec compare([map()], [map()]) :: [map()]
  def compare(frames, cells) when is_list(frames) and is_list(cells) do
    frames
    |> Enum.filter(&eligible?/1)
    |> Enum.map(fn frame ->
      matches =
        cells
        |> Enum.filter(&matching_sense?(&1, frame.predicate_surface))
        |> Enum.uniq_by(&Map.get(&1, :id))
        |> Enum.map(fn candidate ->
          tags = Map.get(candidate, :semantic_atoms, []) |> List.wrap()
          support = support(frame.construction, tags)

          %{id: candidate.id, positive_support: support,
            grammatical_tags: Enum.filter(tags, &(&1 in [
              "tag:transitive", "tag:intransitive", "tag:ditransitive",
              "tag:ambitransitive"
            ]))}
        end)
        |> Enum.sort_by(&{-&1.positive_support, &1.id})

      %{status: :shadow_only, predicate: frame.predicate,
        construction: frame.construction, object_head: frame.object_head,
        recipient: frame.recipient, object_status: frame.object_status,
        candidate_count: length(matches),
        candidates: matches,
        semantic_sense_verified?: false, winner_changed?: false,
        applied_to_lifg?: false, brain_cells_stimulated?: false,
        evidence_source: :dictionary_argument_tags}
    end)
  end

  def compare(_, _), do: []

  defp eligible?(%{
    status: :candidate, source: :original_syntax_and_dictionary_pos,
    meaning_verified?: false, event_verified?: false,
    construction: construction, predicate: predicate,
    predicate_surface: surface, object_head: object,
    evidence: evidence
  }) when construction in [:recipient_object, :direct_object] and
          is_integer(predicate) and is_integer(object) and
          is_binary(surface) and is_list(evidence) do
    Enum.all?([:supported_predicate_role, :bounded_argument_order,
               :determiner_witness, :nominal_head_surface],
      &(&1 in evidence)) and
      (construction != :recipient_object or :pronoun_recipient_witness in evidence)
  end

  defp eligible?(_), do: false

  defp matching_sense?(%{id: id, pos: "verb"} = cell, surface)
       when is_binary(id) do
    String.starts_with?(id, surface <> "|verb|") and
      String.downcase(to_string(Map.get(cell, :norm, ""))) == surface and
      not String.ends_with?(id, "|fallback")
  end

  defp matching_sense?(_, _), do: false

  defp support(:recipient_object, tags) do
    if "tag:ditransitive" in tags, do: 0.10, else: 0.0
  end

  defp support(:direct_object, tags) do
    cond do
      "tag:transitive" in tags -> 0.06
      "tag:ambitransitive" in tags -> 0.06
      "tag:ditransitive" in tags -> 0.04
      true -> 0.0
    end
  end
end
