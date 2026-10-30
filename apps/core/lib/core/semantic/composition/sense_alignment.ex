defmodule Core.Semantic.Composition.SenseAlignment do
  @moduledoc """
  Experimental, read-only dictionary-sense alignment audit for a typed event
  proof. It asks whether a selected *preposition sense* has an explicit
  dictionary-gloss family consistent with the relation in the lambda term.

  This is NOT a lexical-semantic proof: the gloss classifier is a deliberately
  limited hypothesis and cannot establish the speaker's intended sense.
  Inconclusive definitions remain unresolved; family disagreements do not
  invalidate or modify existing mathematical proofs or LIFG decisions.
  """

  alias Core.Semantic.Composition.Event

  # Deliberately bounded high-precision cues for a pilot; never infer a sense
  # from its numeric ID or from the surface word alone. Explicit prefixes
  # avoid treating incidental words like "position" as semantic evidence.
  @position ~r/\AIndicating position or location\.\s*/iu
  @contact ~r/\ADenoting physical contact or interaction\b/iu
  @topic ~r/\ADealing with the subject of\b/iu
  @opposition ~r/\bAgainst;\s*in opposition to\./iu

  @spec audit(map(), map()) :: map()
  def audit(%{} = proof, %{} = candidate) do
    witness =
      proof
      |> Map.get(:premises, %{})
      |> Map.get(:lexical_witnesses, [])
      |> List.wrap()
      |> Enum.find(fn
        %{role: :preposition} -> true
        _ -> false
      end)

    proof_valid? = Event.verify(proof) == :ok
    witness_id = if(is_map(witness), do: Map.get(witness, :sense_id), else: nil)
    candidate_id = field(candidate, :id)
    word = if(is_map(witness), do: Map.get(witness, :word), else: nil)

    attested_witness? =
      is_binary(witness_id) and witness_id == candidate_id and
        is_binary(word) and
        String.downcase(word) == normalize(field(candidate, :norm)) and
        normalize(field(candidate, :pos)) == "preposition"

    relation = get_in(proof, [:event_semantics, :spatial, :relation])
    relation_family = relation_family(relation)
    gloss_family = gloss_family(field(candidate, :definition))

    status =
      cond do
        not proof_valid? -> :invalid_formal_proof
        not attested_witness? -> :unmatched_dictionary_witness
        is_nil(relation_family) -> :unclassified_composed_relation
        is_nil(gloss_family) -> :unclassified_dictionary_definition
        relation_family == gloss_family -> :family_consistent
        true -> :different_relation_family
      end

    %{
      status: status,
      sense_id: candidate_id,
      witness_sense_id: witness_id,
      formal_proof_valid?: proof_valid?,
      candidate_matches_witness?: attested_witness?,
      composed_relation: relation,
      composed_relation_family: relation_family,
      dictionary_gloss_family: gloss_family,
      classification_basis: :bounded_dictionary_definition_cues,
      verified_intended_sense?: false,
      modifies_lifg?: false
    }
  end

  def audit(_, _), do: %{status: :invalid_input, verified_intended_sense?: false,
                          modifies_lifg?: false}

  # Current event composer uses :on for a spatial-location relation. It does
  # not model force, aboutness, or opposition, although the word has such
  # dictionary senses. Other relations are outside this pilot.
  defp relation_family(:on), do: :spatial_location
  defp relation_family(_), do: nil

  defp gloss_family(definition) when is_binary(definition) do
    text = String.trim(definition)

    cond do
      Regex.match?(@opposition, text) -> :opposition
      Regex.match?(@topic, text) -> :discourse_topic
      Regex.match?(@contact, text) -> :physical_contact
      Regex.match?(@position, text) -> :spatial_location
      true -> nil
    end
  end

  defp gloss_family(_), do: nil

  defp normalize(s) when is_binary(s), do: String.downcase(String.trim(s))
  defp normalize(_), do: ""

  defp field(map, key) when is_map(map),
    do: Map.get(map, key) || Map.get(map, Atom.to_string(key))
end
