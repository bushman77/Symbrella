defmodule Core.LIFG.SignatureRoles do
  @moduledoc """
  POS role constraints from complete, sentence-bounded grammatical signatures.

  A POS constraint never creates a dictionary sense. Core.LIFG.Attach may use
  this result only to prefer existing alternatives at the same token index.

  Bare adjacent verb-particle hypotheses are deliberately insufficient:
  a noun + adverb may form an apparent verb-particle signature without
  constituting an imperative or a complete predicate/object structure.
  """

  @spec required_pos(map(), non_neg_integer()) :: String.t() | nil
  def required_pos(si, index) when is_map(si) and is_integer(index) and index >= 0 do
    signatures = si |> Map.get(:sentence_signatures, []) |> List.wrap()

    Enum.find_value(signatures, fn
      %{id: :have_verb_to_infinitive, roles: roles} when is_map(roles) ->
        cond do
          Map.get(roles, :complement) == index -> "verb"
          Map.get(roles, :infinitive_marker) == index -> "particle"
          true -> nil
        end

      %{
        id: :verb_particle_construction,
        span: {first, last},
        roles: %{verb: index_role, object: object, particle: particle}
      }
      when index_role == index and first == index and last == particle and
             is_integer(object) and is_integer(particle) and
             object > index and object < particle ->
        # A dictionary-POS-based verb-particle hypothesis is not a resolved
        # parse. If the same surface is an attested subject noun phrase
        # followed by an independently attested finite-past predicate,
        # the supposed "object" is that predicate, not evidence strong
        # enough to *force* this earlier noun into verb POS.
        if competing_subject_past?(signatures, index, object),
          do: nil, else: "verb"

      _ ->
        nil
    end)
  end

  defp competing_subject_past?(signatures, noun_head, predicate_index) do
    subject? =
      Enum.any?(signatures, fn
        %{id: :noun_phrase, span: {determiner, ^noun_head},
          roles: %{determiner: determiner, head: ^noun_head}}
        when is_integer(determiner) and determiner < noun_head ->
          true
        _ ->
          false
      end)

    past_predicate? =
      Enum.any?(signatures, fn
        %{id: :finite_past_candidate,
          roles: %{verb: ^predicate_index}, evidence: evidence}
        when is_list(evidence) ->
          :simple_past_form_available in evidence
        _ ->
          false
      end)

    subject? and past_predicate?
  end

  def required_pos(_, _), do: nil
end
