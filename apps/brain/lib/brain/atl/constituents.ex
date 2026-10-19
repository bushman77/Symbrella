defmodule Brain.ATL.Constituents do
  @moduledoc """
  Pure, bounded projection of Core sentence signatures onto ATL's resolved
  semantic winners.

  This supplements (never replaces) ATL's by_norm and by_id grouping. A
  grammatical role is resolved only when an existing, non-fallback selected
  sense at that original token index satisfies its POS constraints. A phrase
  hypothesis is not a new BrainCell or a selected MWE sense.

  Shadowed child words of a canonical lexical MWE remain unresolved rather
  than being assigned their phrase winner's ID.
  """

  @max_constituents 48
  @ids [:noun_phrase, :possessive_noun_phrase, :prepositional_phrase,
        :verb_particle_construction, :wh_aux_topic_question, :wh_aux_object_question]

  @spec attach(map(), list()) :: map()
  def attach(%{winners: winners} = slate, signatures)
      when is_list(winners) and is_list(signatures) do
    by_index =
      winners
      |> Enum.filter(fn
        %{token_index: i, id: id}
        when is_integer(i) and i >= 0 and is_binary(id) -> true
        _ -> false
      end)
      |> Enum.group_by(& &1.token_index)

    constituents =
      signatures
      |> Enum.filter(&valid_signature?/1)
      |> Enum.take(@max_constituents)
      |> Enum.map(&resolve(&1, by_index))

    slate
    |> Map.put(:constituents, constituents)
    |> Map.put(:constituent_count, length(constituents))
  end

  def attach(slate, _signatures), do: slate

  defp valid_signature?(%{id: id, span: {first, last}, roles: roles})
       when id in @ids and is_integer(first) and first >= 0 and
              is_integer(last) and last >= first and is_map(roles) and
              map_size(roles) > 0 do
    Enum.all?(roles, fn
      {role, i} when is_atom(role) and is_integer(i) -> i >= first and i <= last
      _ -> false
    end)
  end

  defp valid_signature?(_), do: false

  defp resolve(signature, by_index) do
    required = Map.get(signature, :required_features, %{})

    {roles, unresolved} =
      signature.roles
      |> Enum.sort_by(fn {role, _index} -> Atom.to_string(role) end)
      |> Enum.reduce({%{}, []}, fn {role, index}, {resolved, missing} ->
        features = if is_map(required), do: Map.get(required, role, []), else: []

        winner =
          by_index
          |> Map.get(index, [])
          |> Enum.find(&compatible_choice?(&1, features))

        if winner do
          entry =
            %{
              token_index: index,
              sense_id: winner.id,
              norm: winner.norm
            }
            |> maybe_mark_phrase_winner(Map.get(winner, :raw))

          {Map.put(resolved, role, entry), missing}
        else
          {resolved, [role | missing]}
        end
      end)

    unresolved = unresolved |> Enum.reverse() |> Enum.sort()

    status =
      cond do
        unresolved == [] -> :resolved
        map_size(roles) > 0 -> :partially_resolved
        true -> :unresolved
      end

    %{
      id: signature.id,
      span: signature.span,
      roles: roles,
      unresolved_roles: unresolved,
      status: status,
      source: :sentence_signature
    }
  end

  # The MWE's lexical choice may be anchored at the verb's original index.
  # Keep its true phrase scope explicit rather than presenting it as a
  # unigram sense at that index.
  defp maybe_mark_phrase_winner(entry, %{kind: :mwe, token_span: {first, last}})
       when is_integer(first) and is_integer(last) and last >= first do
    Map.merge(entry, %{selection_scope: :lexical_mwe, token_span: {first, last}})
  end

  defp maybe_mark_phrase_winner(entry, _), do: entry

  defp compatible_choice?(%{id: id}, features) when is_binary(id) do
    if String.ends_with?(id, "|fallback") do
      false
    else
      pos = pos_from_id(id)

      case List.wrap(features) do
        [] -> pos != nil
        required -> Enum.any?(required, &matches_pos?(&1, pos))
      end
    end
  end

  defp compatible_choice?(_, _), do: false

  defp matches_pos?(:pos_noun_or_pronoun, pos),
    do: pos in ["noun", "proper_noun", "pronoun"]

  defp matches_pos?(:pos_noun, pos), do: pos in ["noun", "proper_noun"]
  defp matches_pos?(:pos_verb, "verb"), do: true
  defp matches_pos?(:pos_auxiliary, "auxiliary"), do: true
  defp matches_pos?(:pos_modal, "modal"), do: true
  defp matches_pos?(:pos_pronoun, "pronoun"), do: true
  defp matches_pos?(:pos_particle, "particle"), do: true
  defp matches_pos?(:pos_adverb, "adverb"), do: true
  defp matches_pos?(:pos_preposition, "preposition"), do: true
  defp matches_pos?(:pos_determiner, "determiner"), do: true
  defp matches_pos?(:pos_adjective, "adjective"), do: true
  defp matches_pos?(_, _), do: false

  defp pos_from_id(id) when is_binary(id) do
    case String.split(id, "|", parts: 3) do
      [_lemma, pos, _sense] -> pos
      _ -> nil
    end
  end
end
