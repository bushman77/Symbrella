defmodule Core.Syntax.VerbRoleShadow do
  @moduledoc """
  Read-only structural observation of an object plus a local "from"/"out of" source.

  This is deliberately stricter than a sentence-wide keyword match:
  the prepositional phrase must BEGIN immediately after the attested direct
  object's head, and it must contain a nearby attested nominal source head.

  Dictionary senses are used irrespective of BrainCell lifecycle status.
  The source reading is a hypothesis, not a resolved semantic role. No
  Core signature, Stage1 decision, memory, or DB record is modified.
  """

  alias Core.Syntax.VerbArgumentShadow

  @determiners MapSet.new(~w(a an the this that these those my your his her its our their))
  @punctuation MapSet.new([".", "!", "?", ";", ":", ","])

  @doc """
  Observe a local V + object + ("from" or "out of") + nominal-source construction.

  Tokens MUST preserve punctuation as indexed tokens, not silently remove
  punctuation and join two sentences. The caller supplies lexical POS senses
  from LTM, not just senses with lifecycle status "active".
  """
  @spec observe([map()], non_neg_integer(), [map()]) :: map()
  def observe(tokens, verb_index, cells)
      when is_list(tokens) and is_integer(verb_index) and verb_index >= 0 and
             is_list(cells) do
    argument = VerbArgumentShadow.observe(tokens, verb_index, cells)
    by_index = index_tokens(tokens)

    if argument.status == :candidate do
      object_index = argument.roles.object_head
      object = Map.get(by_index, object_index)
      marker = Map.get(by_index, object_index + 1)

      cond do
        is_nil(object) or is_nil(marker) ->
          abstain(:no_adjacent_source_phrase, argument)

        marker.word == "from" ->
          source_candidate(by_index, object, marker, marker, "from",
            verb_index, cells, argument)

        marker.word == "out" and
            match?(%{word: "of"}, Map.get(by_index, marker.index + 1)) ->
          marker_end = Map.fetch!(by_index, marker.index + 1)

          source_candidate(by_index, object, marker, marker_end, "out of",
            verb_index, cells, argument)

        true ->
          abstain(:no_adjacent_from_preposition, argument)
      end
    else
      abstain(:direct_object_unresolved, argument)
    end
  end

  def observe(_, _, _), do: abstain(:invalid_input, nil)

  defp source_candidate(by_index, object, marker, marker_end, marker_text,
         verb_index, cells, argument) do
    first = Map.get(by_index, marker_end.index + 1)

    {head, determiner_index} =
      cond do
        is_nil(first) ->
          {nil, nil}

        MapSet.member?(@determiners, first.word) ->
          {Map.get(by_index, first.index + 1), first.index}

        true ->
          {first, nil}
      end

    cond do
      is_nil(head) or MapSet.member?(@punctuation, head.word) or
          (not is_nil(head) and Regex.match?(~r/^[\p{P}\p{S}]$/u, head.word)) ->
        abstain(:source_head_missing, argument)

      not noun_attested?(head.word, cells) ->
        abstain(:source_head_not_lexically_attested, argument)

      true ->
        %{
          status: :candidate,
          frame_id: :verb_object_source_hypothesis,
          construction: :verb_object_from_nominal_source,
          roles:
            %{
              verb: verb_index,
              object_head: object.index,
              source_preposition: marker.index,
              source_head: head.index,
              source_determiner: determiner_index
            }
            |> maybe_marker_end(marker.index, marker_end.index),
          words: %{object: object.word, preposition: marker_text, source: head.word},
          evidence: [
            :attested_direct_object,
            :source_phrase_adjacent_to_object,
            if(marker_text == "from", do: :explicit_from_preposition,
              else: :explicit_out_of_source_marker),
            :attested_nominal_source
          ],
          source_relation_verified?: false,
          rejects_intransitive?: false,
          verified?: false
        }
    end
  end

  # Keep the original shape unchanged for "from" (existing audit contracts).
  defp maybe_marker_end(roles, start_index, start_index), do: roles
  defp maybe_marker_end(roles, _start_index, end_index),
    do: Map.put(roles, :source_marker_end, end_index)

  defp noun_attested?(word, cells) do
    Enum.any?(cells, fn
      %{norm: norm, pos: pos} when pos in ["noun", "proper_noun"] ->
        normalize(norm) == word

      _ ->
        false
    end)
  end

  defp index_tokens(tokens) do
    tokens
    |> Enum.with_index()
    |> Enum.reduce(%{}, fn
      {%{phrase: phrase} = tok, fallback}, acc when is_binary(phrase) ->
        index = Map.get(tok, :index, fallback)
        word = normalize(phrase)

        if is_integer(index) and index >= 0 and word != "" and
             Map.get(tok, :n, 1) == 1 do
          Map.put_new(acc, index, %{index: index, word: word})
        else
          acc
        end

      _, acc ->
        acc
    end)
  end

  defp normalize(value) when is_binary(value), do: value |> String.trim() |> String.downcase()
  defp normalize(_), do: ""

  defp abstain(reason, argument) do
    %{
      status: :abstain,
      reason: reason,
      argument_status: if(is_map(argument), do: argument.status, else: nil),
      source_relation_verified?: false,
      rejects_intransitive?: false,
      verified?: false
    }
  end
end
