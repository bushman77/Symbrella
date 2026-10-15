defmodule Core.Syntax.VerbRoleShadow do
  @moduledoc """
  Read-only structural observation of an object plus a local "from" source.

  This is deliberately stricter than a sentence-wide keyword match:
  the prepositional phrase must BEGIN immediately after the attested direct
  object's head, and it must contain a nearby attested nominal source head.

  Dictionary senses are used irrespective of BrainCell lifecycle status.
  The source reading is a hypothesis, not a resolved semantic role. No
  Core signature, Stage1 decision, memory, or DB record is modified.
  """

  alias Core.Syntax.VerbArgumentShadow

  @determiners MapSet.new(~w(a an the this that these those my your his her its our their))

  @doc """
  Observe a local V + object + "from" + nominal-source construction.

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
      from = Map.get(by_index, object_index + 1)

      cond do
        is_nil(object) or is_nil(from) ->
          abstain(:no_adjacent_source_phrase, argument)

        from.word != "from" ->
          abstain(:no_adjacent_from_preposition, argument)

        true ->
          source_candidate(by_index, object, from, verb_index, cells, argument)
      end
    else
      abstain(:direct_object_unresolved, argument)
    end
  end

  def observe(_, _, _), do: abstain(:invalid_input, nil)

  defp source_candidate(by_index, object, from, verb_index, cells, argument) do
    first = Map.get(by_index, from.index + 1)

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
      is_nil(head) ->
        abstain(:source_head_missing, argument)

      not noun_attested?(head.word, cells) ->
        abstain(:source_head_not_lexically_attested, argument)

      true ->
        %{
          status: :candidate,
          frame_id: :verb_object_source_hypothesis,
          construction: :verb_object_from_nominal_source,
          roles: %{
            verb: verb_index,
            object_head: object.index,
            source_preposition: from.index,
            source_head: head.index,
            source_determiner: determiner_index
          },
          words: %{object: object.word, preposition: "from", source: head.word},
          evidence: [
            :attested_direct_object,
            :source_phrase_adjacent_to_object,
            :explicit_from_preposition,
            :attested_nominal_source
          ],
          source_relation_verified?: false,
          rejects_intransitive?: false,
          verified?: false
        }
    end
  end

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
