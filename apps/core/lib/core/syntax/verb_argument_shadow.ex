defmodule Core.Syntax.VerbArgumentShadow do
  @moduledoc """
  Strict, experimental direct-object observation; NOT production parsing.

  Requires an attested verb POS, a narrowly recognizable subject + finite
  predicate frame, and an immediately adjacent nominal complement. A noun
  somewhere in the clause does not count as a direct object.

  Dictionary POS ambiguity is retained: outputs are hypotheses, never a
  syntactic proof or authorization to overwrite a Stage1 decision. This
  module is intentionally not wired into Core.Syntax.Signatures or Stage1.

  Db.BrainCell.status is a LIFECYCLE field, not a dictionary-sense validity
  bit. This observer intentionally never filters on "active"/"inactive" or
  changes a cell's status, activation, or persistence. Corpus-level lexical
  POS availability and per-turn cognitive activation are separate concerns.

  Only affirmative transitive observations are emitted. Failure to observe
  an object does NOT establish an intransitive construction.
  """

  @subject_pronouns MapSet.new(~w(i you he she it we they))
  @determiners MapSet.new(~w(a an the this that these those my your his her its our their))
  @auxiliaries MapSet.new(~w(will would can could should may might do does did))
  @prepositions MapSet.new(~w(from with of at on in into through by over under to for))
  @clause_boundaries MapSet.new(~w(and but because if while when although))
  @punctuation_boundaries MapSet.new([".", "!", "?", ";", ":", ","])
  @max_nominal_words 3

  @spec observe([map()], non_neg_integer(), [map()]) :: map()
  def observe(tokens, verb_index, cells)
      when is_list(tokens) and is_integer(verb_index) and verb_index >= 0 and
             is_list(cells) do
    nodes = index_tokens(tokens)
    lexicon = pos_lexicon(cells)

    verb = Map.get(nodes, verb_index)

    cond do
      is_nil(verb) ->
        abstain(:verb_token_missing)

      not pos?(lexicon, verb.word, "verb") ->
        abstain(:verb_pos_not_attested)

      true ->
        case clause_subject(nodes, verb_index, lexicon) do
          nil ->
            abstain(:subject_frame_unresolved)

          subject_index ->
            object_candidate(nodes, verb_index, subject_index, lexicon)
        end
    end
  end

  def observe(_, _, _), do: abstain(:invalid_input)

  defp object_candidate(nodes, verb_index, subject_index, lexicon) do
    first = Map.get(nodes, verb_index + 1)

    cond do
      is_nil(first) ->
        abstain(:no_following_complement)

      MapSet.member?(@prepositions, first.word) ->
        abstain(:following_prepositional_phrase_not_direct_object)

      MapSet.member?(@determiners, first.word) ->
        determiner_led_object(nodes, verb_index, subject_index, lexicon)

      pos?(lexicon, first.word, "noun") or pos?(lexicon, first.word, "proper_noun") ->
        bare_object(nodes, verb_index, subject_index, lexicon)

      true ->
        abstain(:no_confirmed_nominal_complement)
    end
  end

  defp determiner_led_object(nodes, verb_index, subject_index, lexicon) do
    start = verb_index + 2

    # Deliberately bounded; do not bridge punctuation, another clause,
    # conjunction, a preposition, or arbitrarily distant dictionary words.
    candidates =
      start..(start + @max_nominal_words - 1)
      |> Enum.reduce_while([], fn index, acc ->
        case Map.get(nodes, index) do
          %{word: word} ->
            if boundary?(word) or MapSet.member?(@determiners, word) do
              {:halt, Enum.reverse(acc)}
            else
              {:cont, [{index, word} | acc]}
            end

          _ ->
            {:halt, Enum.reverse(acc)}
        end
      end)

    noun_heads =
      Enum.filter(candidates, fn {_index, word} ->
        pos?(lexicon, word, "noun") or pos?(lexicon, word, "proper_noun")
      end)

    case List.last(noun_heads) do
      {head_index, _head} ->
        # Every intervening term must at least be lexically plausible as
        # a noun modifier. This may include false positives; it is a shadow
        # hypothesis, not an asserted parse.
        modifiers =
          Enum.filter(candidates, fn {index, _word} -> index < head_index end)

        plausible? =
          Enum.all?(modifiers, fn {_index, word} ->
            Enum.any?(["adjective", "noun", "proper_noun", "verb"], fn pos ->
              pos?(lexicon, word, pos)
            end)
          end)

        if plausible? do
          candidate(:determiner_nominal_object, :strong, subject_index, verb_index,
            head_index, verb_index + 1,
            [:attested_verb_pos, :bounded_subject_frame, :surface_determiner,
             :attested_nominal_head, :immediately_following_nominal])
        else
          abstain(:nominal_modifier_not_attested)
        end

      nil ->
        abstain(:determiner_without_attested_nominal_head)
    end
  end

  defp bare_object(nodes, verb_index, subject_index, _lexicon) do
    # A bare nominal next to a verb is weaker than a determiner-headed NP.
    # Constrain it to the next clause boundary or explicit PP.
    after_head = Map.get(nodes, verb_index + 2)

    if is_nil(after_head) or boundary?(after_head.word) or
         MapSet.member?(@prepositions, after_head.word) do
      candidate(:bare_nominal_object, :weak, subject_index, verb_index,
        verb_index + 1, nil,
        [:attested_verb_pos, :bounded_subject_frame, :attested_nominal_head,
         :bare_nominal_immediately_following_verb])
    else
      abstain(:bare_nominal_continuation_ambiguous)
    end
  end

  defp clause_subject(nodes, verb_index, lexicon) do
    preceding = Map.get(nodes, verb_index - 1)

    cond do
      is_nil(preceding) ->
        nil

      MapSet.member?(@subject_pronouns, preceding.word) ->
        verb_index - 1

      MapSet.member?(@auxiliaries, preceding.word) ->
        subject_ending_at(nodes, verb_index - 2, lexicon)

      true ->
        subject_ending_at(nodes, verb_index - 1, lexicon)
    end
  end

  defp subject_ending_at(nodes, index, lexicon) when index >= 0 do
    term = Map.get(nodes, index)
    determiner = Map.get(nodes, index - 1)

    cond do
      is_nil(term) ->
        nil

      MapSet.member?(@subject_pronouns, term.word) ->
        index

      pos?(lexicon, term.word, "noun") and
          not is_nil(determiner) and
          MapSet.member?(@determiners, determiner.word) ->
        index

      true ->
        nil
    end
  end

  defp subject_ending_at(_, _, _), do: nil

  defp candidate(frame, strength, subject, verb, head, determiner, evidence) do
    %{
      status: :candidate,
      frame_id: :verb_direct_object_hypothesis,
      construction: frame,
      strength: strength,
      roles: %{
        subject: subject,
        verb: verb,
        object_head: head,
        object_determiner: determiner
      },
      evidence: evidence,
      supports: :transitive,
      rejects_intransitive?: false,
      verified?: false
    }
  end

  defp abstain(reason) do
    %{
      status: :abstain,
      reason: reason,
      supports: nil,
      rejects_intransitive?: false,
      verified?: false
    }
  end

  defp boundary?(word) do
    MapSet.member?(@prepositions, word) or
      MapSet.member?(@clause_boundaries, word) or
      MapSet.member?(@punctuation_boundaries, word) or
      Regex.match?(~r/^[\p{P}\p{S}]$/u, word)
  end

  defp index_tokens(tokens) do
    tokens
    |> Enum.with_index()
    |> Enum.reduce(%{}, fn {token, fallback}, acc ->
      index = Map.get(token, :index, fallback)
      word = normalize(Map.get(token, :phrase))
      if is_integer(index) and index >= 0 and word != "" do
        Map.put_new(acc, index, %{index: index, word: word})
      else
        acc
      end
    end)
  end

  defp pos_lexicon(cells) do
    Enum.reduce(cells, %{}, fn row, acc ->
      word = normalize(Map.get(row, :norm) || Map.get(row, :word))
      pos = normalize(Map.get(row, :pos))

      if word != "" and pos != "" do
        Map.update(acc, word, MapSet.new([pos]), &MapSet.put(&1, pos))
      else
        acc
      end
    end)
  end

  defp pos?(lexicon, word, pos),
    do: MapSet.member?(Map.get(lexicon, word, MapSet.new()), pos)

  defp normalize(value) when is_binary(value),
    do: value |> String.trim() |> String.downcase()

  defp normalize(_), do: ""
end
