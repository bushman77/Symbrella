defmodule Core.MWE.Decisions do
  @moduledoc """
  Build a non-overlapping semantic choice view after LIFG.

  Original tokens and word-level :lifg_choices remain unchanged.
  Dictionary-backed :mwe_choices are promoted only when their lexical ID,
  source and claimed token span agree with the underlying words.
  The derived :semantic_choices anchor each phrase at its first word index.
  """

  alias Core.MWE.GrammarPrecedence

  @spec attach(map()) :: map()
  def attach(%{lifg_choices: words, tokens: tokens} = si)
      when is_list(words) and is_list(tokens) do
    {accepted, covered} =
      si
      |> Map.get(:mwe_choices, [])
      |> List.wrap()
      |> Enum.filter(&valid_phrase_choice?(&1, tokens, si))
      |> Enum.sort_by(fn choice ->
        {first, last} = choice.token_span
        {-(last - first + 1), first, choice.id}
      end)
      |> Enum.reduce({[], MapSet.new()}, fn choice, {acc, occupied} ->
        {first, last} = choice.token_span
        indices = MapSet.new(first..last)

        if MapSet.disjoint?(occupied, indices) do
          semantic_choice =
            choice
            |> Map.put(:token_index, first)
            |> Map.put(:kind, :mwe)

          {[semantic_choice | acc], MapSet.union(occupied, indices)}
        else
          {acc, occupied}
        end
      end)

    visible_words =
      Enum.reject(words, fn choice ->
        index = Map.get(choice, :token_index)
        is_integer(index) and MapSet.member?(covered, index)
      end)

    semantic_choices =
      (visible_words ++ Enum.reverse(accepted))
      |> Enum.sort_by(fn choice ->
        index = Map.get(choice, :token_index)
        {if(is_integer(index), do: index, else: -1), Map.get(choice, :id, "")}
      end)

    si
    |> Map.put(:semantic_choices, semantic_choices)
    |> maybe_trace_promotions(tokens, accepted)
  end

  def attach(other), do: other

  # Record the *actual* lexical promotion gate outcomes, with no scoring
  # changes. ATL can subsequently mark a grammatical role unresolved, but
  # that observation cannot retroactively rewrite these already-set winners.
  defp maybe_trace_promotions(si, tokens, accepted) do
    opts = Map.get(si, :lifg_opts) || []

    if is_list(opts) and Keyword.get(opts, :trace_mwe_promotion?, false) == true do
      candidates =
        si
        |> Map.get(:mwe_choices, [])
        |> List.wrap()
        |> Enum.take(12)
        |> Enum.map(fn choice ->
          span = Map.get(choice, :token_span)
          phrase = Map.get(choice, :phrase)
          pos = Map.get(choice, :pos)
          {verb_conflict?, question_conflict?} =
            if is_tuple(span) and tuple_size(span) == 2 do
              {
                GrammarPrecedence.incompatible_verb_mwe?(si, span, pos),
                GrammarPrecedence.incompatible_question_mwe?(si, phrase || "", pos)
              }
            else
              {false, false}
            end

          %{
            id: Map.get(choice, :id),
            phrase: phrase,
            span: span,
            score: Map.get(choice, :score),
            source: Map.get(choice, :source),
            lexical_gate_passed?: valid_phrase_choice?(choice, tokens, si),
            verb_frame_conflict?: verb_conflict?,
            question_conflict?: question_conflict?,
            promoted?: Enum.any?(accepted, &(&1.id == Map.get(choice, :id) and
                                              &1.token_span == span))
          }
        end)

      prior_count =
        si
        |> Map.get(:trace, [])
        |> List.wrap()
        |> Enum.count(fn
          %{stage: :mwe_promotion_audit} -> true
          _ -> false
        end)

      Core.Pipeline.Trace.append(si, :mwe_promotion_audit,
        decision: :observed,
        reason: :lexical_gate_and_nonoverlapping_selection,
        meta: %{pass: prior_count + 1, candidates: candidates}
      )
    else
      si
    end
  end

  defp valid_phrase_choice?(%{
         id: id,
         phrase: phrase,
         token_span: {first, last},
         source: source
       } = choice, tokens, si)
       when is_binary(id) and is_binary(phrase) and is_integer(first) and
              is_integer(last) and is_list(tokens) do
    surface =
      tokens
      |> Enum.slice(first, last - first + 1)
      |> Enum.map(&Map.get(&1, :phrase, ""))
      |> Enum.join(" ")

    first >= 0 and last >= first and last < length(tokens) and
      source in [:mwe_lexicon, "mwe_lexicon"] and
      not String.ends_with?(id, "|fallback") and
      normalize(surface) == normalize(phrase) and
      id_lemma_matches?(id, phrase) and
      not GrammarPrecedence.incompatible_verb_mwe?(
        si, {first, last}, Map.get(choice, :pos)
      ) and
      not GrammarPrecedence.incompatible_question_mwe?(
        si, phrase, Map.get(choice, :pos)
      )
  end

  defp valid_phrase_choice?(_, _, _), do: false

  defp id_lemma_matches?(id, phrase) do
    case String.split(id, "|", parts: 2) do
      [lemma, _sense] -> normalize(lemma) == normalize(phrase)
      _ -> false
    end
  end

  defp normalize(value) when is_binary(value) do
    value
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/\s+/u, " ")
  end
end
