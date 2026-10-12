defmodule Core.MWE.SenseResolution do
  @moduledoc """
  Contextual ranking for dictionary-verified verb MWEs, including separated
  verb/object/particle constructions.

  Syntax proposes the construction. The lexicon supplies exact sense IDs.
  ContextFit supplies evidence from the sentence and a *selected* noun-object
  gloss. An ambiguous/unmatched phrase is never assigned a fabricated sense.

  Original base word LIFG choices, tokens, dictionary rows, and ATL grammatical
  roles remain unchanged.
  """

  alias Brain.LIFG.ContextFit
  alias Core.MWE.{Decisions, Lexicon}
  alias Core.Pipeline.Trace

  @min_context_score 0.12
  @min_margin 0.05
  @max_queries 12

  @doc """
  Rerank a canonical contiguous lexical verb MWE only when the dictionary
  gives positive, discriminating contextual evidence. Rebuild semantic_choices
  from the updated MWE choice; do not change raw LIFG unigram choices.
  """
  @spec rerank_contiguous(map()) :: map()
  def rerank_contiguous(%{mwe_choices: choices} = si) when is_list(choices) do
    units = List.wrap(Map.get(si, :mwe_lexicon))
    words = List.wrap(Map.get(si, :lifg_choices))

    {rewritten, changes} =
      Enum.map_reduce(choices, [], fn choice, notes ->
        span = Map.get(choice, :token_span)
        phrase = normalize(Map.get(choice, :phrase))

        unit =
          Enum.find(units, fn u ->
            Map.get(u, :span) == span and Map.get(u, :phrase) == phrase
          end)

        senses = if is_map(unit), do: List.wrap(Map.get(unit, :senses)), else: []
        object = contiguous_object(si, span, words)
        ranked = rank_senses(si, phrase, senses, object)

        case qualified_winner(ranked) do
          %{sense: sense, score: evidence_score}
          when is_map(choice) and is_binary(Map.get(choice, :id)) ->
            if Map.get(choice, :id) != Map.get(sense, :id) do
              updated =
                choice
                |> Map.put(:id, sense.id)
                |> Map.put(:lemma, phrase)
                |> Map.put(:pos, sense.pos)
                |> Map.put(:definition, Map.get(sense, :definition))
                |> Map.put(:score, max(number(Map.get(choice, :score)), evidence_score))
                |> Map.put(:context_score, evidence_score)
                |> Map.put(:source, :mwe_lexicon)

              {updated, [%{span: span, previous: choice.id, selected: sense.id} | notes]}
            else
              {Map.put(choice, :context_score, evidence_score), notes}
            end

          _ ->
            {choice, notes}
        end
      end)

    si
    |> Map.put(:mwe_choices, rewritten)
    |> Decisions.attach()
    |> maybe_trace_reselection(changes)
  end

  def rerank_contiguous(si), do: si

  @doc """
  Verify discontinuous verb+particle hypotheses against the real dictionary
  after ATL has identified their roles. This does not change word winners or
  claim that the separated pair was itself a contiguous dictionary token.

  Also records scored, already-selected contiguous lexical MWEs in the same
  evidence contract for downstream comprehension and response planning.
  """
  @spec attach_verified(map(), keyword()) :: map()
  def attach_verified(%{} = si, opts \\ []) when is_list(opts) do
    verified_contiguous = contiguous_meanings(si)
    candidates = separated_candidates(si)

    terms =
      candidates
      |> Enum.map(& &1.phrase)
      |> Enum.uniq()
      |> Enum.take(@max_queries)

    by_phrase = if terms == [], do: %{}, else: Lexicon.lookup_senses(terms, opts)

    separated =
      Enum.flat_map(candidates, fn candidate ->
        senses = Map.get(by_phrase, candidate.phrase, [])
        object = choice_at(si, candidate.object_index)
        ranked = rank_senses(si, candidate.phrase, senses, object)

        case qualified_winner(ranked) do
          %{sense: sense, score: context_score} ->
            [
              %{
                id: sense.id,
                phrase: candidate.phrase,
                definition: Map.get(sense, :definition),
                context_score: context_score,
                source: :verified_discontinuous_mwe,
                dictionary_verified?: true,
                verb_index: candidate.verb_index,
                particle_index: candidate.particle_index,
                object_index: candidate.object_index,
                constituent_indices: [candidate.verb_index, candidate.particle_index],
                span: candidate.span
              }
            ]

          _ ->
            []
        end
      end)

    constructions =
      (verified_contiguous ++ separated)
      |> Enum.uniq_by(&{&1.id, &1.constituent_indices})
      |> Enum.sort_by(&{&1.verb_index, &1.particle_index})

    si
    |> Map.put(:lexical_constructions, constructions)
    |> Trace.append(:mwe_sense_resolution,
      decision: if(constructions == [], do: :unresolved, else: :attached),
      reason: :verified_dictionary_context,
      meta: %{
        separated_hypotheses: length(candidates),
        verified_constructions: length(constructions),
        sense_ids: Enum.map(constructions, & &1.id)
      }
    )
  end

  def attach_verified(si, _opts), do: si

  defp separated_candidates(si) do
    hypotheses = List.wrap(Map.get(si, :mwe_hypotheses))

    constituents =
      si
      |> Map.get(:atl_slate)
      |> case do
        %{} = slate -> List.wrap(Map.get(slate, :constituents))
        _ -> []
      end

    si
    |> Map.get(:sentence_signatures)
    |> List.wrap()
    |> Enum.flat_map(fn
      %{
        id: :verb_particle_construction,
        span: span,
        roles: %{verb: verb, particle: particle, object: object}
      }
      when is_integer(verb) and is_integer(particle) and is_integer(object) and
             particle > verb + 1 ->
        confirmed_roles? =
          Enum.any?(constituents, fn
            %{id: :verb_particle_construction, span: ^span, status: :resolved} -> true
            _ -> false
          end)

        hypothesis =
          Enum.find(hypotheses, fn h ->
            Map.get(h, :category) == :phrasal_verb and
              Map.get(h, :constituent_indices) == [verb, particle] and
              Map.get(h, :particle_position) == :separated
          end)

        verb_choice = choice_at(si, verb)
        particle_choice = choice_at(si, particle)

        if confirmed_roles? and is_map(hypothesis) and
             pos?(verb_choice, "verb") and
             particle_pos?(particle_choice) do
          [
            %{
              phrase: normalize(Map.get(hypothesis, :phrase)),
              verb_index: verb,
              particle_index: particle,
              object_index: object,
              span: span
            }
          ]
        else
          []
        end

      _ ->
        []
    end)
    |> Enum.uniq_by(&{&1.verb_index, &1.particle_index})
  end

  defp contiguous_meanings(si) do
    units = List.wrap(Map.get(si, :mwe_lexicon))

    si
    |> Map.get(:semantic_choices)
    |> List.wrap()
    |> Enum.flat_map(fn
      %{kind: :mwe, source: :mwe_lexicon, id: id, token_span: {first, last}} = ch
      when is_binary(id) and is_integer(first) and is_integer(last) ->
        unit =
          Enum.find(units, fn u ->
            Map.get(u, :span) == {first, last} and
              Enum.any?(List.wrap(Map.get(u, :senses)), &(Map.get(&1, :id) == id))
          end)

        sense =
          if is_map(unit),
            do: Enum.find(List.wrap(Map.get(unit, :senses)), &(Map.get(&1, :id) == id)),
            else: nil

        phrase = if is_map(unit), do: Map.get(unit, :phrase), else: nil
        object = contiguous_object(si, {first, last}, List.wrap(Map.get(si, :lifg_choices)))
        score = if is_map(sense), do: context_score(si, phrase, sense, object), else: 0.0

        if is_map(sense) and score >= @min_context_score do
          [
            %{
              id: id,
              phrase: phrase,
              definition: Map.get(sense, :definition),
              context_score: score,
              source: :mwe_lexicon,
              dictionary_verified?: true,
              verb_index: first,
              particle_index: last,
              object_index: if(is_map(object), do: Map.get(object, :token_index), else: nil),
              constituent_indices: Enum.to_list(first..last),
              span: {first, last}
            }
          ]
        else
          []
        end

      _ ->
        []
    end)
  end

  defp contiguous_object(_si, {_, last}, choices) when is_integer(last) do
    Enum.find(choices, fn c ->
      index = Map.get(c, :token_index)
      is_integer(index) and index > last and index <= last + 3 and
        pos?(c, "noun")
    end)
  end

  defp contiguous_object(_, _, _), do: nil

  defp choice_at(si, index) when is_integer(index) do
    si
    |> Map.get(:lifg_choices)
    |> List.wrap()
    |> Enum.find(&(Map.get(&1, :token_index) == index))
  end

  defp choice_at(_, _), do: nil

  defp rank_senses(si, phrase, senses, object) do
    senses
    |> Enum.filter(&(Map.get(&1, :pos) == "verb"))
    |> Enum.map(fn sense ->
      %{sense: sense, score: context_score(si, phrase, sense, object)}
    end)
    |> Enum.sort_by(fn %{sense: sense, score: score} ->
      {-score, Map.get(sense, :id, "")}
    end)
  end

  defp qualified_winner([%{score: first} = top | rest]) do
    runner_up = case rest do [%{score: score} | _] -> score; _ -> 0.0 end

    if first >= @min_context_score and first - runner_up >= @min_margin,
      do: top,
      else: nil
  end

  defp qualified_winner(_), do: nil

  defp context_score(si, phrase, sense, object) do
    extra =
      if pos?(object, "noun") do
        [Map.get(object, :definition)]
      else
        []
      end

    ContextFit.score(Map.get(si, :sentence), phrase, sense, extra_context: extra)
  end

  defp maybe_trace_reselection(si, []), do: si

  defp maybe_trace_reselection(si, changes) do
    Trace.append(si, :mwe_sense_refinement,
      decision: :reranked,
      reason: :contextual_dictionary_evidence,
      meta: %{changes: Enum.reverse(changes)}
    )
  end

  defp pos?(%{id: id}, expected) when is_binary(id) do
    case String.split(id, "|", parts: 3) do
      [_lemma, ^expected, _sense] -> true
      _ -> false
    end
  end

  defp pos?(_, _), do: false

  defp particle_pos?(choice) do
    Enum.any?(["adverb", "preposition", "particle"], &pos?(choice, &1))
  end

  defp number(value) when is_number(value), do: value * 1.0
  defp number(_), do: 0.0

  defp normalize(value) when is_binary(value) do
    value
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/^\p{P}+|\p{P}+$/u, "")
    |> String.replace(~r/\s+/u, " ")
  end

  defp normalize(_), do: ""
end
