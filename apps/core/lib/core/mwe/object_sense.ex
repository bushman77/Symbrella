defmodule Core.MWE.ObjectSense do
  @moduledoc """
  A bounded, evidence-driven second pass for an ambiguous noun object inside
  a complete separated verb-particle construction.

  The governing verb and particle have already received real LIFG senses.
  Their dictionary definitions provide local semantic evidence when selecting
  among the *existing* dictionary noun senses of the object. Never create a
  sense or alter the token stream, grammar signatures, or top-level intent.

  Example: the verb sense of "power" describes an electronic device, whereas
  "computer|noun|1" denotes a historical human calculator. Other exact noun
  candidates may provide a better fit to that evidence.
  """

  alias Brain.LIFG.ContextFit
  alias Core.MWE.Decisions
  alias Core.Pipeline.Trace

  @min_fit 0.12
  @min_margin 0.05

  @spec refine(map()) :: map()
  def refine(%{lifg_choices: choices} = si) when is_list(choices) do
    refinements =
      si
      |> Map.get(:sentence_signatures, [])
      |> List.wrap()
      |> Enum.flat_map(fn
        %{id: :verb_particle_construction,
          span: {start, last},
          roles: %{verb: verb, object: object, particle: particle}}
        when is_integer(verb) and is_integer(object) and is_integer(particle) and
             start == verb and last == particle and verb < object and object < particle ->
          if verified_syntax_hypothesis?(si, verb, particle) do
            case rank_object(si, choices, verb, object, particle) do
              nil -> []
              updated -> [{object, updated}]
            end
          else
            []
          end

        _ ->
          []
      end)
      |> Enum.uniq_by(&elem(&1, 0))
      |> Map.new()

    if map_size(refinements) == 0 do
      si
    else
      updated =
        Enum.map(choices, fn choice ->
          Map.get(refinements, Map.get(choice, :token_index), choice)
        end)

      si
      |> Map.put(:lifg_choices, updated)
      |> Decisions.attach()
      |> Trace.append(:object_sense_refinement,
        decision: :reranked,
        reason: :governing_verb_particle_dictionary_context,
        meta: %{
          changes:
            Enum.map(refinements, fn {index, choice} ->
              %{token_index: index, id: choice.id, contextual_score: choice.context_score}
            end)
        }
      )
    end
  end

  def refine(si), do: si

  defp verified_syntax_hypothesis?(si, verb, particle) do
    si
    |> Map.get(:mwe_hypotheses, [])
    |> List.wrap()
    |> Enum.any?(fn h ->
      Map.get(h, :category) == :phrasal_verb and
        Map.get(h, :constituent_indices) == [verb, particle] and
        Map.get(h, :particle_position) == :separated
    end)
  end

  defp rank_object(si, choices, verb_index, object_index, particle_index) do
    verb = at(choices, verb_index)
    object = at(choices, object_index)
    particle = at(choices, particle_index)

    if pos?(verb, "verb") and pos?(object, "noun") and particle_pos?(particle) and
         valid_gloss?(verb) do
      term = object_surface(si, object_index)
      candidates = object_candidates(si, object_index, term, object)

      ranked =
        candidates
        |> Enum.map(fn candidate ->
          score =
            ContextFit.score(
              Map.get(si, :sentence),
              term,
              candidate,
              extra_context: [field(verb, :definition), field(particle, :definition)],
              related_morphology?: true
            )

          %{candidate: candidate, score: score}
        end)
        |> Enum.sort_by(fn %{candidate: c, score: score} ->
          {-score, field(c, :id) || ""}
        end)

      case ranked do
        [%{candidate: best, score: first} | rest] when first >= @min_fit ->
          second =
            case rest do
              [%{score: value} | _] -> value
              _ -> 0.0
            end

          if first - second >= @min_margin and
               field(best, :id) != field(object, :id) do
            object
            |> Map.put(:id, field(best, :id))
            |> Map.put(:lemma, field(best, :norm) || term)
            |> Map.put(:pos, "noun")
            |> Map.put(:definition, field(best, :definition))
            |> Map.put(:example, field(best, :example))
            |> Map.put(:source, field(best, :source) || :active_cells)
            |> Map.put(:context_score, first)
            |> Map.put(:selection_scope, :contextual_object_sense)
          end

        _ ->
          nil
      end
    end
  end

  defp rank_object(_, _, _, _, _), do: nil

  defp object_candidates(si, index, term, selected) do
    bucket =
      case Map.get(si, :sense_candidates, %{}) do
        %{} = buckets ->
          Map.get(buckets, index) || Map.get(buckets, Integer.to_string(index)) || []
        _ ->
          []
      end

    (List.wrap(bucket) ++ List.wrap(Map.get(si, :active_cells)) ++ [selected])
    |> Enum.filter(&is_map/1)
    |> Enum.filter(fn candidate ->
      id = field(candidate, :id)
      case is_binary(id) && String.split(id, "|", parts: 3) do
        [lemma, "noun", _sense] ->
          normalize(lemma) == term and
            valid_gloss?(candidate)
        _ -> false
      end
    end)
    |> Enum.uniq_by(&field(&1, :id))
    |> Enum.take(100)
  end

  defp at(choices, index) do
    Enum.find(choices, &(Map.get(&1, :token_index) == index))
  end

  defp object_surface(si, index) do
    si
    |> Map.get(:tokens, [])
    |> List.wrap()
    |> Enum.find(&(Map.get(&1, :index) == index))
    |> case do
      %{} = token -> normalize(Map.get(token, :phrase))
      _ -> ""
    end
  end

  defp valid_gloss?(candidate), do: is_binary(field(candidate, :definition)) and
    String.trim(field(candidate, :definition)) != ""

  defp pos?(candidate, desired) when is_map(candidate) do
    case field(candidate, :id) do
      id when is_binary(id) ->
        case String.split(id, "|", parts: 3) do
          [_lemma, ^desired, _sense] -> true
          _ -> false
        end

      _ -> false
    end
  end

  defp pos?(_, _), do: false

  defp particle_pos?(choice) do
    Enum.any?(["adverb", "particle", "preposition"], &pos?(choice, &1))
  end

  defp field(%{} = map, key), do: Map.get(map, key) || Map.get(map, Atom.to_string(key))
  defp field(_, _), do: nil

  defp normalize(value) when is_binary(value) do
    value
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/^\p{P}+|\p{P}+$/u, "")
    |> String.replace(~r/\s+/u, " ")
  end

  defp normalize(_), do: ""
end
