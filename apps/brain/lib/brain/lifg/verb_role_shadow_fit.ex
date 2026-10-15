defmodule Brain.LIFG.VerbRoleShadowFit do
  @moduledoc """
  Pure shadow-only, candidate-specific lexical compatibility for an explicitly
  observed "verb OBJECT from SOURCE" construction.

  A role match requires the exact ordered object -> from -> source pattern
  inside ONE dictionary sense definition, not a sentence-wide keyword bag.
  The preposition and object must also be structurally attested at their
  original sentence positions. Only explicit transitive senses are eligible.

  All scores are diagnostic heuristics, NOT calibrated probabilities or
  production Stage1 choices. No fields of any BrainCell are ever changed.
  """

  alias Brain.LIFG.VerbArgumentShadowScoring, as: Baseline

  @role_bonus 0.10
  @determiners MapSet.new(~w(a an the this that these those my your his her its our their))
  @token_re ~r/[\p{L}\p{N}]+(?:['’][\p{L}\p{N}]+)*|[.!?;,:]/u

  @spec score(String.t(), String.t(), non_neg_integer(), [map()], map(), map()) :: map()
  def score(sentence, target, verb_index, cells, argument, role)
      when is_binary(sentence) and is_binary(target) and
             is_integer(verb_index) and verb_index >= 0 and
             is_list(cells) and is_map(argument) and is_map(role) do
    baseline = Baseline.score(sentence, target, verb_index, cells, argument)
    source = validated_source(sentence, verb_index, argument, role, baseline)
    by_id = Map.new(cells, &{Map.get(&1, :id), &1})

    candidates =
      Enum.map(baseline.candidates, fn row ->
        cell = Map.fetch!(by_id, row.id)

        hits? =
          not is_nil(source) and
            is_list(Map.get(cell, :gram_function)) and
            "transitive" in cell.gram_function and
            exact_role_relation?(Map.get(cell, :definition), source.object, source.source)

        role_bonus = if hits?, do: @role_bonus, else: 0.0

        row
        |> Map.put(:role_bonus, role_bonus)
        |> Map.put(:role_evidence,
          if(hits?, do: %{object: source.object, preposition: "from", source: source.source}, else: nil))
        |> Map.put(:combined, Float.round(min(1.0, row.combined + role_bonus), 6))
      end)
      |> Enum.sort_by(&{-&1.combined, &1.id})

    %{
      source: :shadow_only,
      role_observation_accepted?: not is_nil(source),
      role_bonus_cap: @role_bonus,
      role_adjusted_senses: Enum.count(candidates, &(&1.role_bonus > 0.0)),
      candidates: candidates,
      candidate_count: length(candidates),
      activated_senses: 0,
      production_winners_changed?: false
    }
  end

  def score(_, _, _, _, _, _), do: %{
    source: :shadow_only,
    role_observation_accepted?: false,
    role_bonus_cap: @role_bonus,
    role_adjusted_senses: 0,
    candidates: [],
    candidate_count: 0,
    activated_senses: 0,
    production_winners_changed?: false
  }

  # Keep every constraint, including token indices, surface words and
  # independent object evidence, before allowing any candidate score change.
  defp validated_source(
         sentence,
         verb_index,
         %{status: :candidate, roles: %{verb: verb_index, object_head: object_index}},
         %{status: :candidate,
           frame_id: :verb_object_source_hypothesis,
           roles: %{verb: verb_index, object_head: object_index,
                    source_preposition: prep_index, source_head: source_index,
                    source_determiner: determiner_index},
           words: %{object: object, preposition: "from", source: source},
           evidence: evidence},
         %{observation_strength: strength}
       )
       when strength in [:weak, :strong] and is_list(evidence) and
              is_integer(object_index) and is_integer(prep_index) and
              is_integer(source_index) and is_binary(object) and is_binary(source) do
    tokens = Regex.scan(@token_re, String.downcase(sentence)) |> Enum.map(&hd/1)

    required? =
      Enum.all?(
        [:attested_direct_object, :source_phrase_adjacent_to_object,
         :explicit_from_preposition, :attested_nominal_source],
        &(&1 in evidence)
      )

    determiner_ok? =
      case determiner_index do
        nil ->
          source_index == prep_index + 1

        index when is_integer(index) ->
          index == prep_index + 1 and source_index == index + 1 and
            MapSet.member?(@determiners, Enum.at(tokens, index))

        _ ->
          false
      end

    # Object and source are tied to exact token positions; never cross a
    # punctuation mark or a new clause by deleting non-lexical input tokens.
    sequence? =
      prep_index == object_index + 1 and
        Enum.at(tokens, verb_index) != nil and
        Enum.at(tokens, object_index) == object and
        Enum.at(tokens, prep_index) == "from" and
        Enum.at(tokens, source_index) == source and
        String.length(object) >= 3 and String.length(source) >= 3 and
        determiner_ok?

    if required? and sequence?, do: %{object: object, source: source}, else: nil
  end

  defp validated_source(_, _, _, _, _), do: nil

  # Exact ordered relation, optionally with a single determiner, within the
  # SAME gloss. "A well contains water" is not "water from a well".
  defp exact_role_relation?(definition, object, source)
       when is_binary(definition) and is_binary(object) and is_binary(source) do
    terms =
      Regex.scan(~r/[\p{L}\p{N}]+(?:['’][\p{L}\p{N}]+)*/u, String.downcase(definition))
      |> Enum.map(&hd/1)

    terms
    |> Enum.chunk_every(4, 1, :discard)
    |> Enum.any?(fn
      [^object, "from", determiner, ^source] -> MapSet.member?(@determiners, determiner)
      _ -> false
    end) or
      (terms
       |> Enum.chunk_every(3, 1, :discard)
       |> Enum.any?(fn
         [^object, "from", ^source] -> true
         _ -> false
       end))
  end

  defp exact_role_relation?(_, _, _), do: false
end
