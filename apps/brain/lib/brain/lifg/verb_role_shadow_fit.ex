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
  alias Brain.LIFG.RoleGlossCompatibility

  @role_bonus 0.10
  @determiners MapSet.new(~w(a an the this that these those my your his her its our their))
  @token_re ~r/[\p{L}\p{N}]+(?:['’][\p{L}\p{N}]+)*|[\p{P}\p{S}]/u

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

        gloss_match =
          if not is_nil(source) and
               is_list(Map.get(cell, :gram_function)) and
               "transitive" in cell.gram_function do
            RoleGlossCompatibility.match(Map.get(cell, :definition), source)
          else
            :no_match
          end

        role_bonus = if match?({:ok, _}, gloss_match), do: @role_bonus, else: 0.0

        row
        |> Map.put(:role_bonus, role_bonus)
        |> Map.put(:role_evidence, if(role_bonus > 0.0, do: source, else: nil))
        |> Map.put(:role_match, case gloss_match do
          {:ok, trace} -> trace
          :no_match -> nil
        end)
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

  # Verify structure against the ORIGINAL sentence with all punctuation.
  # Index labels and words supplied by another component are not trusted alone.
  defp validated_source(
         sentence,
         verb_index,
         %{status: :candidate, roles: %{verb: verb_index, object_head: object_index}},
         %{status: :candidate,
           frame_id: :verb_object_source_hypothesis,
           roles: %{verb: verb_index, object_head: object_index,
                    source_preposition: prep_index, source_head: source_index,
                    source_determiner: determiner_index} = roles,
           words: %{object: object, preposition: marker, source: source},
           evidence: evidence},
         %{observation_strength: strength}
       )
       when strength in [:weak, :strong] and is_list(evidence) and
              is_integer(object_index) and is_integer(prep_index) and
              is_integer(source_index) and is_binary(object) and
              is_binary(source) and marker in ["from", "out of"] do
    tokens = Regex.scan(@token_re, String.downcase(sentence)) |> Enum.map(&hd/1)
    marker_end = Map.get(roles, :source_marker_end, prep_index)

    required? =
      Enum.all?(
        [:attested_direct_object, :source_phrase_adjacent_to_object,
         :attested_nominal_source],
        &(&1 in evidence)
      )

    marker_ok? =
      case marker do
        "from" ->
          marker_end == prep_index and
            Enum.at(tokens, prep_index) == "from" and
            :explicit_from_preposition in evidence

        "out of" ->
          marker_end == prep_index + 1 and
            Enum.at(tokens, prep_index) == "out" and
            Enum.at(tokens, marker_end) == "of" and
            :explicit_out_of_source_marker in evidence
      end

    determiner_ok? =
      case determiner_index do
        nil ->
          source_index == marker_end + 1

        index when is_integer(index) ->
          index == marker_end + 1 and source_index == index + 1 and
            MapSet.member?(@determiners, Enum.at(tokens, index))

        _ ->
          false
      end

    sequence? =
      object_index > verb_index and
        prep_index == object_index + 1 and
        Enum.at(tokens, verb_index) == String.downcase(target_for_verb(sentence, verb_index)) and
        Enum.at(tokens, object_index) == object and
        Enum.at(tokens, source_index) == source and
        String.length(object) >= 3 and String.length(source) >= 3 and
        marker_ok? and determiner_ok?

    if required? and sequence? do
      %{object: object, source: source, preposition: marker}
    else
      nil
    end
  end

  defp validated_source(_, _, _, _, _), do: nil

  # Do not infer the lemma here. The reported verb is tied to a real sentence
  # token; its lexical sense set was already filtered by the caller's target.
  defp target_for_verb(sentence, index) do
    sentence
    |> String.downcase()
    |> then(&Regex.scan(@token_re, &1))
    |> Enum.map(&hd/1)
    |> Enum.at(index, "")
  end
end
