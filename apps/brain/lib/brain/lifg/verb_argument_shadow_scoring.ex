defmodule Brain.LIFG.VerbArgumentShadowScoring do
  @moduledoc """
  Pure, SHADOW-ONLY combination of existing ContextFit with a bounded
  positive hint from Core.Syntax.VerbArgumentShadow.

  This is NOT plugged into Stage1, does not recalibrate probabilities, and
  does not treat a candidate argument frame as syntactic ground truth.
  Only dictionary senses explicitly tagged "transitive" can receive a bonus.
  Missing argument evidence does not establish intransitivity; competing
  senses are never removed, penalized, reactivated or persisted.

  This module intentionally does not depend on Core or query the database.
  The caller supplies a proposed observation and selected sense cells.
  """

  alias Brain.LIFG.ContextFit

  @strong_bonus 0.12
  @weak_bonus 0.06

  @spec score(String.t(), String.t(), non_neg_integer(), [map()], map()) :: map()
  def score(sentence, target, verb_index, cells, observation)
      when is_binary(sentence) and is_binary(target) and
             is_integer(verb_index) and verb_index >= 0 and
             is_list(cells) and is_map(observation) do
    strength = supported_strength(observation, verb_index)
    bonus = case strength do
      :strong -> @strong_bonus
      :weak -> @weak_bonus
      _ -> 0.0
    end

    rows =
      cells
      |> Enum.filter(&matching_verb?(&1, target))
      |> Enum.map(fn cell ->
        lexical = ContextFit.score(sentence, target, cell)
        grammar = if transitive?(cell), do: bonus, else: 0.0

        %{
          id: cell.id,
          lexical: lexical,
          grammar_bonus: grammar,
          combined: Float.round(min(1.0, lexical + grammar), 6),
          grammar: Map.get(cell, :gram_function, []),
          status: Map.get(cell, :status)
        }
      end)
      |> Enum.sort_by(&{-&1.combined, &1.id})

    %{
      source: :shadow_only,
      observation_strength: strength,
      candidates: rows,
      candidate_count: length(rows),
      adjusted_senses: Enum.count(rows, &(&1.grammar_bonus > 0.0)),
      activated_senses: 0,
      production_winners_changed?: false
    }
  end

  def score(_, _, _, _, _), do: %{
    source: :shadow_only,
    observation_strength: nil,
    candidates: [],
    candidate_count: 0,
    adjusted_senses: 0,
    activated_senses: 0,
    production_winners_changed?: false
  }

  defp matching_verb?(%{id: id, pos: "verb"}, target)
       when is_binary(id) and is_binary(target),
       do: String.starts_with?(id, target <> "|verb|") and
         not String.ends_with?(id, "|fallback")

  defp matching_verb?(_, _), do: false

  defp transitive?(cell) do
    case Map.get(cell, :gram_function) do
      tags when is_list(tags) -> "transitive" in tags
      _ -> false
    end
  end

  defp supported_strength(
         %{status: :candidate,
           frame_id: :verb_direct_object_hypothesis,
           strength: strength,
           supports: :transitive,
           rejects_intransitive?: false,
           roles: %{verb: observed_index},
           evidence: evidence},
         expected_index
       ) when strength in [:strong, :weak] and
              observed_index == expected_index and is_list(evidence) do
    base? =
      Enum.all?(
        [:attested_verb_pos, :bounded_subject_frame, :attested_nominal_head],
        &(&1 in evidence)
      )

    extra? =
      case strength do
        :strong -> :surface_determiner in evidence and
                     :immediately_following_nominal in evidence
        :weak -> :bare_nominal_immediately_following_verb in evidence
      end

    if base? and extra?, do: strength, else: nil
  end

  defp supported_strength(_, _), do: nil
end
