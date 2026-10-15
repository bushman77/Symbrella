defmodule Brain.LIFG.DefinitionImpactAudit do
  @moduledoc """
  Pure, read-only shadow comparison of ContextFit with stored versus proposed
  dictionary definitions, holding every OTHER BrainCell field constant.

  Ranking is raw deterministic dictionary ContextFit, not a production Stage1
  winner or calibrated sense correctness. A proposed expected ID is a human
  hypothesis, not a training label. The actual SemanticControl gate still
  requires sufficient independent evidence.
  """

  alias Brain.LIFG.LexicalAudit

  @spec compare(String.t(), String.t(), String.t(), list(), list()) ::
          {:ok, map()} | {:error, term()}
  def compare(sentence, target, expected_id, old_cells, reconstructed_cells)
      when is_binary(sentence) and is_binary(target) and is_binary(expected_id) and
             is_list(old_cells) and is_list(reconstructed_cells) do
    old_ids = Enum.map(old_cells, & &1.id)
    new_ids = Enum.map(reconstructed_cells, & &1.id)

    cond do
      old_ids == [] ->
        {:error, :no_candidates}

      length(old_ids) != length(Enum.uniq(old_ids)) or
          length(new_ids) != length(Enum.uniq(new_ids)) ->
        {:error, :duplicate_ids}

      Enum.sort(old_ids) != Enum.sort(new_ids) ->
        {:error, :id_census_mismatch}

      not Enum.member?(old_ids, expected_id) ->
        {:error, :expected_id_not_found}

      true ->
        by_id = Map.new(reconstructed_cells, &{&1.id, &1})

        # Never use reconstructed semantic atoms, activation, examples,
        # synonyms or grammar: the experiment changes ONLY the gloss.
        proposed =
          Enum.map(old_cells, fn cell ->
            %{cell | definition: Map.fetch!(by_id, cell.id).definition}
          end)

        baseline = LexicalAudit.analyze(sentence, target, old_cells)
        shadow = LexicalAudit.analyze(sentence, target, proposed)

        case {summarize(baseline, expected_id), summarize(shadow, expected_id)} do
          {{:ok, before}, {:ok, after}} ->
            {:ok, %{
              sentence: sentence,
              target: target,
              hypothesized_id: expected_id,
              before: before,
              after: after,
              expected_fit_delta:
                Float.round(after.expected_fit - before.expected_fit, 6),
              same_top_id?: before.top_id == after.top_id,
              note: :dictionary_only_shadow_no_winner_changes
            }}

          _ ->
            {:error, :missing_comparable_rows}
        end
    end
  end

  def compare(_, _, _, _, _), do: {:error, :invalid_arguments}

  defp summarize(report, expected_id) do
    ordered =
      report.senses
      |> Enum.sort_by(&{-&1.context_fit, &1.id})

    rank = Enum.find_index(ordered, &(&1.id == expected_id))

    if is_nil(rank) or ordered == [] do
      {:error, :missing_id}
    else
      expected = Enum.at(ordered, rank)
      group = Enum.find(report.groups, &(&1.pos == expected.pos))
      first = hd(ordered)

      {:ok, %{
        top_id: first.id,
        top_fit: first.context_fit,
        hypothesized_id_rank: rank + 1,
        expected_fit: expected.context_fit,
        expected_definition_matches: Map.get(expected.unique_matches, :definition, []),
        positive_fit_candidates: Enum.count(ordered, &(&1.context_fit > 0.0)),
        semantic_control_eligible?: if(group, do: group.eligible_for_control?, else: false),
        semantic_control_reason: if(group, do: group.reason, else: :no_group),
        evidence_gap: if(group, do: group.evidence_gap, else: 0.0)
      }}
    end
  end
end
