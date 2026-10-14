defmodule Brain.LIFG.LexicalAudit do
  @moduledoc """
  Read-only, sense-specific evidence audit for ambiguous dictionary words.

  Report context fit, same-POS competition, and whether the existing
  SemanticControl evidence gate could distinguish one exact dictionary ID.
  A top-scoring candidate is NOT automatically a verified meaning; zero or
  indistinguishable lexical evidence must remain unresolved.

  This module does not create senses, rewrite winners, update memory, or
  interpret factual truth. In particular, grammatical POS compatibility is
  not treated as evidence for a specific sense within that POS.
  """

  alias Brain.LIFG.ContextFit
  alias Brain.LIFG.SemanticControl
  alias Brain.Utils.Safe

  @spec analyze(String.t(), String.t(), [map()]) :: map()
  def analyze(sentence, target, candidates)
      when is_binary(sentence) and is_binary(target) and is_list(candidates) do
    rows =
      candidates
      |> Enum.filter(&is_map/1)
      |> Enum.flat_map(&evidence_row(sentence, target, &1))
      |> Enum.uniq_by(& &1.id)
      |> Enum.sort_by(&{&1.pos, -&1.context_fit, &1.id})

    groups =
      rows
      |> Enum.group_by(& &1.pos)
      |> Enum.sort_by(fn {pos, _} -> pos end)
      |> Enum.map(fn {pos, group} -> summarize_group(pos, group) end)

    %{
      sentence: sentence,
      target: target,
      candidate_count: length(rows),
      groups: groups,
      senses: rows
    }
  end

  def analyze(_, target, _), do: %{target: target, candidate_count: 0, groups: [], senses: []}

  defp evidence_row(sentence, target, candidate) do
    id = Safe.get(candidate, :id)
    pos = Safe.get(candidate, :pos) || pos_from_id(id)

    if is_binary(id) and is_binary(pos) and
         length(String.split(id, "|")) >= 3 do
      report = ContextFit.explain(sentence, target, candidate)

      [%{
        id: id,
        pos: pos,
        status: Safe.get(candidate, :status),
        context_fit: report.score,
        matches: report.matches,
        unique_matches: report.unique_matches,
        context_terms: report.context_terms,
        definition: Safe.get(candidate, :definition),
        example: Safe.get(candidate, :example),
        synonyms: Safe.get(candidate, :synonyms, [])
      }]
    else
      []
    end
  end

  defp summarize_group(pos, rows) do
    scores = Map.new(rows, &{&1.id, 0.5})
    evidence = Enum.map(rows, &%{id: &1.id, context_evidence: &1.context_fit})
    {_scores, control} = SemanticControl.apply(scores, evidence, focus_pos: pos)

    [first | rest] = rows
    second = case rest do [row | _] -> row.context_fit; _ -> 0.0 end

    # "eligible" means existing LIFG semantic control can make a bounded
    # adjustment. It does not assert the dictionary gloss is factually right.
    %{
      pos: pos,
      sense_count: length(rows),
      top_id: first.id,
      top_context_fit: first.context_fit,
      next_context_fit: second,
      evidence_gap: Float.round(max(first.context_fit - second, 0.0), 6),
      eligible_for_control?: control.applied?,
      reason: control.reason,
      adjustments: control.adjustments
    }
  end

  defp pos_from_id(id) when is_binary(id) do
    case String.split(id, "|", parts: 3) do
      [_, pos, _] -> pos
      _ -> nil
    end
  end

  defp pos_from_id(_), do: nil
end
