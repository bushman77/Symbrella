defmodule Core.Curiosity.UnresolvedIssues do
  @moduledoc """
  Read-only queue projection for independently running Curiosity/DriveLoop.

  This does not run web searches or resolve issues automatically. It exposes
  only bounded public-knowledge uncertainties, allowing the Curiosity loop to
  prioritize an investigation when an explicit, supervised tool boundary exists.
  """
  import Ecto.Query, only: [from: 2]
  alias Db.{CognitiveIssues, ConsolidatedConclusion}

  @spec candidates(pos_integer()) :: [map()]
  def candidates(limit \\ 5)

  def candidates(limit) when is_integer(limit) and limit > 0 do
    if Application.get_env(:core, :symbolic_consolidation_enabled?, false) do
      CognitiveIssues.pending(min(limit, 20))
      |> Enum.map(fn issue ->
        %{
          issue_id: issue.id,
          kind: issue.kind,
          status: issue.status,
          priority: issue.priority,
          next_action: issue.next_action,
          reference_year: issue.reference_year,
          subjects: Map.get(issue.evidence, "subjects", [])
        }
      end)
    else
      []
    end
  rescue
    _ -> []
  catch
    :exit, _ -> []
  end

  def candidates(_), do: []

  @doc """
  Read-only queue of independently eligible reasoning-link issues.

  Classification is a planning hint, not an authorized proof or a command.
  A direct hint requires an already stored, source-supported pair/year proof.
  Acquisition is a hint only when its independently controlled gate is on.
  """
  @spec reasoning_candidates(pos_integer(), keyword()) :: [map()]
  def reasoning_candidates(limit \\ 12, opts \\ [])

  def reasoning_candidates(limit, opts)
      when is_integer(limit) and limit > 0 and is_list(opts) do
    if Application.get_env(:core, :symbolic_consolidation_enabled?, false) do
      issues = CognitiveIssues.pending_reasoning_links(min(limit, 20))
      direct_pairs = source_backed_direct_pairs(issues)
      worker_cfg = Application.get_env(:core, Core.Curiosity.Investigator, [])
      acquire? =
        Keyword.get(opts, :acquire_evidence?,
          Keyword.get(worker_cfg, :acquire_evidence?, false)) == true

      Enum.map(issues, fn issue ->
        %{
          issue_id: issue.id,
          kind: issue.kind,
          status: issue.status,
          priority: issue.priority,
          next_action: issue.next_action,
          reference_year: issue.reference_year,
          strategy: strategy_hint(issue, direct_pairs, acquire?)
        }
      end)
    else
      []
    end
  rescue
    _ -> []
  catch
    :exit, _ -> []
  end

  def reasoning_candidates(_, _), do: []

  # A single bounded query avoids one DB roundtrip per candidate and verifies
  # only the existence of compatible durable premises. The Core investigator
  # still revalidates conflicts, source fingerprints and derivation rules.
  defp source_backed_direct_pairs([]), do: MapSet.new()

  defp source_backed_direct_pairs(issues) do
    keys = issues |> Enum.map(& &1.subject_key) |> Enum.uniq()

    Db.all(
      from(c in ConsolidatedConclusion,
        where: c.pair_key in ^keys and
          c.kind == "population_comparison" and
          c.provenance == "source_supported_symbolic_derivation",
        select: {c.pair_key, c.reference_year},
        distinct: true)
    )
    |> MapSet.new()
  end

  defp strategy_hint(issue, direct_pairs, acquire?) do
    cond do
      MapSet.member?(direct_pairs, {issue.subject_key, issue.reference_year}) ->
        :direct

      match?(%{"stored_proof_ids" => [id]} when is_integer(id) and id > 0,
        issue.evidence) ->
        :two_hop

      acquire? ->
        :acquisition

      true ->
        :missing_link
    end
  end
end
