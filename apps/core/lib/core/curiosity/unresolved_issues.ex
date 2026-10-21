defmodule Core.Curiosity.UnresolvedIssues do
  @moduledoc """
  Read-only queue projection for independently running Curiosity/DriveLoop.

  This does not run web searches or resolve issues automatically. It exposes
  only bounded public-knowledge uncertainties, allowing the Curiosity loop to
  prioritize an investigation when an explicit, supervised tool boundary exists.
  """
  alias Db.CognitiveIssues

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

  @doc "Prioritize only supported reasoning-link uncertainties for silent investigation."
  @spec reasoning_candidates(pos_integer()) :: [map()]
  def reasoning_candidates(limit \\ 12)

  def reasoning_candidates(limit) when is_integer(limit) and limit > 0 do
    if Application.get_env(:core, :symbolic_consolidation_enabled?, false) do
      CognitiveIssues.pending_reasoning_links(min(limit, 20))
      |> Enum.map(fn issue ->
        %{
          issue_id: issue.id,
          kind: issue.kind,
          status: issue.status,
          priority: issue.priority,
          next_action: issue.next_action,
          reference_year: issue.reference_year,
          # Advisory only: a single original parent suggests missing-bridge
          # reasoning, but neither the parent nor composition is authorized.
          strategy: proposed_strategy(issue.evidence)
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

  def reasoning_candidates(_), do: []

  defp proposed_strategy(%{"stored_proof_ids" => [id]}) when is_integer(id) and id > 0,
    do: :two_hop
  defp proposed_strategy(_), do: :missing_link
end
