defmodule Core.Curiosity.UnresolvedIssues do
  @moduledoc """
  Read-only queue projection for independently running Curiosity/DriveLoop.

  This does not run web searches or resolve issues automatically. It exposes
  only bounded public-knowledge uncertainties, allowing the Curiosity loop to
  prioritize an investigation when an explicit, supervised tool boundary exists.
  """
  alias Db.CognitiveIssues

  @spec candidates(pos_integer()) :: [map()]
  def candidates(limit \\ 5) when is_integer(limit) and limit > 0 do
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
end
