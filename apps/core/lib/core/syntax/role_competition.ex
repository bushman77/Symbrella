defmodule Core.Syntax.RoleCompetition do
  @moduledoc """
  Read-only pre-LIFG adjudication of conflicting grammatical role hypotheses.

  Attach only evidence-backed POS support. Dictionary identities, words,
  sentence signatures, alternatives and factual truth remain unchanged.
  """
  alias Core.Pipeline.Trace

  @spec attach(map()) :: map()
  def attach(%{} = si) do
    result = Brain.LIFG.RoleCompetition.resolve(si)

    si
    |> Map.put(:role_competition, result)
    |> Trace.append(:role_competition,
      decision: if(result.roles == %{}, do: :abstained, else: :supported),
      reason: :independent_grammatical_evidence_competition,
      meta: %{
        decisions: result.decisions,
        supported_count: map_size(result.roles)
      })
  end

  def attach(si), do: si
end
