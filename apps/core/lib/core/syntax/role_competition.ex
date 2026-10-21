defmodule Core.Syntax.RoleCompetition do
  @moduledoc """
  Read-only pre-LIFG adjudication of conflicting grammatical role hypotheses.

  Attach only evidence-backed POS support. Dictionary identities, words,
  sentence signatures, alternatives and factual truth remain unchanged.
  """
  alias Core.Pipeline.Trace

  @spec attach(map()) :: map()
  def attach(%{} = si) do
    # The Core-side evidence producer can inspect syntax and dictionary cells
    # without creating a Brain -> Core application dependency.
    predicate_proposals = Core.Syntax.PredicateEvidence.detect(si)

    result =
      si
      |> Map.put(:predicate_proposals, predicate_proposals)
      |> Brain.LIFG.RoleCompetition.resolve()
      |> Map.put(:predicate_proposals, predicate_proposals)

    si
    |> Map.put(:role_competition, result)
    |> Trace.append(:role_competition,
      decision: if(result.roles == %{}, do: :abstained, else: :supported),
      reason: :independent_grammatical_evidence_competition,
      meta: %{
        decisions: result.decisions,
        predicate_proposals: length(predicate_proposals),
        supported_count: map_size(result.roles)
      })
  end

  def attach(si), do: si
end
