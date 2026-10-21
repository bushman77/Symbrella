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
    nominal = Core.Syntax.NominalEvidence.detect(si)

    adjudicated =
      si
      |> Map.put(:predicate_proposals, predicate_proposals)
      |> Map.put(:nominal_proposals, nominal.proposals)
      |> Brain.LIFG.RoleCompetition.resolve()

    argument_frames = Core.Syntax.ArgumentEvidence.detect(si, adjudicated, nominal)
    argument_compatibility =
      Brain.LIFG.ArgumentCompatibility.compare(
        argument_frames, Map.get(si, :active_cells, [])
      )

    result =
      adjudicated
      |> Map.put(:predicate_proposals, predicate_proposals)
      |> Map.put(:nominal_frames, nominal.frames)
      |> Map.put(:unresolved_tokens, nominal.unresolved_tokens)
      |> Map.put(:argument_frames, argument_frames)
      |> Map.put(:argument_compatibility, argument_compatibility)

    si
    |> Map.put(:role_competition, result)
    |> Trace.append(:role_competition,
      decision: if(result.roles == %{}, do: :abstained, else: :supported),
      reason: :independent_grammatical_evidence_competition,
      meta: %{
        decisions: result.decisions,
        predicate_proposals: length(predicate_proposals),
        nominal_frames: length(nominal.frames),
        unresolved_code_tokens: length(nominal.unresolved_tokens),
        argument_frames: length(argument_frames),
        supported_count: map_size(result.roles)
      })
  end

  def attach(si), do: si
end
