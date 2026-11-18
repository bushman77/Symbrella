defmodule Core.Integration.Audit do
  @moduledoc """
  Read-only consistency audit across semantic pipeline boundaries.

  Revalidates upstream evidence without altering lexical winners,
  semantic interpretations, episodic candidates, or control policy.

  An inconsistent result is diagnostic in V1, not a new authority.
  """

  alias Core.Pipeline.Trace
  alias Core.Semantic.{EpisodicProjection, SentenceMeaning}
  alias Core.Syntax.StructuralCompetition
  alias Core.Integration.Coverage
  @version 1

  @spec check(map()) :: map()
  def check(%{} = si) do
    checks = %{
      sentence_meaning: check_meaning(si),
      structural_preference: check_structure(si),
      episodic_projection: check_episodes(si),
      provisional_authority: check_provisional(si)
    }

    violations =
      checks
      |> Enum.filter(fn {_name, result} -> result == :fail end)
      |> Enum.map(&elem(&1, 0))
      |> Enum.sort()

    status =
      cond do
        violations != [] ->
          :inconsistent

        checks.sentence_meaning == :not_observed ->
          :unassessed

        true ->
          :consistent
      end

    %{
      version: @version,
      status: status,
      checks: checks,
      violations: violations,
      enforcement: :observe_only,
      factual_authority?: false,
      coverage: Coverage.assess(si)
    }
  end

  def check(_) do
    %{
      version: @version,
      status: :unassessed,
      checks: %{},
      violations: [],
      enforcement: :observe_only,
      factual_authority?: false
    }
  end

  @spec attach(map()) :: map()
  def attach(%{} = si) do
    audit = check(si)

    si
    |> Map.put(:integration_audit, audit)
    |> Trace.append(:integration_audit,
      decision: audit.status,
      reason: :cross_stage_consistency_check,
      meta: audit
    )
  end

  def attach(other), do: other

  # The attached meaning must match independent recomputation
  # from the current SemanticInput.
  defp check_meaning(%{sentence_meaning: %{} = meaning} = si) do
    if meaning == SentenceMeaning.build(si),
      do: :pass,
      else: :fail
  end

  defp check_meaning(_), do: :not_observed

  # A structural hypothesis must still have the same preferred
  # construction and roles as independently verified competition.
  defp check_structure(
         %{
           sentence_meaning: %{
             kind: :structural_hypothesis,
             status: :provisional,
             source_frame: frame,
             roles: roles
           }
         } = si
       ) do
    case StructuralCompetition.verified(si) do
      {:ok,
       %{
         preferred: %{
           construction: ^frame,
           roles: ^roles
         }
       }} ->
        :pass

      _ ->
        :fail
    end
  end

  defp check_structure(_), do: :not_applicable

  # No injected or stale event may silently enter the candidate list.
  defp check_episodes(%{episodic_candidates: candidates} = si)
       when is_list(candidates) do
    if candidates == EpisodicProjection.project(si),
      do: :pass,
      else: :fail
  end

  defp check_episodes(%{episodic_candidates: _}), do: :fail
  defp check_episodes(_), do: :not_observed

  # Grammar-level interpretation does not establish event existence,
  # factual authority, or the identity of an unresolved participant.
  defp check_provisional(
         %{
           sentence_meaning:
             %{
               kind: :structural_hypothesis,
               status: :provisional
             } = meaning
         } = si
       ) do
    unresolved = Map.get(meaning, :unresolved, [])

    actor_valid? =
      case get_in(meaning, [:participants, :actor]) do
        nil ->
          true

        %{referent: :unresolved} ->
          is_list(unresolved) and
            :actor_referent in unresolved

        _ ->
          false
      end

    valid? =
      meaning.definition == nil and
        match?(
          %{factual_authority?: false},
          Map.get(meaning, :grounding)
        ) and
        is_list(unresolved) and
        :event_grounding in unresolved and
        actor_valid? and
        Map.get(si, :episodic_candidates, []) == []

    if valid?, do: :pass, else: :fail
  end

  defp check_provisional(_), do: :not_applicable
end
