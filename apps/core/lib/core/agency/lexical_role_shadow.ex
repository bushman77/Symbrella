defmodule Core.Agency.LexicalRoleShadow do
  @moduledoc """
  Read-only, default-on accountability for LIFG role conflicts.

  This consumes the current SemanticInput after structural competition and
  composition. It never swaps a lexical winner. An alternative is eligible
  only if the existing RoleWitness gate independently accepts its scored,
  attested dictionary evidence for the *current* grammatical role.

  A compatible POS witness is NOT verification of a dictionary sense, speaker
  intent, event occurrence, or factual truth. Results are diagnostic only.
  """

  alias Core.Pipeline.Trace
  alias Core.Semantic.Composition
  alias Core.Semantic.Composition.{Diagnosis, RoleWitness}
  alias Core.Syntax.StructuralCompetition

  @version 1
  @max_roles 7
  @supported_constructions [
    :progressive_directional_candidate,
    :progressive_locative_candidate
  ]

  @spec evaluate(map()) :: map()
  def evaluate(%{} = si) do
    case StructuralCompetition.verified(si) do
      {:ok, %{preferred: %{construction: kind}}}
          when kind in @supported_constructions ->
        evaluate_verified_structure(si, kind)

      _ ->
        abstain(:structure_not_independently_verified)
    end
  end

  def evaluate(_), do: abstain(:invalid_input)

  @doc """
  Add a bounded accountability trace on every real Core pipeline pass.

  Caller options cannot disable the evidence record. No full sentence,
  definition, sense ID, or candidate distribution is placed in the trace.
  All live LIFG choices, memory, policy and response state remain unchanged.
  """
  @spec attach(map(), keyword()) :: map()
  def attach(si, opts \\ [])

  def attach(%{} = si, _opts) do
    report = evaluate(si)

    Trace.append(si, :reflective_role_shadow,
      decision: report.status,
      reason: report.reason,
      meta: %{
        v: report.v,
        construction: report.construction,
        frame_run_id: Map.get(si, :frame_run_id),
        selected_choices_sha256: selected_choices_sha256(si),
        blocked_role_count: report.blocked_role_count,
        blocked_roles: report.blocked_roles,
        alternative_count: length(report.alternatives),
        unresolved_role_count: report.unresolved_role_count,
        roles: Enum.map(report.alternatives, &Map.take(&1,
          [:role, :token_index, :selected_pos, :candidate_pos,
           :selection_basis, :candidate_score])),
        formal_derivation_verified?: report.formal_derivation_verified?,
        lexical_sense_verified?: false,
        lifg_modified?: false,
        learning_applied?: false,
        factual_authority?: false
      }
    )
  end

  def attach(other, _), do: other

  @doc "Diagnostic SHA-256 fingerprint of the bounded original LIFG winner slate."
  @spec selected_choices_sha256(map()) :: String.t() | nil
  def selected_choices_sha256(%{} = si) do
    choices = Map.get(si, :lifg_choices, [])

    if is_list(choices) and length(choices) <= 128 and
         Enum.all?(choices, fn choice ->
           is_map(choice) and is_integer(Map.get(choice, :token_index)) and
             is_binary(Map.get(choice, :id) || Map.get(choice, :chosen_id))
         end) do
      canonical =
        choices
        |> Enum.map(fn choice ->
          {Map.get(choice, :token_index), Map.get(choice, :id) || Map.get(choice, :chosen_id)}
        end)
        |> Enum.sort()

      :crypto.hash(:sha256, :erlang.term_to_binary(canonical))
      |> Base.encode16(case: :lower)
    else
      nil
    end
  end

  def selected_choices_sha256(_), do: nil

  defp evaluate_verified_structure(si, kind) do
    blocked = Diagnosis.blocked_roles(si) |> Enum.take(@max_roles)

    candidates =
      blocked
      |> Enum.map(&inspect_blocker(si, &1))
      |> Enum.split_with(&match?({:ok, _}, &1))

    {supported, unsupported} = candidates

    alternatives = Enum.map(supported, fn {:ok, candidate} -> candidate end)

    blocked_roles = Enum.map(blocked, &Map.take(&1,
      [:role, :token_index, :reason, :selected_pos,
       :expected_pos, :compatible_dictionary_pos?]))

    formal_proof? = match?({:ok, _}, Composition.verified(si))

    {status, reason} =
      cond do
        blocked == [] ->
          {:no_conflict_detected, :selected_roles_not_blocked}

        alternatives == [] ->
          {:abstained, :no_scored_attested_alternative}

        unsupported != [] ->
          {:partial_role_alternatives, :some_roles_still_unresolved}

        true ->
          {:provisional_role_alternatives, :scored_attested_role_witnesses}
      end

    %{
      v: @version,
      status: status,
      reason: reason,
      construction: kind,
      blocked_role_count: length(blocked),
      blocked_roles: blocked_roles,
      unresolved_role_count: length(unsupported),
      alternatives: alternatives,
      formal_derivation_verified?: formal_proof?,
      lexical_sense_verified?: false,
      lifg_modified?: false,
      learning_applied?: false,
      factual_authority?: false
    }
  end

  defp inspect_blocker(si, %{
         role: role, token_index: index, expected_pos: expected,
         reason: reason, selected_pos: selected_pos
       })
       when is_atom(role) and is_integer(index) and index >= 0 and
              is_list(expected) and reason in [
                :selected_pos_conflict, :selected_sense_not_attested
              ] do
    case RoleWitness.resolve(si, role, index, expected) do
      {:ok, %{selection_basis: basis, original_lifg_id: original,
              pos: candidate_pos} = witness}
          when basis in [:construction_alternative, :construction_pos_only] and
               is_binary(original) and is_binary(candidate_pos) ->
        {:ok, %{
          role: role,
          token_index: index,
          selected_pos: selected_pos,
          candidate_pos: candidate_pos,
          original_lifg_id: original,
          candidate_sense_id: Map.get(witness, :sense_id),
          selection_basis: basis,
          candidate_score: Map.get(witness, :candidate_score),
          evidence: :current_turn_scored_attested_role_witness,
          semantic_authority?: false
        }}

      _ ->
        {:unsupported, role}
    end
  end

  defp inspect_blocker(_si, %{role: role}), do: {:unsupported, role}
  defp inspect_blocker(_si, _), do: {:unsupported, :unknown}

  defp abstain(reason) do
    %{
      v: @version,
      status: :abstained,
      reason: reason,
      construction: nil,
      blocked_role_count: 0,
      blocked_roles: [],
      unresolved_role_count: 0,
      alternatives: [],
      formal_derivation_verified?: false,
      lexical_sense_verified?: false,
      lifg_modified?: false,
      learning_applied?: false,
      factual_authority?: false
    }
  end
end
