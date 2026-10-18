defmodule Brain.LIFG.SenseFocus do
  @moduledoc """
  Two-tier, fail-open budget for expensive LIFG exact-sense evidence.

  Every candidate still enters Stage1's inexpensive feature scorer, grammar
  calibration, full-slate softmax and uncertainty checks. When independent
  role competition supports a POS, only candidates of that POS require
  expensive ContextFit evaluation. This is *not* candidate pruning, and its
  counts are evaluations saved, not measured CPU time or accuracy.

  Incomplete, unsupported, conflicting, missing-POS and MWE slates always
  receive the original broad context evaluation.
  """

  @type plan :: %{
          required(:mode) => :focused | :broad,
          required(:reason) => atom(),
          required(:focus_pos) => String.t() | nil,
          required(:candidates) => non_neg_integer(),
          required(:context_evaluations) => non_neg_integer(),
          required(:context_skipped) => non_neg_integer()
        }

  @spec plan(map(), non_neg_integer(), [map()], keyword()) :: plan()
  def plan(si, index, candidates, opts \\ [])

  def plan(%{} = si, index, candidates, opts)
      when is_integer(index) and index >= 0 and is_list(candidates) and is_list(opts) do
    total = length(candidates)
    enabled? = Keyword.get(opts, :enabled?, true)
    mwe? = Keyword.get(opts, :mwe?, false)

    role =
      case Map.get(si, :role_competition) do
        %{source: :core_syntax_and_attested_dictionary_pos, roles: roles}
        when is_map(roles) -> Map.get(roles, index)

        _ -> nil
      end

    cond do
      not enabled? -> broad(total, :disabled)
      mwe? -> broad(total, :mwe_preserves_broad_context)
      total < 2 -> broad(total, :insufficient_competition)
      not supported_role?(role) -> broad(total, :no_adjudicated_pos)
      true ->
        pos = role.pos
        matching = Enum.count(candidates, &(candidate_pos(&1) == pos))

        if matching > 0 and matching < total do
          %{
            mode: :focused,
            reason: :adjudicated_structural_pos,
            focus_pos: pos,
            candidates: total,
            context_evaluations: matching,
            context_skipped: total - matching
          }
        else
          broad(total, if(matching == 0,
            do: :no_matching_pos_candidate, else: :all_candidates_same_pos))
        end
    end
  end

  def plan(_, _, candidates, _) when is_list(candidates),
    do: broad(length(candidates), :invalid_input)

  @doc "Whether this exact candidate receives the expensive contextual pass."
  @spec evaluate_context?(plan(), map()) :: boolean()
  def evaluate_context?(%{mode: :focused, focus_pos: pos}, candidate),
    do: candidate_pos(candidate) == pos

  def evaluate_context?(_, _), do: true

  defp supported_role?(%{status: :supported, pos: "noun",
                         role: :subject_head,
                         basis: :determined_subject_before_attested_past_predicate,
                         evidence: evidence}) when is_list(evidence) do
    Enum.all?([:bounded_determiner_surface, :dictionary_pos_witness,
               :attested_simple_past_predicate], &(&1 in evidence))
  end

  defp supported_role?(%{status: :supported, pos: "adjective",
                         role: :modifier,
                         basis: :complete_determiner_adjective_noun,
                         evidence: evidence}) when is_list(evidence) do
    Enum.all?([:bounded_determiner_surface, :dictionary_pos_witness,
               :complete_nominal_constituent], &(&1 in evidence))
  end

  defp supported_role?(_), do: false

  defp candidate_pos(candidate) when is_map(candidate) do
    case Map.get(candidate, :pos, Map.get(candidate, "pos")) do
      pos when is_atom(pos) -> Atom.to_string(pos)
      pos when is_binary(pos) -> pos
      _ -> id_pos(Map.get(candidate, :id, Map.get(candidate, "id")))
    end
  end

  defp candidate_pos(_), do: nil

  defp id_pos(id) when is_binary(id) do
    case String.split(id, "|", parts: 3) do
      [_, pos, _] -> pos
      _ -> nil
    end
  end

  defp id_pos(_), do: nil

  defp broad(total, reason) do
    %{
      mode: :broad,
      reason: reason,
      focus_pos: nil,
      candidates: total,
      context_evaluations: total,
      context_skipped: 0
    }
  end
end
