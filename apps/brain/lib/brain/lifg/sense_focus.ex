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

  defp supported_role?(%{status: :supported, pos: "noun",
                          role: :subject_head,
                          basis: :determined_subject_before_attested_present_predicate,
                          evidence: evidence}) when is_list(evidence) do
    Enum.all?([:complete_finite_present_svo, :dictionary_pos_witness,
               :attested_finite_present_verb], &(&1 in evidence))
  end

  defp supported_role?(%{status: :supported, pos: "noun",
                          role: :subject_head,
                          basis: :determined_subject_in_wh_aux_question,
                          evidence: evidence}) when is_list(evidence) do
    Enum.all?([:wh_fronted, :auxiliary_inversion, :bounded_determiner_surface,
               :dictionary_pos_witness, :question_punctuation], &(&1 in evidence))
  end

  defp supported_role?(%{status: :supported, pos: "verb",
                          role: :predicate,
                          basis: :predicate_in_wh_aux_question,
                          evidence: evidence}) when is_list(evidence) do
    Enum.all?([:wh_fronted, :auxiliary_inversion, :bounded_determiner_surface,
               :dictionary_pos_witness, :question_punctuation,
               :attested_verb_alternative], &(&1 in evidence))
  end

  # Consume the same independently witnessed predicate role as Stage1
  # scoring. This focuses contextual work but never prunes alternatives or
  # independently selects a verb meaning.
  defp supported_role?(%{status: :supported, pos: "verb",
                          role: :predicate, basis: basis, evidence: evidence})
       when basis in [:modal_question_predicate, :modal_declarative_predicate,
                      :imperative_predicate, :plural_present_predicate,
                      :finite_present_svo_predicate,
                      :counted_present_transitive_predicate,
                      :counted_negative_transitive_predicate] and is_list(evidence) do
    required =
      case basis do
        :modal_question_predicate -> [:auxiliary_subject_order]
        :modal_declarative_predicate -> [:subject_auxiliary_order]
        :imperative_predicate -> [:initial_predicate]
        :plural_present_predicate ->
          [:attested_plural_subject, :present_base_verb_witness]
        :finite_present_svo_predicate -> [:attested_finite_present_verb]
        :counted_present_transitive_predicate ->
          [:attested_counted_plural_subject, :attested_base_present_verb,
           :attested_nominal_object]
        :counted_negative_transitive_predicate ->
          [:attested_counted_subject, :explicit_did_not_negation,
           :attested_nominal_object]
      end

    Enum.all?(required ++ [:dictionary_pos_witness, :bounded_predicate_slot],
      &(&1 in evidence))
  end

  defp supported_role?(%{status: :supported, pos: "adjective",
                         role: :modifier,
                         basis: :determined_adjective_before_unresolved_head,
                         evidence: evidence}) when is_list(evidence) do
    Enum.all?([:bounded_determiner_surface, :dictionary_pos_witness,
               :unresolved_code_head, :complete_nominal_surface],
      &(&1 in evidence))
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
