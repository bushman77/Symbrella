defmodule Core.Brain.SemanticCohort do
  @moduledoc """
  Read-only sentence-conditioned cohorts of *vetted dictionary verb senses*.

  The independent SemanticSenseValidator performs the genuine SVO,
  inflection, grammatical and specialist-domain gates first. This module
  consumes the FULL eligible diagnostic slate (not its display shortlist).

  Cohorts are overlapping groups indexed by a specific independently
  observed lexical witness, such as {:patient_gloss, "plant"}. Two senses
  share a cohort only if their own dictionary definitions contain the
  witness AND both had the matching sentence-conditioned evidence.
  Similar-looking definitions alone cannot create a supported cohort.

  This is deliberately NOT clustering by transitive similarity: A~B and
  B~C never makes A~C. One shared lexical witness is weak evidence of
  related *uses*, never proof of dictionary sense equivalence.
  """

  alias Core.Semantic.SVOEvent

  @max_candidates 256
  @max_anchors 32
  @max_cohorts 8
  @max_members 12
  @max_distinctions 8
  @min_support 0.04

  @noise MapSet.new(~w(
    a an the and or to for from in on of with by at is are be been
    being that this these those it its which who whose when where
    someone something some one any used use using make makes made
    do does done have has having their them means act action thing
    object objects person persons type types manner often usually
    especially general generally typically such other others
    also into through about as can may might will
    transitive intransitive ambitransitive ditransitive
  ))

  @spec evaluate(map(), map()) :: map()
  def evaluate(si, report)

  def evaluate(%{sentence_meaning: %{kind: :participant_assertion,
      status: :structurally_interpreted, source_frame: :finite_present_svo,
      roles: %{actor: actor, patient: patient, predicate: predicate}}} = si,
      %{status: status, cohort_evidence_rows: rows} = report)
      when status in [:abstained, :provisional_hypothesis] and
        is_list(rows) and length(rows) <= @max_candidates and
        is_map(actor) and is_map(patient) and is_map(predicate) do
    with %{roles: %{subject_head: a, object_head: p, predicate: v}} <-
           SVOEvent.complete_signature(si),
         true <- is_integer(a) and is_integer(v) and is_integer(p),
         true <- Map.get(si, :source) in [:user, :prod, :test],
         true <- Enum.all?(rows, &valid_eligible?/1) do
      candidates =
      rows
      |> Enum.filter(&(&1.combined_diagnostic_score >= @min_support))
      |> Enum.map(&candidate/1)

      groups =
        candidates
        |> Enum.flat_map(fn c ->
          Enum.map(c.anchors, fn anchor -> {anchor, c} end)
        end)
        |> Enum.group_by(&elem(&1, 0), &elem(&1, 1))
        |> Enum.filter(fn {_anchor, members} ->
          length(members) >= 2
        end)
        |> Enum.sort_by(fn {{origin, term}, members} ->
          {-length(members), origin_order(origin), term}
        end)
        |> Enum.take(@max_anchors)
        |> Enum.map(fn {anchor, members} -> cohort(anchor, members) end)
        |> Enum.sort_by(fn group ->
          {-group.minimum_member_support, -group.member_count,
            group.evidence_origin, group.anchor_term}
        end)
        |> Enum.take(@max_cohorts)

      %{
        status: if(groups == [], do: :no_supported_cohort,
          else: :candidate_cohorts),
        reason: if(groups == [], do: :no_shared_context_and_gloss_witness,
          else: :shared_sentence_conditioned_lexical_evidence),
        source_frame: :finite_present_svo,
        base_lexeme: Map.get(report, :attested_base_lexeme),
        original_surface_sense: Map.get(report, :original_surface_id),
        participant_surfaces: %{
          actor: Map.get(actor, :surface),
          predicate: Map.get(predicate, :surface),
          patient: Map.get(patient, :surface)
        },
        eligible_sense_count: length(rows),
        cohort_count: length(groups),
        cohorts: groups,
        independently_verified_meaning?: false,
        semantic_equivalence_verified?: false,
        chose_exact_sense?: false,
        modified_lifg?: false,
        modified_atl?: false,
        modified_brain_cells?: false,
        modified_synapses?: false,
        changed_response?: false
      }
    else
      _ -> skipped(:unvalidated_projected_comprehension)
    end
  end

  def evaluate(_, _), do: skipped(:missing_vetted_sentence_evidence)

  defp valid_eligible?(%{
         id: id, eligible?: true, grammar: grammar,
         combined_diagnostic_score: combined,
         lexical_score: direct, cohort_gloss: gloss,
         matching_context: matches, one_hop_witnesses: witnesses,
         exclusions: [], missing_restrictions: []
       }) do
    is_binary(id) and is_binary(gloss) and byte_size(gloss) <= 4000 and
      grammar not in [:intransitive_only, :conflicting_grammar] and
      is_number(combined) and combined >= 0.0 and combined <= 1.0 and
      is_number(direct) and direct >= 0.0 and direct <= 1.0 and
      is_list(matches) and is_list(witnesses)
  end

  defp valid_eligible?(_), do: false

  defp candidate(row) do
    gloss_tokens = definition_terms(row.cohort_gloss)
    vocabulary = MapSet.new(gloss_tokens)

    direct =
      row.matching_context
      |> Enum.filter(&(is_binary(&1) and MapSet.member?(vocabulary, &1)))
      |> Enum.map(&{:direct_sentence, &1})

    roles =
      row.one_hop_witnesses
      |> Enum.flat_map(fn
        %{role: role, matched: matched, contribution: value}
            when role in [:actor, :patient] and is_list(matched) and
              is_number(value) and value > 0.0 ->
          origin = if(role == :patient, do: :patient_gloss, else: :actor_gloss)
          Enum.filter(matched, &(is_binary(&1) and
            MapSet.member?(vocabulary, &1)))
          |> Enum.map(&{origin, &1})
        _ -> []
      end)

    anchors = (direct ++ roles) |> Enum.uniq() |> Enum.sort()

    %{
      id: row.id,
      direct: row.lexical_score,
      patient: Map.get(row, :patient_support, 0.0),
      actor: Map.get(row, :actor_support, 0.0),
      combined: row.combined_diagnostic_score,
      gloss_tokens: gloss_tokens,
      grammar: row.grammar,
      anchors: anchors
    }
  end

  defp cohort({origin, term}, members) do
    members = Enum.sort_by(members, & &1.id)

    common =
      members
      |> Enum.map(&MapSet.new(&1.gloss_tokens))
      |> then(fn
        [first | rest] ->
          Enum.reduce(rest, first, &MapSet.intersection/2)
          |> MapSet.to_list()
          |> Enum.sort()
        _ -> []
      end)

    unique =
      Enum.map(members, fn member ->
        %{
          id: member.id,
          direct_fit: member.direct,
          patient_support: member.patient,
          actor_support: member.actor,
          diagnostic_score: member.combined,
          grammatical_compatibility: member.grammar,
          distinguishing_definition_terms:
            member.gloss_tokens
            |> Enum.reject(&(&1 in common))
            |> Enum.take(@max_distinctions)
        }
      end)

    %{
      evidence_origin: origin,
      anchor_term: term,
      member_count: length(members),
      members: Enum.take(unique, @max_members),
      member_ids_truncated?: length(members) > @max_members,
      shared_definition_terms: Enum.take(common, @max_distinctions),
      minimum_member_support:
        members |> Enum.map(& &1.combined) |> Enum.min() |> Float.round(6),
      # This is an empirical floor, NOT a probability of group truth.
      evidence_status: :shared_lexical_anchor_only,
      exact_sense_unresolved?: true,
      shared_meaning_verified?: false,
      domain_restrictions_prechecked?: true
    }
  end

  defp definition_terms(gloss) do
    gloss
    |> String.downcase()
    |> String.replace(~r/\A(?:\([^)]{1,120}\)\s*)+/u, "")
    |> String.split(" — ")
    |> List.last()
    |> then(&Regex.scan(~r/[\p{L}]{3,26}/u, &1))
    |> List.flatten()
    |> Enum.map(&normalize_gloss_token/1)
    |> Enum.reject(&MapSet.member?(@noise, &1))
    |> Enum.uniq()
    |> Enum.take(48)
  end

  # Match ContextFit's ordinary English noun/verb inflection comparison.
  # This normalizes only diagnostic words, never original dictionary IDs.
  defp normalize_gloss_token(token) do
    cond do
      String.length(token) > 5 and String.ends_with?(token, "ies") ->
        String.replace_suffix(token, "ies", "y")

      String.length(token) > 5 and
          Enum.any?(["sses", "ches", "shes", "xes", "zes"],
            &String.ends_with?(token, &1)) ->
        String.slice(token, 0, String.length(token) - 2)

      String.length(token) > 4 and String.ends_with?(token, "s") and
          not String.ends_with?(token, "ss") ->
        String.slice(token, 0, String.length(token) - 1)

      true -> token
    end
  end

  defp origin_order(:direct_sentence), do: 0
  defp origin_order(:patient_gloss), do: 1
  defp origin_order(:actor_gloss), do: 2

  defp skipped(reason), do: %{
    status: :skipped, reason: reason, cohorts: [], cohort_count: 0,
    independently_verified_meaning?: false,
    chose_exact_sense?: false, modified_lifg?: false,
    modified_synapses?: false, changed_response?: false
  }
end
