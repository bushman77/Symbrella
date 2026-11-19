defmodule Core.Social.StructuralScope do
  @moduledoc """
  Pure, observational classification of *why* structural competition found no
  complete candidate for an external sentence (P-020.1).

  This module never changes a grammatical decision. It reads the same inputs
  as `Brain.LIFG.StructuralCompetition.resolve/1` and reports coverage facts:
  which prefilter rejected the sentence, how many signatures exist, and how
  much of the sentence they cover. A full-span signature is evidence of
  coverage only; it does not prove the private `complete_clause?/2`
  eligibility, which is intentionally not reproduced here.

  The token-validity filter and the 48-token bound mirror the Brain module and
  are guarded by counterfactual tests that call the real Brain module, so any
  drift fails a test instead of silently mislabelling a sentence.
  """

  # Must equal Brain.LIFG.StructuralCompetition's @max_tokens and @max_signatures.
  @max_tokens 48
  @max_signatures 64

  @constituent_ids [
    :noun_phrase,
    :prepositional_phrase,
    :possessive_noun_phrase,
    :verb_particle_construction
  ]
  @predicate_roles [:predicate, :verb]
  @subject_roles [:subject, :subject_head, :wh, :object_head, :lexical_target]

  @spec observe(map(), map()) :: map()
  def observe(si, structural) when is_map(si) and is_map(structural) do
    indices = base_indices(Map.get(si, :tokens, []))
    last = List.last(indices)

    all_signatures = si |> Map.get(:sentence_signatures, []) |> List.wrap()
    signatures = Enum.take(all_signatures, @max_signatures)
    spans = signature_spans(signatures)

    full_span =
      if is_integer(last),
        do: Enum.count(spans, fn {first, stop} -> first == 0 and stop == last end),
        else: 0

    clause_cores = Enum.count(signatures, &clause_core?/1)

    reason =
      cond do
        Map.get(structural, :status) != :no_complete_candidate -> :not_applicable
        indices == [] -> :no_base_tokens
        last >= @max_tokens -> :structural_token_budget_exceeded
        indices != Enum.to_list(0..last) -> :noncontiguous_token_indices
        all_signatures == [] -> :no_sentence_signatures
        full_span == 0 -> :no_full_span_signature
        true -> :complete_signature_eligibility_unknown
      end

    %{
      status: Map.get(structural, :status, :unavailable),
      reason: reason,
      base_tokens: length(indices),
      sentence_signatures: min(length(all_signatures), @max_signatures),
      full_span_signatures: min(full_span, @max_signatures),
      clause_core_signatures: min(clause_cores, @max_signatures),
      longest_span_tokens:
        spans |> Enum.map(fn {first, stop} -> stop - first + 1 end) |> Enum.max(fn -> 0 end),
      uncovered_token_count:
        Enum.count(indices, fn index ->
          not Enum.any?(spans, fn {first, stop} -> index >= first and index <= stop end)
        end),
      constituent_only?: all_signatures != [] and clause_cores == 0,
      eligible_candidates: structural |> Map.get(:candidates, []) |> List.wrap() |> length(),
      scope: :observed_signatures_not_authoritative_eligibility,
      factual_authority?: false
    }
  end

  # Same validity rule as Brain.LIFG.StructuralCompetition.base_indices/1.
  defp base_indices(tokens) when is_list(tokens) do
    tokens
    |> Enum.filter(fn
      %{index: index, phrase: phrase} = token
      when is_integer(index) and index >= 0 and is_binary(phrase) ->
        Map.get(token, :n, 1) == 1 and Map.get(token, :kind) not in [:punct, "punct"]

      _ ->
        false
    end)
    |> Enum.map(& &1.index)
    |> Enum.sort()
  end

  defp base_indices(_), do: []

  defp signature_spans(signatures) do
    Enum.flat_map(signatures, fn
      %{span: {first, stop}}
      when is_integer(first) and is_integer(stop) and first >= 0 and stop >= first ->
        [{first, stop}]

      _ ->
        []
    end)
  end

  # Observational only: a clause-like signature has a predicate/verb role plus
  # a subject-like role and is not one of the constituent-only families.
  defp clause_core?(%{id: id, roles: roles}) when is_atom(id) and is_map(roles) do
    id not in @constituent_ids and
      Enum.any?(@predicate_roles, &Map.has_key?(roles, &1)) and
      Enum.any?(@subject_roles, &Map.has_key?(roles, &1))
  end

  defp clause_core?(_), do: false
end
