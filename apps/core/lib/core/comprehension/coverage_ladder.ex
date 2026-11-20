defmodule Core.Comprehension.CoverageLadder do
  @moduledoc """
  R-001 Stage 0 (P-022): offline, read-only comprehension coverage census.

  Despite the file name, this is **not** a linear ladder. Each record keeps
  independent dimensions:

    * source / Colony chart policy gate
    * lexical witnessing (dictionary POS rows per base token)
    * sentence-signature coverage (via `Core.Social.StructuralScope`)
    * structural competition, including tie and conflict *opportunities*
    * typed composition (`Core.Semantic.Composition.verified/1`)
    * sentence-meaning class

  A `primary_blocker` is named only when the observed evidence supports one.
  A sentence that any `SentenceMeaning` builder interpreted has no blocker,
  and that interpretation is **not** assessed for correctness here. A policy
  or budget abstention is a deliberate gate, never a grammar failure.

  Nothing here is called by `Core.resolve_input/2` or any live pipeline. It
  never reranks, retries, writes memory, emits telemetry of its own or grants
  factual authority (the reused Phase 1 tokenizer may emit its existing
  span-failure event). Records contain no user text, lexical IDs or definitions; callers
  may merge corpus metadata (case ID, arm, family, origin) afterwards.
  """

  alias Core.Social.StructuralScope
  alias Core.Syntax.{Chart, Signatures}

  @version 1

  # Closed Colony chart refusals that are policy, not missing grammar.
  @policy_gates [
    :unsupported_source,
    :structured_record,
    :instruction_shaped,
    :multiple_clauses,
    :interrogative,
    :negation,
    :modal
  ]
  @abstention_blockers [:policy_abstention, :budget_abstention]
  @structural_statuses [:preferred, :unresolved, :no_complete_candidate]
  @interpreted_classes [:provisional, :interpreted_correctness_unassessed]

  # Pre-declared Stage 0 screening heuristic (R-001 §6.5, refined in §8).
  @screen_ratio 0.10
  @min_eligible_failures 30
  @max_conflicts 16
  @max_count 10_000

  @doc "Census record for a SemanticInput returned by the chat resolver."
  @spec assess(map()) :: map()
  def assess(%{} = si) do
    structural = map_or_empty(Map.get(si, :structural_competition))
    scope = StructuralScope.observe(si, structural)
    nodes = Signatures.chart_nodes(si)

    lexical =
      lexical(Map.get(scope, :base_tokens), Enum.count(nodes, &witnessed?/1), :chart_nodes)

    record(
      :resolver_si,
      lexical,
      scope,
      structure(structural, nodes),
      chart_gate(si, nodes),
      composition(si),
      meaning(Map.get(si, :sentence_meaning))
    )
  end

  def assess(_), do: assess(%{})

  @doc """
  Census record built from the bounded result of `Core.Social.SemanticProbe.run/2`.

  The probe deliberately does not return its SemanticInput, so margin and
  role-conflict evidence are `nil` / `:not_observed` here, and composition is
  reported as derived-but-unverified at most. `policy_gate` should come from
  `colony_policy_gate/1` on the same text.
  """
  @spec from_probe(map(), atom() | nil) :: map()
  def from_probe(%{} = result, policy_gate) do
    ledger = map_or_empty(Map.get(result, :stage_ledger))
    diagnostic = map_or_empty(Map.get(result, :stage_diagnostic))
    audit = result |> Map.get(:syntax) |> map_or_empty() |> Map.get(:audit) |> map_or_empty()
    scope = map_or_empty(Map.get(ledger, :structural_scope))

    lexical =
      lexical(Map.get(scope, :base_tokens), Map.get(audit, :pos_covered_tokens), :probe_audit)

    status = allowed(Map.get(ledger, :structural), @structural_statuses)
    candidates = count(Map.get(diagnostic, :structural_candidates))

    structure = %{
      status: status,
      candidate_count: candidates,
      margin: nil,
      preferred_conflicting_roles: nil,
      conflicts_with_attested_alternative: nil,
      tie_eligible?: status == :unresolved and candidates >= 2,
      conflict_eligible?: :not_observed,
      detail: :probe_summary
    }

    chart = %{
      applicable?: true,
      policy_gate: if(policy_gate in @policy_gates, do: policy_gate, else: nil),
      chart_reason: :not_observed
    }

    composition =
      case Map.get(ledger, :composition) do
        :derived ->
          %{status: :derived_unverified, kind: atom_or_nil(Map.get(diagnostic, :typed_construction))}

        _ ->
          %{status: :not_derived, kind: nil}
      end

    record(
      :colony_probe,
      lexical,
      scope,
      structure,
      chart,
      composition,
      meaning(Map.get(result, :sentence_meaning))
    )
  end

  def from_probe(_, policy_gate), do: from_probe(%{}, policy_gate)

  @doc """
  Colony chart policy gate for raw text, or `nil`.

  Uses the same pure Phase 1 tokenization as the Colony probe, with an empty
  lexicon. Word-based refusals (negation, modal, interrogative, subordinator,
  structured record, multiple clauses) are exact; POS-dependent refusals such
  as a verb-initial imperative cannot be seen without LTM rows and return `nil`.
  """
  @spec colony_policy_gate(term()) :: atom() | nil
  def colony_policy_gate(text) when is_binary(text) do
    trimmed = String.trim(text)

    if trimmed == "" do
      nil
    else
      si =
        trimmed
        |> Core.Pipeline.Input.prepare(max_wordgram_n: 1)
        |> Core.Pipeline.Input.grammatical_projection()
        |> Map.from_struct()
        |> Map.put(:source, :colony_external)
        |> Map.put(:active_cells, [])

      reason = si |> Chart.diagnose(Signatures.chart_nodes(si)) |> Map.get(:reason)
      if reason in @policy_gates, do: reason, else: nil
    end
  end

  def colony_policy_gate(_), do: nil

  @doc """
  Aggregate census records. The screen uses rows marked `representative?: true`
  only, so deliberately seeded cases cannot manufacture an opportunity rate.
  """
  @spec aggregate([map()]) :: map()
  def aggregate(records) when is_list(records) do
    records = Enum.filter(records, &match?(%{v: @version}, &1))
    representative = Enum.filter(records, &(Map.get(&1, :representative?) == true))

    %{
      v: @version,
      total_rows: length(records),
      representative_rows: length(representative),
      all: summarize(records),
      representative: summarize(representative),
      by_arm: group(records, :arm),
      by_family: group(records, :family),
      by_origin: group(records, :origin),
      screen: screen(representative),
      screen_rule: %{
        opportunity_ratio_at_least: @screen_ratio,
        min_eligible_failures: @min_eligible_failures,
        population: :representative_rows_only,
        nature: :pre_declared_screening_heuristic
      },
      factual_authority?: false
    }
  end

  def aggregate(_), do: aggregate([])

  # ─────────────────────── record ───────────────────────

  defp record(observed_from, lexical, scope, structure, chart, composition, meaning) do
    signatures = %{
      total: count(Map.get(scope, :sentence_signatures)),
      full_span: count(Map.get(scope, :full_span_signatures)),
      clause_core: count(Map.get(scope, :clause_core_signatures)),
      longest_span_tokens: count(Map.get(scope, :longest_span_tokens)),
      uncovered_tokens: count(Map.get(scope, :uncovered_token_count)),
      scope_reason: atom_or_nil(Map.get(scope, :reason))
    }

    {blocker, detail} = blocker(lexical, signatures, structure, chart, composition, meaning)

    %{
      v: @version,
      observed_from: observed_from,
      lexical: lexical,
      signatures: signatures,
      structural: structure,
      chart: chart,
      composition: composition,
      meaning: meaning,
      primary_blocker: blocker,
      blocker_detail: detail,
      factual_authority?: false
    }
  end

  # Interpretation first: an interpreted sentence is not a failure, whatever
  # the other dimensions show. Gates next, then coverage from the bottom up.
  defp blocker(lexical, signatures, structure, chart, composition, meaning) do
    cond do
      meaning.class in @interpreted_classes ->
        {nil, nil}

      chart.policy_gate in @policy_gates ->
        {:policy_abstention, chart.policy_gate}

      signatures.scope_reason == :structural_token_budget_exceeded ->
        {:budget_abstention, :structural_token_budget_exceeded}

      lexical.status == :no_tokens ->
        {:no_tokens, nil}

      signatures.scope_reason == :noncontiguous_token_indices ->
        {:token_alignment_gap, nil}

      signatures.full_span == 0 and lexical.unwitnessed_tokens > 0 ->
        {:no_full_span_signature, :unwitnessed_tokens_present}

      signatures.full_span == 0 ->
        {:no_full_span_signature, :all_tokens_witnessed}

      structure.tie_eligible? == true ->
        {:structural_tie, nil}

      structure.status == :no_complete_candidate ->
        {:full_span_not_clause_eligible, nil}

      structure.status == :preferred and composition.status != :verified ->
        {:preferred_structure_not_composed, nil}

      structure.status == :unresolved ->
        {:structural_unresolved_single_candidate, nil}

      true ->
        {:undetermined, nil}
    end
  end

  # ─────────────────────── dimensions ───────────────────────

  defp lexical(base, witnessed, observed_from) do
    base = count(base)
    witnessed = min(count(witnessed), base)

    status =
      cond do
        base == 0 -> :no_tokens
        witnessed == base -> :all_witnessed
        witnessed == 0 -> :none_witnessed
        true -> :partial
      end

    %{
      base_tokens: base,
      witnessed_tokens: witnessed,
      unwitnessed_tokens: base - witnessed,
      status: status,
      observed_from: observed_from
    }
  end

  defp witnessed?(%{pos: %MapSet{} = pos}), do: MapSet.size(pos) > 0
  defp witnessed?(_), do: false

  defp structure(structural, nodes) do
    status = allowed(Map.get(structural, :status), @structural_statuses)
    candidates = structural |> Map.get(:candidates, []) |> List.wrap()
    preferred = Map.get(structural, :preferred)
    conflicts = conflict_evidence(preferred, nodes)

    %{
      status: status,
      candidate_count: min(length(candidates), @max_count),
      margin: margin(Map.get(structural, :discrimination_margin)),
      preferred_conflicting_roles: conflicting_roles(preferred),
      conflicts_with_attested_alternative: length(conflicts),
      tie_eligible?: status == :unresolved and length(candidates) >= 2,
      conflict_eligible?: status == :preferred and conflicts != [],
      detail: :observed
    }
  end

  # A preferred reading whose lexical winner contradicts a grammatical role,
  # while the token already has an attested row of the required POS. This is
  # an *opportunity* count only; nothing is substituted or re-scored.
  defp conflict_evidence(%{} = preferred, nodes) do
    by_index = Map.new(nodes, fn node -> {node.index, node.pos} end)

    preferred
    |> Map.get(:lexical_role_evidence, [])
    |> List.wrap()
    |> Enum.filter(fn
      %{status: :conflict, token_index: index, expected_pos: expected}
      when is_integer(index) and is_list(expected) ->
        attested = Map.get(by_index, index, MapSet.new())
        Enum.any?(expected, &MapSet.member?(attested, &1))

      _ ->
        false
    end)
    |> Enum.take(@max_conflicts)
  end

  defp conflict_evidence(_, _), do: []

  defp conflicting_roles(%{conflicting_roles: n}) when is_integer(n) and n >= 0, do: n
  defp conflicting_roles(_), do: 0

  defp chart_gate(si, nodes) do
    if Map.get(si, :source) == :colony_external do
      reason = si |> Chart.diagnose(nodes) |> Map.get(:reason)

      %{
        applicable?: true,
        policy_gate: if(reason in @policy_gates, do: reason, else: nil),
        chart_reason: atom_or_nil(reason)
      }
    else
      %{applicable?: false, policy_gate: nil, chart_reason: :not_colony_source}
    end
  end

  defp composition(si) do
    case Core.Semantic.Composition.verified(si) do
      {:ok, proof} ->
        %{status: :verified, kind: atom_or_nil(Map.get(proof, :kind))}

      :not_applicable ->
        if is_map(Map.get(si, :compositional_derivation)),
          do: %{status: :derived_unverified, kind: nil},
          else: %{status: :not_derived, kind: nil}
    end
  end

  defp meaning(%{} = meaning) do
    status = atom_or_nil(Map.get(meaning, :status))
    kind = atom_or_nil(Map.get(meaning, :kind))

    class =
      cond do
        is_nil(status) -> :not_observed
        status == :not_yet_interpreted -> :none
        status == :provisional or kind == :structural_hypothesis -> :provisional
        true -> :interpreted_correctness_unassessed
      end

    %{status: status, kind: kind, source: atom_or_nil(Map.get(meaning, :source)), class: class}
  end

  defp meaning(_), do: %{status: nil, kind: nil, source: nil, class: :not_observed}

  # ─────────────────────── aggregation ───────────────────────

  defp summarize(records) do
    failed = Enum.filter(records, &failed?/1)
    {abstained, eligible} = Enum.split_with(failed, &(&1.primary_blocker in @abstention_blockers))

    %{
      rows: length(records),
      interpreted_correctness_unassessed: count_class(records, :interpreted_correctness_unassessed),
      provisional: count_class(records, :provisional),
      failed: length(failed),
      policy_or_budget_abstentions: length(abstained),
      eligible_failures: length(eligible),
      tie_opportunities: Enum.count(eligible, &flag?(&1, :tie_eligible?)),
      conflict_opportunities: Enum.count(eligible, &flag?(&1, :conflict_eligible?)),
      opportunities: Enum.count(eligible, &opportunity?/1),
      blockers: Enum.frequencies_by(failed, &to_string(&1.primary_blocker)),
      blocker_details:
        failed
        |> Enum.filter(&(not is_nil(Map.get(&1, :blocker_detail))))
        |> Enum.frequencies_by(fn record -> "#{record.primary_blocker}:#{record.blocker_detail}" end)
    }
  end

  defp screen(records) do
    summary = summarize(records)
    eligible = summary.eligible_failures

    ratio =
      if eligible > 0, do: Float.round(summary.opportunities / eligible, 4), else: nil

    status =
      cond do
        eligible < @min_eligible_failures -> :insufficient_eligible_failures
        ratio >= @screen_ratio -> :stage_1_may_be_proposed
        true -> :insufficient_eligible_opportunities_in_this_distribution
      end

    %{
      status: status,
      opportunity_ratio: ratio,
      opportunities: summary.opportunities,
      eligible_failures: eligible
    }
  end

  defp group(records, key) do
    records
    |> Enum.group_by(&to_string(Map.get(&1, key)))
    |> Map.new(fn {value, rows} -> {value, summarize(rows)} end)
  end

  defp failed?(record), do: not is_nil(Map.get(record, :primary_blocker))

  defp opportunity?(record), do: flag?(record, :tie_eligible?) or flag?(record, :conflict_eligible?)

  defp flag?(record, key) do
    record |> Map.get(:structural, %{}) |> map_or_empty() |> Map.get(key) == true
  end

  defp count_class(records, class) do
    Enum.count(records, fn record ->
      record |> Map.get(:meaning, %{}) |> map_or_empty() |> Map.get(:class) == class
    end)
  end

  # ─────────────────────── helpers ───────────────────────

  defp allowed(value, permitted), do: if(value in permitted, do: value, else: :not_observed)

  defp margin(n) when is_number(n), do: Float.round(n * 1.0, 4)
  defp margin(_), do: nil

  defp count(n) when is_integer(n) and n >= 0, do: min(n, @max_count)
  defp count(_), do: 0

  defp atom_or_nil(value) when is_atom(value) and not is_boolean(value), do: value
  defp atom_or_nil(_), do: nil

  defp map_or_empty(%{} = map), do: map
  defp map_or_empty(_), do: %{}
end
