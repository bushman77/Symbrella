defmodule Core.Comprehension.CoverageLadderTest do
  use ExUnit.Case, async: true

  alias Core.Comprehension.CoverageLadder
  alias Core.SemanticInput
  alias Core.TokenFilters

  defp cell(word, pos), do: %{id: "#{word}|#{pos}|1", norm: word, pos: pos, semantic_atoms: []}

  defp cells do
    [
      cell("the", "determiner"),
      cell("cat", "noun"),
      cell("is", "auxiliary"),
      cell("not", "adverb"),
      cell("ready", "adjective")
    ]
  end

  defp input(sentence, active_cells, source) do
    %SemanticInput{sentence: sentence, active_cells: active_cells, source: source}
    |> TokenFilters.rebuild_word_ngrams(3)
    |> Map.from_struct()
  end

  defp uninterpreted(si), do: Map.put(si, :sentence_meaning, %{status: :not_yet_interpreted})

  defp full_span(last, roles), do: %{id: :copular_predication, span: {0, last}, roles: roles}

  defp binaries(value) when is_binary(value), do: [value]
  defp binaries(%MapSet{}), do: []
  defp binaries(%{} = map), do: map |> Map.values() |> Enum.flat_map(&binaries/1)
  defp binaries(list) when is_list(list), do: Enum.flat_map(list, &binaries/1)
  defp binaries(_), do: []

  test "an empty input yields a bounded record and invents no evidence" do
    record = CoverageLadder.assess(%{})

    assert record.v == 1
    assert record.lexical.status == :no_tokens
    assert record.signatures.full_span == 0
    assert record.meaning.class == :not_observed
    assert record.chart.applicable? == false
    assert record.primary_blocker == :no_tokens
    assert record.factual_authority? == false
    assert CoverageLadder.assess(:not_a_map) == record
  end

  test "an interpretation from another builder is not a failure, even without a full-span signature" do
    si =
      "The cat is ready."
      |> input(cells(), :user)
      |> Map.put(:sentence_signatures, [])
      |> Map.put(:sentence_meaning, %{
        status: :supported,
        kind: :explicit_temporal_report,
        source: :temporal_sentence
      })

    record = CoverageLadder.assess(si)

    assert record.signatures.full_span == 0
    assert record.composition.status == :not_derived
    assert record.meaning.class == :interpreted_correctness_unassessed
    assert record.primary_blocker == nil
  end

  test "a provisional structural hypothesis has its own class and is not counted as correct" do
    si =
      "The cat is ready."
      |> input(cells(), :user)
      |> Map.put(:sentence_meaning, %{status: :provisional, kind: :structural_hypothesis})

    record = CoverageLadder.assess(si)

    assert record.meaning.class == :provisional
    assert record.primary_blocker == nil
  end

  test "two complete candidates left unresolved are a tie opportunity" do
    si =
      "The cat is ready."
      |> input(cells(), :user)
      |> uninterpreted()
      |> Map.put(:sentence_signatures, [
        full_span(3, %{subject_head: 1, copula: 2, predicate: 3}),
        full_span(3, %{subject_head: 1, copula: 2, predicate: 3, subject_determiner: 0})
      ])
      |> Map.put(:structural_competition, %{
        status: :unresolved,
        candidates: [%{}, %{}],
        discrimination_margin: 0.0,
        preferred: nil
      })

    record = CoverageLadder.assess(si)

    assert record.lexical.status == :all_witnessed
    assert record.signatures.full_span == 2
    assert record.structural.tie_eligible? == true
    assert record.structural.margin == 0.0
    assert record.primary_blocker == :structural_tie
  end

  test "a role conflict counts only when the required POS is already attested" do
    evidence = [
      %{role: :subject_head, token_index: 1, expected_pos: ["noun"], selected_pos: "verb", status: :conflict}
    ]

    structural = %{
      status: :preferred,
      candidates: [%{}],
      discrimination_margin: nil,
      preferred: %{conflicting_roles: 1, lexical_role_evidence: evidence}
    }

    base = [cell("the", "determiner"), cell("fly", "verb"), cell("is", "auxiliary"), cell("small", "adjective")]

    build = fn active_cells ->
      "The fly is small."
      |> input(active_cells, :user)
      |> uninterpreted()
      |> Map.put(:sentence_signatures, [full_span(3, %{subject_head: 1, copula: 2, predicate: 3})])
      |> Map.put(:structural_competition, structural)
      |> CoverageLadder.assess()
    end

    attested = build.([cell("fly", "noun") | base])
    assert attested.structural.conflict_eligible? == true
    assert attested.structural.conflicts_with_attested_alternative == 1
    assert attested.primary_blocker == :preferred_structure_not_composed

    unattested = build.(base)
    assert unattested.structural.conflict_eligible? == false
    assert unattested.structural.preferred_conflicting_roles == 1
  end

  test "a Colony chart refusal is a policy abstention, not a grammar failure" do
    record =
      "The cat is not ready."
      |> input(cells(), :colony_external)
      |> uninterpreted()
      |> CoverageLadder.assess()

    assert record.chart.applicable? == true
    assert record.chart.policy_gate == :negation
    assert record.primary_blocker == :policy_abstention
    assert record.blocker_detail == :negation
  end

  test "the same sentence from chat is never labelled with a Colony policy gate" do
    record =
      "The cat is not ready."
      |> input(cells(), :user)
      |> uninterpreted()
      |> Map.put(:sentence_signatures, [])
      |> CoverageLadder.assess()

    assert record.chart.applicable? == false
    assert record.chart.policy_gate == nil
    assert record.primary_blocker == :no_full_span_signature
    assert record.blocker_detail == :all_tokens_witnessed
  end

  test "colony_policy_gate reports only closed word-based refusals" do
    assert CoverageLadder.colony_policy_gate("The cat is not ready.") == :negation
    assert CoverageLadder.colony_policy_gate("Is the cat ready?") == :interrogative
    assert CoverageLadder.colony_policy_gate("The cat is ready.") == nil
    assert CoverageLadder.colony_policy_gate("   ") == nil
    assert CoverageLadder.colony_policy_gate(nil) == nil
  end

  test "from_probe maps the bounded probe summary and marks unobservable fields" do
    probe = %{
      stage_ledger: %{
        structural: :unresolved,
        composition: :not_derived,
        structural_scope: %{
          base_tokens: 5,
          sentence_signatures: 3,
          full_span_signatures: 2,
          clause_core_signatures: 2,
          longest_span_tokens: 5,
          uncovered_token_count: 0,
          reason: :not_applicable
        }
      },
      stage_diagnostic: %{structural_candidates: 2},
      syntax: %{audit: %{pos_covered_tokens: 5}},
      sentence_meaning: %{status: :not_yet_interpreted}
    }

    record = CoverageLadder.from_probe(probe, nil)

    assert record.observed_from == :colony_probe
    assert record.lexical.status == :all_witnessed
    assert record.structural.tie_eligible? == true
    assert record.structural.conflict_eligible? == :not_observed
    assert record.structural.margin == nil
    assert record.primary_blocker == :structural_tie

    gated = CoverageLadder.from_probe(probe, :negation)
    assert gated.primary_blocker == :policy_abstention
    assert CoverageLadder.from_probe(probe, :not_a_policy_gate).chart.policy_gate == nil
  end

  test "records carry no user text" do
    record =
      "The cat is ready."
      |> input(cells(), :user)
      |> uninterpreted()
      |> CoverageLadder.assess()

    assert binaries(record) == []
  end

  defp rec(blocker, opts \\ []) do
    %{
      v: 1,
      primary_blocker: blocker,
      blocker_detail: Keyword.get(opts, :detail),
      structural: %{tie_eligible?: Keyword.get(opts, :tie?, false), conflict_eligible?: false},
      meaning: %{class: if(is_nil(blocker), do: :interpreted_correctness_unassessed, else: :none)},
      representative?: Keyword.get(opts, :representative?, true),
      arm: :chat,
      family: "copular",
      origin: "test"
    }
  end

  defp failures(n), do: List.duplicate(rec(:no_full_span_signature), n)
  defp ties(n), do: List.duplicate(rec(:structural_tie, tie?: true), n)

  describe "aggregate/1 screen" do
    test "too few eligible failures cannot support any conclusion" do
      result = CoverageLadder.aggregate(failures(26) ++ ties(3))
      assert result.screen.status == :insufficient_eligible_failures
      assert result.screen.eligible_failures == 29
    end

    test "the pre-declared ratio is inclusive at ten percent" do
      result = CoverageLadder.aggregate(failures(27) ++ ties(3))
      assert result.screen.status == :stage_1_may_be_proposed
      assert result.screen.opportunity_ratio == 0.1
    end

    test "below the ratio the conclusion is scoped to the evaluated distribution" do
      result = CoverageLadder.aggregate(failures(28) ++ ties(2))
      assert result.screen.status == :insufficient_eligible_opportunities_in_this_distribution
    end

    test "abstentions, interpreted rows and seeded non-representative ties are excluded" do
      seeded = List.duplicate(rec(:structural_tie, tie?: true, representative?: false), 10)
      policy = List.duplicate(rec(:policy_abstention, detail: :negation), 10)
      interpreted = List.duplicate(rec(nil), 5)

      result = CoverageLadder.aggregate(failures(30) ++ seeded ++ policy ++ interpreted)

      assert result.screen.eligible_failures == 30
      assert result.screen.opportunities == 0
      assert result.screen.status == :insufficient_eligible_opportunities_in_this_distribution
      assert result.representative.policy_or_budget_abstentions == 10
      assert result.representative.interpreted_correctness_unassessed == 5
      assert result.all.tie_opportunities == 10
      assert result.representative.blocker_details == %{"policy_abstention:negation" => 10}
      assert Map.keys(result.by_arm) == ["chat"]
    end

    test "unknown record versions and non-lists are ignored" do
      assert CoverageLadder.aggregate([%{v: 99}]).total_rows == 0
      assert CoverageLadder.aggregate(:nope).total_rows == 0
    end
  end

  test "the census is not wired into any production module" do
    root = Path.expand("../../../../..", __DIR__)

    referencing =
      root
      |> Path.join("apps/*/lib/**/*.ex")
      |> Path.wildcard()
      |> Enum.filter(&(File.read!(&1) =~ "CoverageLadder"))
      |> Enum.map(&Path.relative_to(&1, root))

    assert referencing == ["apps/core/lib/core/comprehension/coverage_ladder.ex"]
  end
end
