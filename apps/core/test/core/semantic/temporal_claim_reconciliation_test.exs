defmodule Core.Semantic.TemporalClaimReconciliationTest do
  use ExUnit.Case, async: false

  alias Core.Discourse.Ledger
  alias Core.Response.{SemanticStrategy, SymbolicGeneration}
  alias Core.Semantic.{SentenceMeaning, TemporalClaimReconciliation, TemporalSentence}

  @before ~w(The bird is sitting on the fence before the cat is sleeping under the tree)
  @after ~w(The bird is sitting on the fence after the cat is sleeping under the tree)
  @different ~w(The dog is sitting on the fence before the cat is sleeping under the tree)
  @pos ~w(determiner noun verb verb preposition determiner noun conjunction determiner noun verb verb preposition determiner noun)
  @question "Did I describe conflicting temporal orders?"
  @features %{risk_bucket: :low, intent: :ask}
  @decision %{action: :answer}

  defp session, do: "temporal-conflict-#{System.unique_integer([:positive])}"

  defp claim(sid, words) do
    rows = Enum.zip(words, @pos) |> Enum.map(fn {word, pos} ->
      norm = String.downcase(word)
      %{id: "#{norm}|#{pos}|1", norm: norm, pos: pos,
        semantic_atoms: if(norm in ["sitting", "sleeping"],
          do: ["tag:form-of", "tag:present", "tag:participle"], else: [])}
    end)

    si = %Core.SemanticInput{
      session_id: sid, source: :test, intent: :statement,
      sentence: Enum.join(words, " ") <> ".",
      tokens: words |> Enum.with_index()
        |> Enum.map(fn {phrase, index} -> %{phrase: phrase, index: index, n: 1} end),
      active_cells: rows,
      lifg_choices: rows |> Enum.with_index()
        |> Enum.map(fn {row, index} -> %{id: row.id, token_index: index} end)
    }
    |> SentenceMeaning.attach()

    assert {:ok, _meaning} = TemporalSentence.verified(si)
    Ledger.record(si)
  end

  defp ask(sid, question \\ @question) do
    %Core.SemanticInput{
      session_id: sid, sentence: question, source: :test, intent: :ask
    }
    |> SentenceMeaning.attach()
  end

  setup do
    sid = session()
    other = session()
    assert is_pid(Process.whereis(Core.Discourse.Ledger.Owner))
    on_exit(fn ->
      Ledger.clear(sid)
      Ledger.clear(other)
    end)
    {:ok, sid: sid, other: other}
  end

  test "two opposing closed descriptions yield one candidate, never an event contradiction",
       %{sid: sid} do
    claim(sid, @before)
    claim(sid, @after)

    assert {:ok, conflict} = TemporalClaimReconciliation.detect(sid)
    assert conflict.kind == :candidate_temporal_order_conflict
    assert conflict.matching_basis == :equal_closed_event_descriptions
    assert conflict.scope == :same_session
    assert conflict.older_report.seq == 1
    assert conflict.newer_report.seq == 2
    assert conflict.older_report.earlier.action == :sit
    assert conflict.older_report.later.action == :sleep
    assert conflict.newer_report.earlier.action == :sleep
    assert conflict.newer_report.later.action == :sit
    assert conflict.report_order == :verified_from_session_sequence
    assert conflict.occurrence_identity == :unresolved
    assert conflict.event_occurrence == :not_established
    assert conflict.actual_event_chronology == :not_established
    refute conflict.contradiction_verified?
    refute conflict.factual_authority?
    assert length(Ledger.entries(sid)) == 2
    assert Enum.all?(Ledger.entries(sid),
      &(&1.kind == :reported_temporal_relation and &1.factual_authority? == false))
  end

  test "an explicit question generates a safe native clarification, no LLM",
       %{sid: sid} do
    claim(sid, @before)
    claim(sid, @after)

    si = ask(sid)
    assert {:ok, meaning} = TemporalClaimReconciliation.verified(si)
    assert meaning.kind == :temporal_claim_conflict_question
    assert meaning.operation == :clarify_opposing_temporal_reports

    assert %{operation: :clarify_opposing_temporal_reports,
      realization_family: :temporal_claim_reconciliation,
      status: :eligible, realizer: :symbolic_generation,
      factual_authority?: false} = plan = SemanticStrategy.select(si)

    expected =
      "In report 1, you described the bird sitting on the fence before " <>
      "the cat sleeping under the tree. In report 2, you described " <>
      "the cat sleeping under the tree before the bird sitting on the fence. " <>
      "Those are opposing descriptions of the order, but they may refer " <>
      "to different occurrences. Which order did you intend?"

    assert {:ok, ^expected, :symbolic_generation, meta} =
      SemanticStrategy.realize(plan, @question, @features, @decision, si)
    assert meta.speech_act == :request_temporal_clarification
    assert meta.contradiction_verified? == false
    assert meta.event_occurrence_verified? == false
    assert meta.actual_event_order_verified? == false
    assert meta.factual_claims? == false
    assert meta.llm_called? == false
    assert {:ok, ^expected, _} =
      SymbolicGeneration.generate(@question, @features, @decision, si)

    # The detector and explanation are read-only.
    assert length(Ledger.entries(sid)) == 2
  end

  test "one report, matching order, different event properties and other sessions abstain",
       %{sid: sid, other: other} do
    assert TemporalClaimReconciliation.detect(sid) == :not_found
    claim(sid, @before)
    assert TemporalClaimReconciliation.detect(sid) == :not_found
    claim(sid, @before)
    assert TemporalClaimReconciliation.detect(sid) == :not_found
    claim(sid, @different)
    assert TemporalClaimReconciliation.detect(sid) == :not_found
    assert TemporalClaimReconciliation.detect(other) == :not_found
    assert TemporalClaimReconciliation.question(ask(other)) == nil
  end
  test "multiple opposing pairs remain ambiguous and cannot generate a clarification",
       %{sid: sid} do
    claim(sid, @before)
    claim(sid, @after)
    claim(sid, @before)

    assert TemporalClaimReconciliation.detect(sid) == :ambiguous
    assert TemporalClaimReconciliation.question(ask(sid)) == nil
    si = ask(sid)
    assert TemporalClaimReconciliation.verified(si) == :not_applicable
    refute match?(%{operation: :clarify_opposing_temporal_reports,
      status: :eligible}, SemanticStrategy.select(si))
    assert SymbolicGeneration.generate(@question, @features, @decision, si) ==
      :not_applicable
    assert length(Ledger.entries(sid)) == 3
  end

  test "an edited question, changed session or changed evidence fails revalidation",
       %{sid: sid, other: other} do
    claim(sid, @before)
    claim(sid, @after)
    good = ask(sid)
    assert {:ok, _} = TemporalClaimReconciliation.verified(good)

    assert TemporalClaimReconciliation.verified(%{good | session_id: other}) ==
      :not_applicable
    changed = %{good | sentence: "Did something else happen?"}
    assert TemporalClaimReconciliation.verified(changed) == :not_applicable
    forged = put_in(good, [Access.key!(:sentence_meaning),
      Access.key!(:conflict_evidence), Access.key!(:contradiction_verified?)], true)
    assert TemporalClaimReconciliation.verified(forged) == :not_applicable

    Ledger.clear(sid)
    assert TemporalClaimReconciliation.verified(good) == :not_applicable
    assert SemanticStrategy.select(good) == :not_applicable
    assert SymbolicGeneration.generate(@question, @features, @decision, good) ==
      :not_applicable
  end

  test "over-limit retained history refuses a selective unique interpretation",
       %{sid: sid} do
    claim(sid, @before)
    claim(sid, @after)
    for index <- 1..127 do
      Ledger.record(%Core.SemanticInput{
        session_id: sid, sentence: "Unrelated entry #{index}.", source: :test
      })
    end

    assert length(Ledger.entries(sid)) == 129
    assert TemporalClaimReconciliation.detect(sid) == :window_incomplete
    assert TemporalClaimReconciliation.question(ask(sid)) == nil
  end

  test "unverified or damaged ledger records are ignored",
       %{sid: sid} do
    claim(sid, @before)
    claim(sid, @after)
    [second | _] = Ledger.entries(sid)

    # Even an exact reversed descriptor cannot be accepted merely because
    # it resembles a previously witnessed event property.
    forged = %{second |
      proof_sha256: "not-a-proof",
      factual_authority?: true
    }
    assert {:ok, 3} = Core.Discourse.Ledger.Owner.append(sid, forged)
    assert {:ok, conflict} = TemporalClaimReconciliation.detect(sid)
    assert conflict.newer_report.seq == 2
    assert conflict.older_report.seq == 1
  end

end
