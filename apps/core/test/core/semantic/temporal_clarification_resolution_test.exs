defmodule Core.Semantic.TemporalClarificationResolutionTest do
  use ExUnit.Case, async: false

  alias Core.Discourse.Ledger
  alias Core.Response.{SemanticStrategy, SymbolicGeneration}
  alias Core.Semantic.{SentenceMeaning, TemporalSentence,
    TemporalClaimReconciliation, TemporalClarificationResolution}

  @before_words ~w(The bird is sitting on the fence before the cat is sleeping under the tree)
  @after_words ~w(The bird is sitting on the fence after the cat is sleeping under the tree)
  @parts ~w(determiner noun verb verb preposition determiner noun conjunction determiner noun verb verb preposition determiner noun)
  @question "Did I describe conflicting temporal orders?"
  @features %{risk_bucket: :low, intent: :statement}
  @decision %{action: :answer}

  defp session, do: "temporal-resolution-#{System.unique_integer([:positive])}"

  defp input(sid, words) do
    rows =
      Enum.zip(words, @parts)
      |> Enum.map(fn {word, pos} ->
        norm = String.downcase(word)
        %{id: "#{norm}|#{pos}|1", norm: norm, pos: pos,
          semantic_atoms: if(norm in ["sitting", "sleeping"],
            do: ["tag:form-of", "tag:present", "tag:participle"], else: [])}
      end)

    %Core.SemanticInput{
      session_id: sid,
      source: :test,
      intent: :statement,
      sentence: Enum.join(words, " ") <> ".",
      tokens: words |> Enum.with_index()
        |> Enum.map(fn {word, index} ->
          %{phrase: word, index: index, n: 1}
        end),
      active_cells: rows,
      lifg_choices: rows |> Enum.with_index()
        |> Enum.map(fn {row, index} ->
          %{id: row.id, token_index: index}
        end)
    }
    |> SentenceMeaning.attach()
  end

  defp say(sid, sentence) do
    %Core.SemanticInput{session_id: sid, source: :test,
      intent: :statement, sentence: sentence}
    |> SentenceMeaning.attach()
  end

  defp seed(sid) do
    first = input(sid, @before_words)
    second = input(sid, @after_words)
    assert {:ok, _} = TemporalSentence.verified(first)
    assert {:ok, _} = TemporalSentence.verified(second)
    Ledger.record(first)
    Ledger.record(second)
    assert {:ok, _} = TemporalClaimReconciliation.detect(sid)
  end

  defp request(sid) do
    question = say(sid, @question)
    assert {:ok, _} = TemporalClaimReconciliation.verified(question)
    Ledger.record(question)
    assert [%{kind: :temporal_clarification_request, seq: 3} | _] =
      Ledger.entries(sid)
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

  test "first report preference is interpreted, acknowledged and appended without altering reports",
       %{sid: sid} do
    seed(sid)
    request(sid)
    answer = "I meant report 1."
    si = say(sid, answer)
    assert {:ok, meaning} = TemporalClarificationResolution.verified(si)
    assert meaning.kind == :temporal_clarification_resolution
    assert meaning.chosen_report_seq == 1
    assert meaning.pending_request_seq == 3
    assert meaning.chosen_claim.earlier.action == :sit
    assert meaning.chosen_claim.later.action == :sleep
    refute meaning.actual_event_chronology_verified?

    assert %{operation: :acknowledge_intended_temporal_report,
      status: :eligible, realizer: :symbolic_generation} =
      plan = SemanticStrategy.select(si)

    expected = "You meant the order described in report 1. " <>
      "That clarifies your intended description, not verified event chronology. " <>
      "Both original reports remain part of the conversation."

    assert {:ok, ^expected, :symbolic_generation, meta} =
      SemanticStrategy.realize(plan, answer, @features, @decision, si)
    assert meta.llm_called? == false
    assert meta.factual_claims? == false
    assert meta.event_occurrence_verified? == false
    assert {:ok, ^expected, _meta} =
      SymbolicGeneration.generate(answer, @features, @decision, si)

    Ledger.record(si)
    assert [resolution, pending, second, first] = Ledger.entries(sid)
    assert resolution.seq == 4
    assert resolution.kind == :temporal_clarification_resolution
    assert resolution.chosen_report_seq == 1
    assert resolution.pending_request_seq == 3
    assert pending.kind == :temporal_clarification_request
    assert second.kind == :reported_temporal_relation
    assert first.kind == :reported_temporal_relation
    refute resolution.event_occurrence_verified?
    refute resolution.actual_event_chronology_verified?
    refute resolution.factual_authority?
    refute Map.has_key?(resolution, :sentence)
    assert {:resolved, state} = TemporalClaimReconciliation.detect(sid)
    assert state.chosen_report_seq == 1
    assert state.resolution_seq == 4
    assert state.factual_authority? == false
    assert TemporalClaimReconciliation.question(say(sid, @question)) == nil
  end

  test "second report can be selected, but a second answer cannot silently overwrite it",
       %{sid: sid} do
    seed(sid)
    request(sid)
    selection = say(sid, "I meant report 2.")
    assert {:ok, meaning} = TemporalClarificationResolution.verified(selection)
    assert meaning.chosen_report_seq == 2
    assert meaning.chosen_claim.earlier.action == :sleep

    Ledger.record(selection)
    assert {:resolved, %{chosen_report_seq: 2}} =
      TemporalClaimReconciliation.detect(sid)

    retry = say(sid, "I meant report 1.")
    assert TemporalClarificationResolution.verified(retry) == :not_applicable
    assert SemanticStrategy.select(retry) == :not_applicable
    Ledger.record(retry)
    assert [%{kind: :uninterpreted}, %{kind: :temporal_clarification_resolution} | _] =
      Ledger.entries(sid)
    assert {:resolved, %{chosen_report_seq: 2}} =
      TemporalClaimReconciliation.detect(sid)
  end

  test "no request, wrong session, unsupported report or intervening turn fail closed",
       %{sid: sid, other: other} do
    seed(sid)

    assert TemporalClarificationResolution.build(say(sid, "I meant report 1.")) ==
      nil
    request(sid)

    assert TemporalClarificationResolution.build(say(other, "I meant report 1.")) ==
      nil
    assert TemporalClarificationResolution.build(say(sid, "I meant report 999.")) ==
      nil
    assert TemporalClarificationResolution.build(say(sid, "The first one.")) ==
      nil

    Ledger.record(%Core.SemanticInput{session_id: sid, source: :test,
      sentence: "An unrelated turn."})
    assert TemporalClarificationResolution.build(say(sid, "I meant report 1.")) ==
      nil
    assert {:ok, _} = TemporalClaimReconciliation.detect(sid)
  end

  test "stale, edited and forged correction meanings never revalidate",
       %{sid: sid, other: other} do
    seed(sid)
    request(sid)
    answer = say(sid, "I meant report 1.")
    assert {:ok, _} = TemporalClarificationResolution.verified(answer)

    altered = %{answer | sentence: "I meant report 2."}
    assert TemporalClarificationResolution.verified(altered) == :not_applicable
    crossed = %{answer | session_id: other}
    assert TemporalClarificationResolution.verified(crossed) == :not_applicable
    forged = put_in(answer,
      [Access.key!(:sentence_meaning), Access.key!(:actual_event_chronology_verified?)],
      true)
    assert TemporalClarificationResolution.verified(forged) == :not_applicable
    Ledger.clear(sid)
    assert TemporalClarificationResolution.verified(answer) == :not_applicable
    assert SymbolicGeneration.generate("I meant report 1.", @features,
      @decision, answer) == :not_applicable
  end

  test "multiple conflict candidates cannot open a clarification request",
       %{sid: sid} do
    seed(sid)
    Ledger.record(input(sid, @before_words))
    assert TemporalClaimReconciliation.detect(sid) == :ambiguous
    question = say(sid, @question)
    assert TemporalClaimReconciliation.verified(question) == :not_applicable
    Ledger.record(question)
    assert TemporalClarificationResolution.build(say(sid, "I meant report 1.")) ==
      nil
  end

  test "live cautious policy and feedback intent still realize a verified clarification",
       %{sid: sid} do
    seed(sid)
    request(sid)

    answer = "I meant report 1."
    si = say(sid, answer)
    plan = SemanticStrategy.select(si)
    assert %{operation: :acknowledge_intended_temporal_report,
             status: :eligible} = plan

    features = %{risk_bucket: :low, intent: :feedback}
    decision = %{action: :ask_first}

    assert {:ok, reply, :symbolic_generation, meta} =
      SemanticStrategy.realize(plan, answer, features, decision, si)
    assert String.starts_with?(reply, "You meant the order described in report 1.")
    assert meta.llm_called? == false
    assert meta.factual_claims? == false

    assert SymbolicGeneration.generate(answer,
      %{risk_bucket: :high, intent: :feedback}, decision, si) == :not_applicable
    assert SymbolicGeneration.generate(answer, features,
      %{action: :safe_redirect}, si) == :not_applicable

    # No generic social output is unlocked by a cautious action.
    assert SymbolicGeneration.generate("Hello.", features, decision,
      %{sentence: "Hello.", source: :test}) == :not_applicable
  end

end
