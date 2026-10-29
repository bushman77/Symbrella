defmodule Core.Semantic.TemporalMemoryRetrievalTest do
  use ExUnit.Case, async: false

  alias Core.Discourse.Ledger
  alias Core.Response.{SemanticStrategy, SymbolicGeneration}
  alias Core.Semantic.{SentenceMeaning, TemporalSentence,
    TemporalClaimReconciliation, TemporalClarificationResolution,
    TemporalMemoryRetrieval}

  @before_words ~w(The bird is sitting on the fence before the cat is sleeping under the tree)
  @after_words ~w(The bird is sitting on the fence after the cat is sleeping under the tree)
  @parts ~w(determiner noun verb verb preposition determiner noun conjunction determiner noun verb verb preposition determiner noun)
  @conflict_question "Did I describe conflicting temporal orders?"
  @recall_question "Which order did I finally mean?"
  @features %{risk_bucket: :low, intent: :ask}
  @decision %{action: :answer}

  defp session, do: "temporal-recall-#{System.unique_integer([:positive])}"

  defp claim(sid, words) do
    rows =
      Enum.zip(words, @parts)
      |> Enum.map(fn {word, pos} ->
        norm = String.downcase(word)
        %{id: "#{norm}|#{pos}|1", norm: norm, pos: pos,
          semantic_atoms: if(norm in ["sitting", "sleeping"],
            do: ["tag:form-of", "tag:present", "tag:participle"], else: [])}
      end)

    si =
      %Core.SemanticInput{
        session_id: sid, source: :test, intent: :statement,
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

    assert {:ok, _} = TemporalSentence.verified(si)
    Ledger.record(si)
  end

  defp say(sid, sentence) do
    %Core.SemanticInput{
      session_id: sid, source: :test, intent: :ask, sentence: sentence
    }
    |> SentenceMeaning.attach()
  end

  defp seed(sid, report_seq \\ 1) do
    claim(sid, @before_words)
    claim(sid, @after_words)

    request = say(sid, @conflict_question)
    assert {:ok, _} = TemporalClaimReconciliation.verified(request)
    Ledger.record(request)

    selection = say(sid, "I meant report #{report_seq}.")
    assert {:ok, _} = TemporalClarificationResolution.verified(selection)
    Ledger.record(selection)

    assert {:resolved, %{chosen_report_seq: ^report_seq}} =
      TemporalClaimReconciliation.detect(sid)
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

  test "recall after unrelated turns retains the original chosen order and native provenance",
       %{sid: sid} do
    seed(sid)

    for n <- 1..20 do
      Ledger.record(%Core.SemanticInput{
        session_id: sid, source: :test,
        sentence: "Unrelated turn #{n}."
      })
    end

    assert {:ok, recalled} = TemporalMemoryRetrieval.recall(sid)
    assert recalled.chosen_report_seq == 1
    assert recalled.resolution_seq == 4
    assert recalled.pending_request_seq == 3
    assert recalled.earlier == %{
      action: :sit, actor_kind: :bird, relation: :on, landmark: :fence
    }
    assert recalled.later == %{
      action: :sleep, actor_kind: :cat, relation: :under, landmark: :tree
    }
    assert recalled.status == :revalidated_from_ledger
    assert recalled.speaker_attributed?
    refute recalled.factual_authority?
    refute recalled.event_occurrence_verified?
    refute recalled.actual_event_chronology_verified?

    si = say(sid, @recall_question)
    assert {:ok, meaning} = TemporalMemoryRetrieval.verified(si)
    assert meaning.kind == :temporal_preference_recall_question
    assert meaning.operation == :recall_intended_temporal_order

    assert %{operation: :answer_intended_temporal_order,
      realization_family: :temporal_preference_recall,
      status: :eligible, realizer: :symbolic_generation} =
      plan = SemanticStrategy.select(si)

    expected =
      "You clarified that you meant report 1: the bird sitting on the fence " <>
      "before the cat sleeping under the tree. That is your stated intended " <>
      "order, not verified event chronology."

    assert {:ok, ^expected, :symbolic_generation, meta} =
      SemanticStrategy.realize(plan, @recall_question,
        @features, @decision, si)

    assert meta.speech_act == :answer_intended_temporal_order
    assert meta.strategy_operation == :answer_intended_temporal_order
    assert meta.chosen_report_seq == 1
    assert meta.resolution_seq == 4
    assert meta.speaker_preference_recalled?
    refute meta.event_identity_verified?
    refute meta.event_occurrence_verified?
    refute meta.actual_event_order_verified?
    refute meta.factual_claims?
    refute meta.llm_called?
    assert {:ok, ^expected, _} =
      SymbolicGeneration.generate(@recall_question, @features, @decision, si)

    # Read-only recall leaves every existing source record intact.
    assert length(Ledger.entries(sid)) == 24
    assert Enum.map(Enum.reverse(Ledger.entries(sid)) |> Enum.take(4), & &1.kind) ==
      [:reported_temporal_relation, :reported_temporal_relation,
       :temporal_clarification_request, :temporal_clarification_resolution]
  end

  test "selecting report 2 recalls reversed claimed chronology", %{sid: sid} do
    seed(sid, 2)

    question = "Which temporal order did I choose?"
    si = say(sid, question)
    assert {:ok, meaning} = TemporalMemoryRetrieval.verified(si)
    assert meaning.temporal_evidence.chosen_report_seq == 2
    assert meaning.temporal_evidence.earlier.action == :sleep
    assert meaning.temporal_evidence.later.action == :sit

    plan = SemanticStrategy.select(si)

    assert {:ok, reply, :symbolic_generation, meta} =
      SemanticStrategy.realize(plan, question, @features, @decision, si)

    assert reply ==
      "You clarified that you meant report 2: the cat sleeping under the tree " <>
      "before the bird sitting on the fence. That is your stated intended " <>
      "order, not verified event chronology."
    assert meta.chosen_report_seq == 2
    assert meta.factual_claims? == false
    assert meta.llm_called? == false
  end

  test "no resolution, unsupported question, and cross-session retrieval abstain",
       %{sid: sid, other: other} do
    claim(sid, @before_words)
    claim(sid, @after_words)

    assert TemporalMemoryRetrieval.recall(sid) == :not_found
    assert TemporalMemoryRetrieval.recall(other) == :not_found
    assert TemporalMemoryRetrieval.question(say(sid, @recall_question)) == nil

    seed_session = session()
    on_exit(fn -> Ledger.clear(seed_session) end)
    seed(seed_session)

    assert TemporalMemoryRetrieval.question(
      say(seed_session, "Which order did I finally mean")) == nil
    assert TemporalMemoryRetrieval.question(
      say(seed_session, "Which event really happened?")) == nil
    assert TemporalMemoryRetrieval.recall(other) == :not_found

    wrong_origin = %{say(seed_session, @recall_question) | source: :assistant}
    assert TemporalMemoryRetrieval.question(wrong_origin) == nil
  end

  test "saved interpretation must be revalidated after session, content or evidence changes",
       %{sid: sid, other: other} do
    seed(sid)
    good = say(sid, @recall_question)
    assert {:ok, _} = TemporalMemoryRetrieval.verified(good)

    altered = %{good | sentence: "Which temporal order did I choose?"}
    assert TemporalMemoryRetrieval.verified(altered) == :not_applicable
    crossed = %{good | session_id: other}
    assert TemporalMemoryRetrieval.verified(crossed) == :not_applicable

    forged = put_in(good,
      [Access.key!(:sentence_meaning), Access.key!(:temporal_evidence),
       Access.key!(:actual_event_chronology_verified?)], true)
    assert TemporalMemoryRetrieval.verified(forged) == :not_applicable

    Ledger.clear(sid)
    assert TemporalMemoryRetrieval.recall(sid) == :not_found
    assert TemporalMemoryRetrieval.verified(good) == :not_applicable
    assert SemanticStrategy.select(good) == :not_applicable
    assert SymbolicGeneration.generate(@recall_question, @features,
      @decision, good) == :not_applicable
  end

  test "new opposing reports or incomplete ledger window prevent a guessed answer",
       %{sid: sid} do
    seed(sid)
    claim(sid, @before_words)

    assert TemporalClaimReconciliation.detect(sid) == :ambiguous
    assert TemporalMemoryRetrieval.recall(sid) == :not_found

    Ledger.clear(sid)
    seed(sid)
    for n <- 1..125 do
      Ledger.record(%Core.SemanticInput{
        session_id: sid, source: :test,
        sentence: "Unrelated memory #{n}."
      })
    end

    assert length(Ledger.entries(sid)) == 129
    assert TemporalClaimReconciliation.detect(sid) == :window_incomplete
    assert TemporalMemoryRetrieval.recall(sid) == :not_found
    assert TemporalMemoryRetrieval.question(say(sid, @recall_question)) == nil
  end

  test "live cautious policy and non-social intent still realize verified recall",
       %{sid: sid} do
    seed(sid)

    si = say(sid, @recall_question)
    plan = SemanticStrategy.select(si)
    assert %{operation: :answer_intended_temporal_order,
             status: :eligible} = plan

    features = %{risk_bucket: :low, intent: :feedback}
    decision = %{action: :ask_first}

    assert {:ok, reply, :symbolic_generation, meta} =
      SemanticStrategy.realize(plan, @recall_question, features, decision, si)
    assert String.contains?(reply, "report 1")
    assert meta.llm_called? == false
    assert meta.factual_claims? == false

    assert SymbolicGeneration.generate(@recall_question,
      %{risk_bucket: :high, intent: :feedback}, decision, si) == :not_applicable
    assert SymbolicGeneration.generate(@recall_question, features,
      %{action: :safe_redirect}, si) == :not_applicable
  end

end
