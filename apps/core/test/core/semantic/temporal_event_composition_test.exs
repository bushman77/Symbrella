defmodule Core.Semantic.TemporalEventCompositionTest do
  use ExUnit.Case, async: false

  alias Core.Brain.WM.Comprehension, as: WMComprehension
  alias Core.Discourse.{Ledger, References}
  alias Core.Response.{SemanticStrategy, SymbolicGeneration}
  alias Core.Semantic.{Composition, DiscourseComposition, SentenceMeaning,
    TemporalEventComposition}
  alias Core.Syntax.{Signatures, StructuralCompetition}

  @question "What did I describe before it was sleeping under the tree?"
  @features %{risk_bucket: :low, intent: :ask}
  @decision %{action: :answer}

  defp session, do: "temporal-v1-#{System.unique_integer([:positive])}"

  defp input(sid, words, positions) do
    rows =
      Enum.zip(words, positions)
      |> Enum.map(fn {word, pos} ->
        norm = String.downcase(word)
        %{id: "#{norm}|#{pos}|1", norm: norm, pos: pos,
          semantic_atoms: if(norm in ["sitting", "sleeping"],
            do: ["tag:form-of", "tag:present", "tag:participle"], else: [])}
      end)

    %Core.SemanticInput{
      session_id: sid,
      sentence: Enum.join(words, " ") <> ".",
      source: :test,
      intent: :unknown,
      tokens: words |> Enum.with_index()
        |> Enum.map(fn {phrase, index} -> %{phrase: phrase, index: index, n: 1} end),
      active_cells: rows,
      lifg_choices: rows |> Enum.with_index()
        |> Enum.map(fn {row, index} -> %{id: row.id, token_index: index} end)
    }
  end

  defp nominal(sid) do
    input(sid, ~w(The small black bird),
      ~w(determiner adjective adjective noun))
    |> Signatures.attach()
    |> StructuralCompetition.attach()
    |> Composition.attach()
  end

  defp report(sid, context, verb, preposition, place) do
    words = ["It", "is", verb, preposition, "the", place]
    input(sid, words, ~w(pronoun verb verb preposition determiner noun))
    |> Map.put(:wm_context, context)
    |> References.attach()
    |> Signatures.attach()
    |> StructuralCompetition.attach()
    |> SentenceMeaning.attach()
  end

  defp seed(sid) do
    first = nominal(sid)
    assert {:ok, _} = Composition.verified(first)
    context = WMComprehension.context_from(first, %{
      recognized_topics: [], understood: [], uncertain: [], degraded?: false
    })

    sitting = report(sid, context, "sitting", "on", "fence")
    sleeping = report(sid, context, "sleeping", "under", "tree")
    assert {:ok, _} = DiscourseComposition.verified(sitting)
    assert {:ok, _} = DiscourseComposition.verified(sleeping)

    Ledger.record(first)
    Ledger.record(sitting)
    Ledger.record(sleeping)
    {first, sitting, sleeping}
  end

  defp question(sid) do
    %Core.SemanticInput{
      session_id: sid, source: :test, intent: :ask, sentence: @question
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

  test "two typed reports establish only the conversational report order", %{sid: sid} do
    seed(sid)
    assert [%{seq: 3, kind: :reported_spatial_description,
              typed_join_verified?: true, event_existence: :not_asserted},
            %{seq: 2, kind: :reported_spatial_description,
              typed_join_verified?: true},
            %{seq: 1, kind: :nominal_description}] = Ledger.entries(sid)

    assert {:ok, pair} =
      TemporalEventComposition.pair(sid, %{action: :sleep,
        relation: :under, landmark: :tree})

    assert pair.earlier.seq == 2
    assert pair.later.seq == 3
    assert pair.earlier.action == :sit
    assert pair.later.action == :sleep
    assert pair.order == :reported_before
    assert pair.same_actor_identity == :candidate_only
    assert pair.actual_event_order == :not_established
    assert pair.event_occurrence == :not_established
    assert pair.causal_relation == :not_established
    assert pair.factual_authority? == false
    refute Map.has_key?(pair.earlier, :sentence)
    assert is_binary(pair.earlier.typed_join_proof_sha256)
  end

  test "earlier-report question composes an answer without an LLM", %{sid: sid} do
    seed(sid)
    si = question(sid)

    assert {:ok, meaning} = TemporalEventComposition.verified(si)
    assert meaning.kind == :reported_event_order_question
    assert meaning.operation == :recall_earlier_report
    assert meaning.factual_authority? == false
    assert :actual_event_chronology in meaning.unresolved

    assert %{operation: :answer_earlier_report,
      status: :eligible, realizer: :symbolic_generation,
      missing: [:actual_event_chronology, :actor_identity]} =
      plan = SemanticStrategy.select(si)

    assert {:ok, reply, :symbolic_generation, meta} =
      SemanticStrategy.realize(plan, @question, @features, @decision, si)

    assert reply == "Earlier, you described the small black bird sitting on " <>
      "the fence. Later, you described it sleeping under the tree. " <>
      "That is the order of your reports, not verified event chronology."
    assert meta.temporal_report_order_verified? == true
    assert meta.actual_event_order_verified? == false
    assert meta.event_occurrence_verified? == false
    assert meta.referent_verified? == false
    assert meta.factual_claims? == false
    assert meta.llm_called? == false
  end

  test "intervening unrelated turns do not erase report provenance", %{sid: sid} do
    seed(sid)

    for index <- 1..20 do
      Ledger.record(%Core.SemanticInput{
        session_id: sid, source: :test, intent: :unknown,
        sentence: "Unrelated conversation entry #{index}."
      })
    end

    si = question(sid)
    assert {:ok, %{temporal_evidence: pair}} =
      TemporalEventComposition.verified(si)
    assert pair.earlier.seq == 2
    assert pair.later.seq == 3
    assert length(Ledger.entries(sid)) == 23

    assert {:ok, _reply, _meta} =
      SymbolicGeneration.generate(@question, @features, @decision, si)
  end

  test "question tampering, wrong session and unsupported phrasing fail closed",
       %{sid: sid, other: other} do
    seed(sid)
    good = question(sid)
    assert {:ok, _} = TemporalEventComposition.verified(good)

    assert TemporalEventComposition.question(%{good | session_id: other}) == nil
    assert TemporalEventComposition.verified(%{good | session_id: other}) ==
      :not_applicable
    assert TemporalEventComposition.question(%{good | session_id: nil}) == nil

    altered = %{good | sentence:
      "What did I describe before it was flying into the tree?"}
    assert TemporalEventComposition.verified(altered) == :not_applicable
    assert SemanticStrategy.select(altered) == :not_applicable
    assert SymbolicGeneration.generate(altered.sentence, @features, @decision,
      altered) == :not_applicable

    changed = put_in(good, [Access.key!(:sentence_meaning),
      Access.key!(:temporal_evidence), Access.key!(:actual_event_order)],
      :verified)
    assert TemporalEventComposition.verified(changed) == :not_applicable
    assert SemanticStrategy.realize(SemanticStrategy.select(good),
      @question, @features, @decision, changed) == :not_applicable
  end

  test "duplicate later descriptions or competing earlier reports remain ambiguous",
       %{sid: sid} do
    {first, sitting, sleeping} = seed(sid)
    Ledger.record(sleeping)

    assert :ambiguous =
      TemporalEventComposition.pair(sid, %{action: :sleep, relation: :under,
        landmark: :tree})
    assert TemporalEventComposition.question(question(sid)) == nil

    Ledger.clear(sid)
    Ledger.record(first)
    Ledger.record(sitting)
    # Another distinct earlier spatial report produces multiple candidates.
    context = WMComprehension.context_from(first, %{
      recognized_topics: [], understood: [], uncertain: [], degraded?: false
    })
    extra = report(sid, context, "sleeping", "on", "fence")
    assert {:ok, _} = DiscourseComposition.verified(extra)
    Ledger.record(extra)
    Ledger.record(sleeping)

    assert :ambiguous =
      TemporalEventComposition.pair(sid, %{action: :sleep, relation: :under,
        landmark: :tree})
  end

  test "absence of independent evidence cannot compose a timeline",
       %{sid: sid} do
    assert TemporalEventComposition.pair(sid, %{action: :sleep, relation: :under,
      landmark: :tree}) == :not_found
    assert TemporalEventComposition.question(question(sid)) == nil

    first = nominal(sid)
    Ledger.record(first)
    assert TemporalEventComposition.question(question(sid)) == nil

    # A single report provides no earlier report to answer from.
    context = WMComprehension.context_from(first, %{
      recognized_topics: [], understood: [], uncertain: [], degraded?: false
    })
    Ledger.record(report(sid, context, "sleeping", "under", "tree"))
    assert TemporalEventComposition.question(question(sid)) == nil
  end
end
