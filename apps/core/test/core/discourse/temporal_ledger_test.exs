defmodule Core.Discourse.TemporalLedgerTest do
  use ExUnit.Case, async: false

  alias Core.Discourse.Ledger
  alias Core.Semantic.{SentenceMeaning, TemporalSentence, TemporalEventComposition}

  @before_words ~w(The bird is sitting on the fence before the cat is sleeping under the tree)
  @after_words ~w(The bird is sitting on the fence after the cat is sleeping under the tree)
  @parts ~w(determiner noun verb verb preposition determiner noun conjunction determiner noun verb verb preposition determiner noun)

  defp session, do: "temporal-ledger-#{System.unique_integer([:positive])}"

  defp claim(session, words) do
    rows = Enum.zip(words, @parts) |> Enum.map(fn {word, pos} ->
      norm = String.downcase(word)
      %{id: "#{norm}|#{pos}|1", norm: norm, pos: pos,
        semantic_atoms: if(norm in ["sitting", "sleeping"],
          do: ["tag:form-of", "tag:present", "tag:participle"], else: [])}
    end)

    %Core.SemanticInput{
      session_id: session, source: :test, intent: :statement,
      sentence: Enum.join(words, " ") <> ".",
      tokens: words |> Enum.with_index()
        |> Enum.map(fn {phrase, index} ->
          %{phrase: phrase, index: index, n: 1}
        end),
      active_cells: rows,
      lifg_choices: rows |> Enum.with_index()
        |> Enum.map(fn {row, index} ->
          %{token_index: index, id: row.id}
        end)
    }
    |> SentenceMeaning.attach()
  end

  setup do
    one = session()
    two = session()
    assert is_pid(Process.whereis(Core.Discourse.Ledger.Owner))
    on_exit(fn ->
      Ledger.clear(one)
      Ledger.clear(two)
    end)
    {:ok, sid: one, other: two}
  end

  test "before and after persist only a speaker-attributed temporal property", %{sid: sid} do
    first = claim(sid, @before_words)
    second = claim(sid, @after_words)
    assert {:ok, _} = TemporalSentence.verified(first)
    assert {:ok, _} = TemporalSentence.verified(second)

    Ledger.record(first)
    Ledger.record(second)
    assert [later_report, earlier_report] = Ledger.entries(sid)
    assert later_report.seq == 2
    assert earlier_report.seq == 1
    assert earlier_report.connective == :before
    assert later_report.connective == :after
    assert earlier_report.kind == :reported_temporal_relation
    assert earlier_report.earlier.action == :sit
    assert earlier_report.later.action == :sleep
    assert later_report.earlier.action == :sleep
    assert later_report.later.action == :sit
    assert earlier_report.earlier.actor_kind == :bird
    assert later_report.earlier.actor_kind == :cat
    assert earlier_report.relation == :before
    assert later_report.relation == :before
    assert earlier_report.speaker_attributed?
    refute earlier_report.factual_authority?
    refute earlier_report.event_occurrence_verified?
    refute earlier_report.actual_event_chronology_verified?
    refute later_report.referent_identity_verified?
    assert is_binary(later_report.proof_sha256)
    assert byte_size(later_report.proof_sha256) == 64
    refute Map.has_key?(later_report, :sentence)
    refute Map.has_key?(later_report, :tokens)
    refute Map.has_key?(later_report, :lambda_ast)
    assert :not_found = Ledger.resolve(sid, "that bird")
    assert :not_found =
      TemporalEventComposition.pair(sid,
        %{action: :sleep, relation: :under, landmark: :tree})
  end

  test "forged, stale and missing lexical evidence cannot enter temporal memory",
       %{sid: sid} do
    valid = claim(sid, @before_words)
    assert {:ok, _} = TemporalSentence.verified(valid)

    forged = put_in(valid, [Access.key!(:sentence_meaning),
      Access.key!(:temporal_claim), Access.key!(:external_chronology_verified?)],
      true)
    assert TemporalSentence.verified(forged) == :not_applicable
    Ledger.record(forged)

    stale = %{valid | sentence:
      "The bird is sitting on the fence after the cat is sleeping under the tree."}
    assert TemporalSentence.verified(stale) == :not_applicable
    Ledger.record(stale)

    no_conjunction = %{valid | active_cells:
      Enum.reject(valid.active_cells, &(&1.norm == "before"))}
    assert TemporalSentence.verified(no_conjunction) == :not_applicable
    Ledger.record(no_conjunction)

    assert Enum.map(Ledger.entries(sid), & &1.kind) ==
      [:uninterpreted, :uninterpreted, :uninterpreted]

    ignored = %{valid | source: :assistant}
    Ledger.record(ignored)
    assert length(Ledger.entries(sid)) == 3
  end

  test "memory is session scoped, remains bounded to a turn and survives other reports",
       %{sid: sid, other: other} do
    claim(sid, @after_words) |> Ledger.record()
    for n <- 1..20 do
      Ledger.record(%Core.SemanticInput{
        session_id: sid, source: :test,
        sentence: "Unrelated turn #{n}."
      })
    end
    assert length(Ledger.entries(sid)) == 21
    assert List.last(Ledger.entries(sid)).kind == :reported_temporal_relation
    assert List.last(Ledger.entries(sid)).seq == 1
    assert Ledger.entries(other) == []
    assert :ok = Ledger.clear(sid)
    assert Ledger.entries(sid) == []
  end
end
